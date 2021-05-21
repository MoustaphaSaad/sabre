#include "sabre/Parse.h"
#include "sabre/Unit.h"
#include "sabre/AST.h"

#include <mn/IO.h>

namespace sabre
{
	inline static Tkn
	_parser_last_token(const Parser& self)
	{
		return self.tokens[self.prev_it];
	}

	inline static Tkn
	_parser_look_ahead_k(const Parser& self, size_t k)
	{
		if (self.it + k < self.tokens.count)
			return self.tokens[self.it + k];
		Tkn tkn{};
		tkn.kind = Tkn::KIND_EOF;
		tkn.loc.file = self.unit;
		return tkn;
	}

	inline static Tkn
	_parser_look(const Parser& self)
	{
		return _parser_look_ahead_k(self, 0);
	}

	inline static Tkn
	_parser_look_kind(const Parser& self, Tkn::KIND kind)
	{
		auto tkn = _parser_look(self);
		if (tkn.kind == kind)
			return tkn;
		return Tkn{};
	}

	inline static Tkn
	_parser_eat(Parser& self)
	{
		if (self.it >= self.tokens.count)
		{
			Tkn tkn{};
			tkn.kind = Tkn::KIND_EOF;
			tkn.loc.file = self.unit;
			return tkn;
		}
		auto tkn = self.tokens[self.it];
		self.prev_it = self.it;
		++self.it;
		return tkn;
	}

	inline static Tkn
	_parser_eat_kind(Parser& self, Tkn::KIND kind)
	{
		auto tkn = _parser_look(self);
		if (tkn.kind == kind)
			return _parser_eat(self);
		return Tkn{};
	}

	inline static Tkn
	_parser_eat_must(Parser& self, Tkn::KIND kind)
	{
		if (self.it >= self.tokens.count)
		{
			Err err{};
			err.msg = mn::strf("expected '{}' but found 'EOF'", Tkn::NAMES[kind]);
			unit_err(self.unit, err);
			Tkn tkn{};
			tkn.kind = Tkn::KIND_EOF;
			tkn.loc.file = self.unit;
			return tkn;
		}

		auto tkn = _parser_eat(self);
		if (tkn.kind == kind)
			return tkn;

		Err err{};
		err.loc = tkn.loc;
		err.msg = mn::strf("expected '{}' but found '{}'", Tkn::NAMES[kind], tkn.str);
		unit_err(self.unit, err);
		return Tkn{};
	}

	inline static Type_Sign
	_parser_parse_type(Parser& self)
	{
		auto type = type_sign_new(self.unit->ast_arena);
		while (true)
		{
			auto tkn = _parser_look(self);
			if (tkn.kind == Tkn::KIND_ID)
			{
				Tkn package_name{};
				Tkn type_name{};

				package_name = _parser_eat(self);
				if (_parser_look_kind(self, Tkn::KIND_DOT))
				{
					_parser_eat(self);
					type_name = _parser_eat_must(self, Tkn::KIND_ID);
				}
				else
				{
					type_name = package_name;
					package_name = Tkn{};
				}
				// if we end up with a named type signature token this means that we finished the type signature parsing
				type_sign_push(type, type_sign_atom_named(type_name, package_name));
				break;
			}
			else
			{
				break;
			}
		}
		return type;
	}

	inline static bool
	_parser_should_stop_at_curly_with_optional_comma(Parser& self)
	{
		return (
			_parser_look_kind(self, Tkn::KIND_CLOSE_CURLY) ||
			(_parser_look_kind(self, Tkn::KIND_COMMA) && _parser_look_ahead_k(self, 1).kind == Tkn::KIND_CLOSE_CURLY) ||
			_parser_look_kind(self, Tkn::KIND_EOF)
		);
	}

	inline static Expr*
	_parser_parse_expr_atom(Parser& self)
	{
		auto tkn = _parser_look(self);
		Expr* expr = nullptr;
		if (tkn.kind == Tkn::KIND_LITERAL_INTEGER)
		{
			expr = expr_atom_new(self.unit->ast_arena, _parser_eat(self));
		}
		else if (tkn.kind == Tkn::KIND_LITERAL_FLOAT)
		{
			expr = expr_atom_new(self.unit->ast_arena, _parser_eat(self));
		}
		else if (tkn.kind == Tkn::KIND_ID)
		{
			// TODO(Moustapha): handle composite literals later
			expr = expr_atom_new(self.unit->ast_arena, _parser_eat(self));
		}
		else if (tkn.kind == Tkn::KIND_KEYWORD_TRUE)
		{
			expr = expr_atom_new(self.unit->ast_arena, _parser_eat(self));
		}
		else if (tkn.kind == Tkn::KIND_KEYWORD_FALSE)
		{
			expr = expr_atom_new(self.unit->ast_arena, _parser_eat(self));
		}
		else if (tkn.kind == Tkn::KIND_OPEN_PAREN)
		{
			_parser_eat(self); // for the (
			expr = parser_parse_expr(self);
			_parser_eat_must(self, Tkn::KIND_CLOSE_PAREN);
			if (expr != nullptr)
				expr->in_parens = true;
		}
		else if (tkn.kind == Tkn::KIND_COLON)
		{
			_parser_eat(self); // for the :
			auto type = _parser_parse_type(self);
			_parser_eat_must(self, Tkn::KIND_OPEN_CURLY);

			auto fields = mn::buf_with_allocator<Complit_Field>(self.unit->ast_arena);
			bool named = false;
			while (_parser_should_stop_at_curly_with_optional_comma(self) == false)
			{
				if (fields.count > 0)
					_parser_eat_must(self, Tkn::KIND_COMMA);

				auto selector = mn::buf_with_allocator<Expr*>(self.unit->ast_arena);
				while (_parser_eat_kind(self, Tkn::KIND_DOT))
				{
					named = true;
					auto id = _parser_eat_must(self, Tkn::KIND_ID);
					auto atom_expr = expr_atom_new(self.unit->ast_arena, id);
					atom_expr->loc = id.loc;
					mn::buf_push(selector, atom_expr);
				}

				if (selector.count > 0)
				{
					_parser_eat_must(self, Tkn::KIND_EQUAL);

					if (named == false && fields.count > 0)
					{
						Err err{};
						err.loc = selector[0]->loc;
						err.msg = mn::strf("mixing named compound literal fields with unnamed fields is forbidden");
						unit_err(self.unit, err);
					}
				}

				auto right = parser_parse_expr(self);
				mn::buf_push(fields, complit_field_member(selector, right));
			}
			// last comma is optional
			_parser_eat_kind(self, Tkn::KIND_COMMA);
			_parser_eat_must(self, Tkn::KIND_CLOSE_CURLY);

			expr = expr_complit_new(self.unit->ast_arena, type, fields);
		}

		if (expr != nullptr)
		{
			expr->loc.pos = tkn.loc.pos;
			expr->loc.rng = Rng{tkn.loc.rng.begin, _parser_last_token(self).loc.rng.end};
			expr->loc.file = self.unit;
		}
		else
		{
			Err err{};
			err.loc = tkn.loc;
			err.msg = mn::strf("expected an expression but found '{}'", Tkn::NAMES[tkn.kind]);
			unit_err(self.unit, err);
		}
		return expr;
	}

	inline static Expr*
	_parser_parse_expr_base(Parser& self)
	{
		auto tkn = _parser_look(self);
		auto expr = _parser_parse_expr_atom(self);

		while (true)
		{
			if (_parser_eat_kind(self, Tkn::KIND_OPEN_PAREN))
			{
				auto args = mn::buf_with_allocator<Expr*>(self.unit->ast_arena);
				if (_parser_eat_kind(self, Tkn::KIND_CLOSE_PAREN) == false)
				{
					while (true)
					{
						if (auto arg = parser_parse_expr(self))
							mn::buf_push(args, arg);

						if (_parser_eat_kind(self, Tkn::KIND_COMMA) == false)
							break;
					}
					_parser_eat_must(self, Tkn::KIND_CLOSE_PAREN);
				}
				expr = expr_call_new(self.unit->ast_arena, expr, args);
			}
			else if (_parser_eat_kind(self, Tkn::KIND_OPEN_BRACKET))
			{
				auto index = parser_parse_expr(self);
				_parser_eat_must(self, Tkn::KIND_CLOSE_BRACKET);
				expr = expr_indexed_new(self.unit->ast_arena, expr, index);
			}
			else if (_parser_eat_kind(self, Tkn::KIND_DOT))
			{
				auto rhs = _parser_parse_expr_atom(self);
				if (rhs != nullptr)
					expr = expr_dot_new(self.unit->ast_arena, expr, rhs);
			}
			else
			{
				break;
			}

			if (expr != nullptr)
			{
				expr->loc.pos = tkn.loc.pos;
				expr->loc.rng = Rng{tkn.loc.rng.begin, _parser_last_token(self).loc.rng.end};
				expr->loc.file = self.unit;
			}
		}

		if (expr != nullptr)
		{
			expr->loc.pos = tkn.loc.pos;
			expr->loc.rng = Rng{tkn.loc.rng.begin, _parser_last_token(self).loc.rng.end};
			expr->loc.file = self.unit;
		}
		return expr;
	}

	inline static Expr*
	_parser_parse_expr_unary(Parser& self)
	{
		auto tkn = _parser_look(self);
		Expr* expr = nullptr;

		if (tkn_is_unary(_parser_look(self).kind))
		{
			auto op = _parser_eat(self);
			expr = expr_unary_new(self.unit->ast_arena, op, _parser_parse_expr_unary(self));
		}
		else
		{
			expr = _parser_parse_expr_base(self);
		}

		if (expr != nullptr)
		{
			expr->loc.pos = tkn.loc.pos;
			expr->loc.rng = Rng{tkn.loc.rng.begin, _parser_last_token(self).loc.rng.end};
			expr->loc.file = self.unit;
		}
		return expr;
	}

	inline static Expr*
	_parser_parse_expr_cast(Parser& self)
	{
		auto tkn = _parser_look(self);
		auto expr = _parser_parse_expr_unary(self);
		if (_parser_eat_kind(self, Tkn::KIND_COLON))
			expr = expr_cast_new(self.unit->ast_arena, expr, _parser_parse_type(self));

		if (expr != nullptr)
		{
			expr->loc.pos = tkn.loc.pos;
			expr->loc.rng = Rng{tkn.loc.rng.begin, _parser_last_token(self).loc.rng.end};
			expr->loc.file = self.unit;
		}
		return expr;
	}

	inline static Expr*
	_parser_parse_expr_mul(Parser& self)
	{
		auto tkn = _parser_look(self);
		auto expr = _parser_parse_expr_cast(self);
		while (tkn_is_mul(_parser_look(self).kind))
		{
			auto op = _parser_eat(self);
			auto rhs = _parser_parse_expr_cast(self);
			if (rhs == nullptr)
			{
				Err err{};
				err.loc = op.loc;
				err.msg = mn::strf("missing right handside");
				unit_err(self.unit, err);
				break;
			}
			expr = expr_binary_new(self.unit->ast_arena, expr, op, rhs);
			expr->loc.pos = tkn.loc.pos;
			expr->loc.rng = Rng{tkn.loc.rng.begin, _parser_last_token(self).loc.rng.end};
			expr->loc.file = self.unit;
		}

		if (expr != nullptr)
		{
			expr->loc.pos = tkn.loc.pos;
			expr->loc.rng = Rng{tkn.loc.rng.begin, _parser_last_token(self).loc.rng.end};
			expr->loc.file = self.unit;
		}
		return expr;
	}

	inline static Expr*
	_parser_parse_expr_add(Parser& self)
	{
		auto tkn = _parser_look(self);
		auto expr = _parser_parse_expr_mul(self);
		while (tkn_is_add(_parser_look(self).kind))
		{
			auto op = _parser_eat(self);
			auto rhs = _parser_parse_expr_mul(self);
			if (rhs == nullptr)
			{
				Err err{};
				err.loc = op.loc;
				err.msg = mn::strf("missing right handside");
				unit_err(self.unit, err);
				break;
			}
			expr = expr_binary_new(self.unit->ast_arena, expr, op, rhs);
			expr->loc.pos = tkn.loc.pos;
			expr->loc.rng = Rng{tkn.loc.rng.begin, _parser_last_token(self).loc.rng.end};
			expr->loc.file = self.unit;
		}

		if (expr != nullptr)
		{
			expr->loc.pos = tkn.loc.pos;
			expr->loc.rng = Rng{tkn.loc.rng.begin, _parser_last_token(self).loc.rng.end};
			expr->loc.file = self.unit;
		}
		return expr;
	}

	inline static Expr*
	_parser_parse_expr_cmp(Parser& self)
	{
		auto tkn = _parser_look(self);
		auto expr = _parser_parse_expr_add(self);
		if (tkn_is_cmp(_parser_look(self).kind))
		{
			auto op = _parser_eat(self);
			auto rhs = _parser_parse_expr_add(self);
			if (rhs == nullptr)
			{
				Err err{};
				err.loc = tkn.loc;
				err.msg = mn::strf("missing right handside");
				unit_err(self.unit, err);
			}
			else
			{
				expr = expr_binary_new(self.unit->ast_arena, expr, op, rhs);
			}
		}

		if (expr != nullptr)
		{
			expr->loc.pos = tkn.loc.pos;
			expr->loc.rng = Rng{tkn.loc.rng.begin, _parser_last_token(self).loc.rng.end};
			expr->loc.file = self.unit;
		}
		return expr;
	}

	inline static Expr*
	_parser_parse_expr_and(Parser& self)
	{
		auto tkn = _parser_look(self);
		auto expr = _parser_parse_expr_cmp(self);
		while (true)
		{
			auto tkn = _parser_eat_kind(self, Tkn::KIND_LOGICAL_AND);
			if (tkn)
			{
				auto rhs = _parser_parse_expr_cmp(self);
				if (rhs == nullptr)
				{
					Err err{};
					err.loc = tkn.loc;
					err.msg = mn::strf("missing right handside");
					unit_err(self.unit, err);
					break;
				}
				expr = expr_binary_new(self.unit->ast_arena, expr, tkn, rhs);
				expr->loc.pos = tkn.loc.pos;
				expr->loc.rng = Rng{tkn.loc.rng.begin, _parser_last_token(self).loc.rng.end};
				expr->loc.file = self.unit;
			}
			else
			{
				break;
			}
		}

		if (expr != nullptr)
		{
			expr->loc.pos = tkn.loc.pos;
			expr->loc.rng = Rng{tkn.loc.rng.begin, _parser_last_token(self).loc.rng.end};
			expr->loc.file = self.unit;
		}
		return expr;
	}

	inline static Expr*
	_parser_parse_expr_or(Parser& self)
	{
		auto tkn = _parser_look(self);
		auto expr = _parser_parse_expr_and(self);
		while (true)
		{
			auto tkn = _parser_eat_kind(self, Tkn::KIND_LOGICAL_OR);
			if (tkn)
			{
				auto rhs = _parser_parse_expr_and(self);
				if (rhs == nullptr)
				{
					Err err{};
					err.loc = tkn.loc;
					err.msg = mn::strf("missing right handside");
					unit_err(self.unit, err);
					break;
				}
				expr = expr_binary_new(self.unit->ast_arena, expr, tkn, rhs);
			}
			else
			{
				break;
			}
		}

		if (expr != nullptr)
		{
			expr->loc.pos = tkn.loc.pos;
			expr->loc.rng = Rng{tkn.loc.rng.begin, _parser_last_token(self).loc.rng.end};
			expr->loc.file = self.unit;
		}
		return expr;
	}

	inline static Stmt*
	_parser_parse_stmt_block(Parser& self)
	{
		_parser_eat_must(self, Tkn::KIND_OPEN_CURLY);

		auto stmts = mn::buf_with_allocator<Stmt*>(self.unit->ast_arena);
		while (_parser_look_kind(self, Tkn::KIND_CLOSE_CURLY) == false)
		{
			if (auto s = parser_parse_stmt(self))
				mn::buf_push(stmts, s);
			else
				break;
		}
		_parser_eat_must(self, Tkn::KIND_CLOSE_CURLY);
		return stmt_block_new(self.unit->ast_arena, stmts);
	}

	inline static Stmt*
	_parser_parse_stmt_if(Parser& self)
	{
		_parser_eat_must(self, Tkn::KIND_KEYWORD_IF);

		auto cond = mn::buf_with_allocator<Expr*>(self.unit->ast_arena);
		auto body = mn::buf_with_allocator<Stmt*>(self.unit->ast_arena);
		Stmt* else_body = nullptr;

		mn::buf_push(cond, parser_parse_expr(self));
		mn::buf_push(body, _parser_parse_stmt_block(self));

		while (_parser_eat_kind(self, Tkn::KIND_KEYWORD_ELSE))
		{
			if (_parser_eat_kind(self, Tkn::KIND_KEYWORD_IF))
			{
				mn::buf_push(cond, parser_parse_expr(self));
				mn::buf_push(body, _parser_parse_stmt_block(self));
			}
			else
			{
				else_body = _parser_parse_stmt_block(self);
				break;
			}
		}

		return stmt_if_new(self.unit->ast_arena, cond, body, else_body);
	}

	inline static Stmt*
	_parser_parse_stmt_internal(Parser& self, bool accept_semicolon);

	inline static Stmt*
	_parser_parse_stmt_for(Parser& self)
	{
		auto for_keyword = _parser_eat_must(self, Tkn::KIND_KEYWORD_FOR);

		Stmt* init = nullptr;
		Expr* cond = nullptr;
		Stmt* post = nullptr;
		Stmt* body = nullptr;

		if (_parser_look_kind(self, Tkn::KIND_OPEN_CURLY))
		{
			// for {}
			body = _parser_parse_stmt_block(self);
		}
		else if (_parser_eat_kind(self, Tkn::KIND_SEMICOLON))
		{
			// for ;cond;post {}
			cond = parser_parse_expr(self);
			_parser_eat_must(self, Tkn::KIND_SEMICOLON);
			if (_parser_look_kind(self, Tkn::KIND_OPEN_CURLY) == false)
				post = _parser_parse_stmt_internal(self, false);
			body = _parser_parse_stmt_block(self);
		}
		else
		{
			// for cond {}
			// for init;cond;post {}
			init = _parser_parse_stmt_internal(self, false);
			if (_parser_eat_kind(self, Tkn::KIND_SEMICOLON))
			{
				if (_parser_eat_kind(self, Tkn::KIND_SEMICOLON) == false)
				{
					cond = parser_parse_expr(self);
					_parser_eat_must(self, Tkn::KIND_SEMICOLON);
				}
				if (_parser_look_kind(self, Tkn::KIND_OPEN_CURLY) == false)
				{
					post = _parser_parse_stmt_internal(self, false);
				}
				body = _parser_parse_stmt_block(self);
			}
			else if (_parser_look_kind(self, Tkn::KIND_OPEN_CURLY))
			{
				if (init != nullptr && init->kind == Stmt::KIND_EXPR)
				{
					cond = init->expr_stmt;
					init = nullptr;
					body = _parser_parse_stmt_block(self);
				}
				else
				{
					Err err{};
					err.loc = for_keyword.loc;
					err.msg = mn::strf("expected a condition expression in a for statement");
					unit_err(self.unit, err);
					return nullptr;
				}
			}
		}

		return stmt_for_new(self.unit->ast_arena, init, cond, post, body);
	}

	inline static Stmt*
	_parser_parse_stmt_simple(Parser& self)
	{
		auto lhs = mn::buf_with_allocator<Expr*>(self.unit->ast_arena);
		while (true)
		{
			if (auto e = parser_parse_expr(self))
				mn::buf_push(lhs, e);

			if (_parser_eat_kind(self, Tkn::KIND_COMMA) == false)
				break;
		}

		if (lhs.count == 0)
		{
			auto l = _parser_look(self);
			Err err{};
			err.loc = l.loc;
			err.msg = mn::strf("can't parse an assignment statement");
			unit_err(self.unit, err);
			return nullptr;
		}

		if (tkn_is_assign(_parser_look(self).kind))
		{
			auto op = _parser_eat(self);
			auto rhs = mn::buf_with_allocator<Expr*>(self.unit->ast_arena);
			while (true)
			{
				if (auto e = parser_parse_expr(self))
					mn::buf_push(rhs, e);

				if (_parser_eat_kind(self, Tkn::KIND_COMMA) == false)
					break;
			}
			return stmt_assign_new(self.unit->ast_arena, lhs, op, rhs);
		}

		// this is not an assignment statement so it must be an expression statement
		if (lhs.count > 1)
		{
			Err err{};
			err.loc = lhs[0]->loc;
			err.msg = mn::strf("can't have multiple expression in the same statement");
			unit_err(self.unit, err);
		}

		return stmt_expr_new(self.unit->ast_arena, lhs[0]);
	}

	inline static Decl*
	_parser_parse_decl_var(Parser& self, bool expect_semicolon);

	inline static Decl*
	_parser_parse_decl_const(Parser& self, bool expect_semicolon);

	inline static Decl*
	_parser_parse_decl_func(Parser& self);

	inline static Stmt*
	_parser_parse_stmt_internal(Parser& self, bool accept_semicolon)
	{
		auto tkn = _parser_look(self);
		Stmt* res = nullptr;
		bool expect_semicolon = true;

		if (tkn.kind == Tkn::KIND_KEYWORD_BREAK)
		{
			res = stmt_break_new(self.unit->ast_arena, _parser_eat(self));
		}
		else if (tkn.kind == Tkn::KIND_KEYWORD_CONTINUE)
		{
			res = stmt_continue_new(self.unit->ast_arena, _parser_eat(self));
		}
		else if (tkn.kind == Tkn::KIND_KEYWORD_RETURN)
		{
			_parser_eat(self); // for the return keyword
			auto expr = parser_parse_expr(self);
			if (expr == nullptr)
				return nullptr;
			res = stmt_return_new(self.unit->ast_arena, expr);
		}
		else if (tkn.kind == Tkn::KIND_OPEN_CURLY)
		{
			expect_semicolon = false;
			res = _parser_parse_stmt_block(self);
		}
		else if (tkn.kind == Tkn::KIND_KEYWORD_IF)
		{
			expect_semicolon = false;
			res = _parser_parse_stmt_if(self);
		}
		else if (tkn.kind == Tkn::KIND_KEYWORD_FOR)
		{
			expect_semicolon = false;
			res = _parser_parse_stmt_for(self);
		}
		else if (tkn.kind == Tkn::KIND_KEYWORD_VAR)
		{
			auto decl = _parser_parse_decl_var(self, false);
			if (decl == nullptr)
				return nullptr;
			res = stmt_decl_new(self.unit->ast_arena, decl);
		}
		else if (tkn.kind == Tkn::KIND_KEYWORD_CONST)
		{
			auto decl = _parser_parse_decl_const(self, false);
			if (decl == nullptr)
				return nullptr;
			res = stmt_decl_new(self.unit->ast_arena, decl);
		}
		else if (tkn.kind == Tkn::KIND_KEYWORD_FUNC)
		{
			auto decl = _parser_parse_decl_func(self);
			if (decl == nullptr)
				return nullptr;
			decl->loc.rng = Rng{tkn.loc.rng.begin, _parser_last_token(self).loc.rng.end};
			decl->loc.pos = tkn.loc.pos;
			decl->loc.file = self.unit;
			res = stmt_decl_new(self.unit->ast_arena, decl);
		}
		else
		{
			res = _parser_parse_stmt_simple(self);
		}

		// there should be a semicolon here if we don't find it we skip to it
		if (accept_semicolon && expect_semicolon)
		{
			if (auto tkn = _parser_look(self); tkn.kind == Tkn::KIND_SEMICOLON)
			{
				_parser_eat(self); // eat the semicolon
			}
			else
			{
				// we didn't find the semicolon issue an error and skip till we find one
				Err err{};
				err.loc = tkn.loc;
				err.msg = mn::strf("Expected a semicolon at the end of the statement");
				unit_err(self.unit, err);

				while (true)
				{
					auto tkn = _parser_eat(self);
					if (parser_eof(self) || tkn.kind == Tkn::KIND_SEMICOLON)
						break;
				}
			}
		}

		if (res != nullptr)
		{
			res->loc.rng = Rng{tkn.loc.rng.begin, _parser_last_token(self).loc.rng.end};
			res->loc.pos = tkn.loc.pos;
			res->loc.file = self.unit;
		}
		return res;
	}

	inline static Decl*
	_parser_parse_decl_var_base(Parser& self, bool expect_semicolon, Tkn::KIND keyword)
	{
		_parser_eat_must(self, keyword);

		auto names = mn::buf_with_allocator<Tkn>(self.unit->ast_arena);
		auto values = mn::buf_with_allocator<Expr*>(self.unit->ast_arena);
		auto type = type_sign_new(self.unit->ast_arena);

		while (true)
		{
			if (auto name = _parser_eat_must(self, Tkn::KIND_ID))
				mn::buf_push(names, name);
			else
				break;

			if (_parser_eat_kind(self, Tkn::KIND_COMMA) == false)
				break;
		}

		if (_parser_eat_kind(self, Tkn::KIND_COLON))
			type = _parser_parse_type(self);

		if (_parser_eat_kind(self, Tkn::KIND_EQUAL))
		{
			while (true)
			{
				if (auto value = parser_parse_expr(self))
					mn::buf_push(values, value);
				else
					break;

				if (_parser_eat_kind(self, Tkn::KIND_COMMA) == false)
					break;
			}
		}

		if (expect_semicolon)
			_parser_eat_must(self, Tkn::KIND_SEMICOLON);

		return decl_var_new(self.unit->ast_arena, names, values, type);
	}

	inline static Decl*
	_parser_parse_decl_var(Parser& self, bool expect_semicolon)
	{
		return _parser_parse_decl_var_base(self, expect_semicolon, Tkn::KIND_KEYWORD_VAR);
	}

	inline static Decl*
	_parser_parse_decl_const(Parser& self, bool expect_semicolon)
	{
		auto res = _parser_parse_decl_var_base(self, expect_semicolon, Tkn::KIND_KEYWORD_CONST);
		if (res != nullptr)
			res = decl_convert_var_to_const(res);
		return res;
	}

	inline static Tag_Table
	_parser_parse_tags(Parser& self);

	inline static Arg
	_parser_parse_arg(Parser& self)
	{
		auto arg = arg_new(self.unit->ast_arena);
		arg.tags = _parser_parse_tags(self);
		while (true)
		{
			if (auto name = _parser_eat_kind(self, Tkn::KIND_ID))
				mn::buf_push(arg.names, name);

			if (_parser_eat_kind(self, Tkn::KIND_COMMA) == false)
				break;
		}
		// arguments must have a type
		_parser_eat_must(self, Tkn::KIND_COLON);
		arg.type = _parser_parse_type(self);
		return arg;
	}

	inline static Decl*
	_parser_parse_decl_func(Parser& self)
	{
		_parser_eat_must(self, Tkn::KIND_KEYWORD_FUNC);
		auto name = _parser_eat_must(self, Tkn::KIND_ID);

		auto args = mn::buf_with_allocator<Arg>(self.unit->ast_arena);
		_parser_eat_must(self, Tkn::KIND_OPEN_PAREN);
		while (true)
		{
			if (_parser_look_kind(self, Tkn::KIND_CLOSE_PAREN) == false)
				mn::buf_push(args, _parser_parse_arg(self));

			if (_parser_eat_kind(self, Tkn::KIND_COMMA) == false)
				break;
		}
		_parser_eat_must(self, Tkn::KIND_CLOSE_PAREN);

		auto ret = type_sign_new(self.unit->ast_arena);
		if (_parser_eat_kind(self, Tkn::KIND_COLON))
			ret = _parser_parse_type(self);

		Stmt* body = nullptr;
		if (_parser_look_kind(self, Tkn::KIND_OPEN_CURLY))
			body = _parser_parse_stmt_block(self);
		return decl_func_new(self.unit->ast_arena, name, args, ret, body);
	}

	inline static Field
	_parser_parse_field(Parser& self)
	{
		auto field = field_new(self.unit->ast_arena);
		field.tags = _parser_parse_tags(self);
		while (true)
		{
			if (auto name = _parser_eat_kind(self, Tkn::KIND_ID))
				mn::buf_push(field.names, name);

			if (_parser_eat_kind(self, Tkn::KIND_COMMA) == false)
				break;
		}
		_parser_eat_must(self, Tkn::KIND_COLON);
		field.type = _parser_parse_type(self);
		return field;
	}

	inline static Decl*
	_parser_parse_decl_type(Parser& self)
	{
		_parser_eat_must(self, Tkn::KIND_KEYWORD_TYPE);
		auto name = _parser_eat_must(self, Tkn::KIND_ID);
		_parser_eat_must(self, Tkn::KIND_KEYWORD_STRUCT);

		_parser_eat_must(self, Tkn::KIND_OPEN_CURLY);
		auto fields = mn::buf_with_allocator<Field>(self.unit->ast_arena);

		while (_parser_should_stop_at_curly_with_optional_comma(self) == false)
		{
			if (fields.count > 0)
				_parser_eat_must(self, Tkn::KIND_COMMA);

			mn::buf_push(fields, _parser_parse_field(self));
		}
		// last comma is optional
		_parser_eat_kind(self, Tkn::KIND_COMMA);
		_parser_eat_must(self, Tkn::KIND_CLOSE_CURLY);

		return decl_struct_new(self.unit->ast_arena, name, fields);
	}

	inline static Decl*
	_parser_parse_decl_import(Parser& self)
	{
		_parser_eat_must(self, Tkn::KIND_KEYWORD_IMPORT);
		auto name = _parser_eat_must(self, Tkn::KIND_ID);
		auto path = _parser_eat_must(self, Tkn::KIND_LITERAL_STRING);

		// TODO(Moustapha): unescape the string
		auto package_path = mn::str_from_c(path.str, mn::memory::tmp());
		mn::str_trim(package_path, "\"");

		auto [package, resolve_err] = unit_file_resolve_package(self.unit, package_path, name);
		if (resolve_err)
		{
			Err err{};
			err.loc = path.loc;
			err.msg = mn::strf("import failed because {}", resolve_err);
			unit_err(self.unit, err);
		}

		return decl_import_new(self.unit->ast_arena, path, name);
	}

	inline static mn::Buf<Decl*>
	_parser_parse_decl_group(Parser& self)
	{
		auto res = mn::buf_with_allocator<Decl*>(self.unit->ast_arena);
		_parser_eat_must(self, Tkn::KIND_OPEN_CURLY);
		while (_parser_eat_kind(self, Tkn::KIND_CLOSE_CURLY) == false)
		{
			if (auto d = parser_parse_decl(self))
				mn::buf_push(res, d);
			else
				break;
		}
		return res;
	}

	inline static Decl*
	_parser_parse_decl_if(Parser& self)
	{
		_parser_eat_must(self, Tkn::KIND_KEYWORD_IF);

		auto cond = mn::buf_with_allocator<Expr*>(self.unit->ast_arena);
		auto body = mn::buf_with_allocator<mn::Buf<Decl*>>(self.unit->ast_arena);
		auto else_body = mn::buf_with_allocator<Decl*>(self.unit->ast_arena);

		auto if_cond = parser_parse_expr(self);
		auto if_body = _parser_parse_decl_group(self);
		mn::buf_push(cond, if_cond);
		mn::buf_push(body, if_body);

		while (_parser_eat_kind(self, Tkn::KIND_KEYWORD_ELSE))
		{
			if (_parser_eat_kind(self, Tkn::KIND_KEYWORD_IF))
			{
				auto if_cond = parser_parse_expr(self);
				auto if_body = _parser_parse_decl_group(self);
				mn::buf_push(cond, if_cond);
				mn::buf_push(body, if_body);
			}
			else
			{
				else_body = _parser_parse_decl_group(self);
				break;
			}
		}

		return decl_if_new(self.unit->ast_arena, cond, body, else_body);
	}

	inline static Tag_Table
	_parser_parse_tags(Parser& self)
	{
		auto res = tag_table_new(self.unit->ast_arena);
		while (true)
		{
			if (_parser_eat_kind(self, Tkn::KIND_AT) == false)
				break;

			auto tag = tag_new(self.unit->ast_arena);
			tag.name = _parser_eat_must(self, Tkn::KIND_ID);
			if (_parser_eat_kind(self, Tkn::KIND_OPEN_CURLY))
			{
				while (_parser_look_kind(self, Tkn::KIND_CLOSE_CURLY) == false)
				{
					auto key = _parser_eat_must(self, Tkn::KIND_ID);
					_parser_eat_must(self, Tkn::KIND_EQUAL);
					auto value = _parser_eat(self);
					if (value.kind != Tkn::KIND_LITERAL_INTEGER &&
						value.kind != Tkn::KIND_LITERAL_STRING)
					{
						Err err{};
						err.loc = value.loc;
						err.msg = mn::strf("invalid tag value, allowed values are integers and strings");
						unit_err(self.unit, err);
					}

					if (key && value)
					{
						if (auto it = mn::map_lookup(tag.args, key))
						{
							Err err{};
							err.loc = key.loc;
							err.msg = mn::strf(
								"duplicated tag key, first defined in {}:{}:{}",
								it->value.loc.file->filepath,
								it->value.loc.pos.line,
								it->value.loc.pos.col
							);
							unit_err(self.unit, err);
						}
						else
						{
							mn::map_insert(tag.args, key, value);
						}

						_parser_eat_kind(self, Tkn::KIND_COMMA);
					}
					else
					{
						return res;
					}
				}
				_parser_eat_must(self, Tkn::KIND_CLOSE_CURLY);
			}
			mn::map_insert(res.table, tag.name, tag);
		}
		return res;
	}

	// API
	Parser
	parser_new(Unit_File* unit)
	{
		Parser self{};
		self.unit = unit;
		self.tokens = mn::buf_memcpy_clone(unit->tkns);
		mn::buf_remove_if(self.tokens, [](const auto& tkn) {
			return tkn_can_ignore(tkn.kind);
		});
		return self;
	}

	void
	parser_free(Parser& self)
	{
		mn::buf_free(self.tokens);
	}

	Expr*
	parser_parse_expr(Parser& self)
	{
		return _parser_parse_expr_or(self);
	}

	Stmt*
	parser_parse_stmt(Parser& self)
	{
		return _parser_parse_stmt_internal(self, true);
	}

	Decl*
	parser_parse_decl(Parser& self)
	{
		auto tags = _parser_parse_tags(self);
		auto tkn = _parser_look(self);
		Decl* res = nullptr;

		if (tkn.kind == Tkn::KIND_KEYWORD_VAR)
			res = _parser_parse_decl_var(self, true);
		else if (tkn.kind == Tkn::KIND_KEYWORD_CONST)
			res = _parser_parse_decl_const(self, true);
		else if (tkn.kind == Tkn::KIND_KEYWORD_FUNC)
			res = _parser_parse_decl_func(self);
		else if (tkn.kind == Tkn::KIND_KEYWORD_TYPE)
			res = _parser_parse_decl_type(self);
		else if (tkn.kind == Tkn::KIND_KEYWORD_IMPORT)
			res = _parser_parse_decl_import(self);
		else if (tkn.kind == Tkn::KIND_KEYWORD_IF)
			res = _parser_parse_decl_if(self);

		if (res != nullptr)
		{
			res->loc.pos = tkn.loc.pos;
			res->loc.rng = Rng{tkn.loc.rng.begin, _parser_last_token(self).loc.rng.end};
			res->loc.file = self.unit;
			res->tags = tags;
		}
		return res;
	}
}