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
			return tkn;
		}

		auto tkn = _parser_eat(self);
		if (tkn.kind == kind)
			return tkn;

		Err err{};
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
				// if we end up with a named type signature token this means that we finished the type signature parsing
				type_sign_push(type, type_sign_atom_named(_parser_eat(self)));
				break;
			}
			else
			{
				break;
			}
		}
		return type;
	}

	inline static Expr*
	_parser_parse_expr_atom(Parser& self)
	{
		auto tkn = _parser_look(self);
		Expr* expr = nullptr;
		if (tkn.kind == Tkn::KIND_INTEGER)
		{
			expr = expr_atom_new(self.unit->ast_arena, _parser_eat(self));
		}
		else if (tkn.kind == Tkn::KIND_FLOAT)
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

		if (expr != nullptr)
		{
			expr->pos = tkn.pos;
			expr->rng = Rng{tkn.rng.begin, _parser_last_token(self).rng.end};
		}
		else
		{
			Err err{};
			err.pos = tkn.pos;
			err.rng = tkn.rng;
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
				auto rhs = parser_parse_expr(self);
				if (rhs != nullptr)
					expr = expr_dot_new(self.unit->ast_arena, expr, rhs);
			}
			else
			{
				break;
			}
		}

		if (expr != nullptr)
		{
			expr->pos = tkn.pos;
			expr->rng = Rng{tkn.rng.begin, _parser_last_token(self).rng.end};
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
			expr->pos = tkn.pos;
			expr->rng = Rng{tkn.rng.begin, _parser_last_token(self).rng.end};
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
			expr->pos = tkn.pos;
			expr->rng = Rng{tkn.rng.begin, _parser_last_token(self).rng.end};
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
				err.pos = op.pos;
				err.rng = op.rng;
				err.msg = mn::strf("missing right handside");
				unit_err(self.unit, err);
				break;
			}
			expr = expr_binary_new(self.unit->ast_arena, expr, op, rhs);
		}

		if (expr != nullptr)
		{
			expr->pos = tkn.pos;
			expr->rng = Rng{tkn.rng.begin, _parser_last_token(self).rng.end};
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
				err.pos = op.pos;
				err.rng = op.rng;
				err.msg = mn::strf("missing right handside");
				unit_err(self.unit, err);
				break;
			}
			expr = expr_binary_new(self.unit->ast_arena, expr, op, rhs);
		}

		if (expr != nullptr)
		{
			expr->pos = tkn.pos;
			expr->rng = Rng{tkn.rng.begin, _parser_last_token(self).rng.end};
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
				err.pos = tkn.pos;
				err.rng = tkn.rng;
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
			expr->pos = tkn.pos;
			expr->rng = Rng{tkn.rng.begin, _parser_last_token(self).rng.end};
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
					err.pos = tkn.pos;
					err.rng = tkn.rng;
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
			expr->pos = tkn.pos;
			expr->rng = Rng{tkn.rng.begin, _parser_last_token(self).rng.end};
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
					err.pos = tkn.pos;
					err.rng = tkn.rng;
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
			expr->pos = tkn.pos;
			expr->rng = Rng{tkn.rng.begin, _parser_last_token(self).rng.end};
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
					err.pos = for_keyword.pos;
					err.rng = for_keyword.rng;
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
			err.pos = l.pos;
			err.rng = l.rng;
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
			err.pos = lhs[0]->pos;
			err.rng = lhs[0]->rng;
			err.msg = mn::strf("can't have multiple expression in the same statement");
			unit_err(self.unit, err);
		}

		return stmt_expr_new(self.unit->ast_arena, lhs[0]);
	}

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
			res = stmt_decl_new(decl);
		}
		else if (tkn.kind == Tkn::KIND_KEYWORD_CONST)
		{
			auto decl = _parser_parse_decl_const(self, false);
			if (decl == nullptr)
				return nullptr;
			res = stmt_decl_new(decl);
		}
		else if (tkn.kind == Tkn::KIND_KEYWORD_FUNC)
		{
			auto decl = _parser_parse_decl_func(self);
			if (decl == nullptr)
				return nullptr;
			decl->rng = Rng{tkn.rng.begin, _parser_last_token(self).rng.end};
			decl->pos = tkn.pos;
			res = stmt_decl_new(decl);
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
				err.pos = tkn.pos;
				err.rng = tkn.rng;
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
			res->rng = Rng{tkn.rng.begin, _parser_last_token(self).rng.end};
			res->pos = tkn.pos;
		}
		return res;
	}

	// API
	Parser
	parser_new(Unit* unit)
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
		return nullptr;
	}
}