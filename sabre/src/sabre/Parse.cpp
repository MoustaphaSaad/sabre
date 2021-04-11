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
		return Tkn{};
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
			return Tkn{};
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
			return Tkn{};
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
		auto type = type_sign_new();
		while (true)
		{
			auto tkn = _parser_look(self);
			if (tkn.kind == Tkn::KIND_ID)
			{
				// if we end up with a named type signature token this means that we finished the type signature parsing
				type_sign_push(type, type_sign_atom_named_new(_parser_eat(self)));
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
			expr = expr_atom_new(_parser_eat(self));
		}
		else if (tkn.kind == Tkn::KIND_FLOAT)
		{
			expr = expr_atom_new(_parser_eat(self));
		}
		else if (tkn.kind == Tkn::KIND_ID)
		{
			// TODO(Moustapha): handle composite literals later
			expr = expr_atom_new(_parser_eat(self));
		}
		else if (tkn.kind == Tkn::KIND_KEYWORD_TRUE)
		{
			expr = expr_atom_new(_parser_eat(self));
		}
		else if (tkn.kind == Tkn::KIND_KEYWORD_FALSE)
		{
			expr = expr_atom_new(_parser_eat(self));
		}
		else if (tkn.kind == Tkn::KIND_OPEN_PAREN)
		{
			_parser_eat(self); // for the (
			expr = parser_parse_expr(self);
			_parser_eat_must(self, Tkn::KIND_CLOSE_PAREN);
			if (expr != nullptr)
				expr->in_parens = true;
		}

		if (expr == nullptr)
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
				auto args = mn::buf_new<Expr*>();
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
				expr = expr_call_new(expr, args);
			}
			else if (_parser_eat_kind(self, Tkn::KIND_OPEN_BRACKET))
			{
				auto index = parser_parse_expr(self);
				_parser_eat_must(self, Tkn::KIND_CLOSE_BRACKET);
				expr = expr_indexed_new(expr, index);
			}
			else if (_parser_eat_kind(self, Tkn::KIND_DOT))
			{
				auto rhs = parser_parse_expr(self);
				if (rhs != nullptr)
					expr = expr_dot_new(expr, rhs);
			}
			else
			{
				break;
			}
		}

		if (expr == nullptr)
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
			expr = expr_unary_new(op, _parser_parse_expr_unary(self));
		}
		else
		{
			expr = _parser_parse_expr_base(self);
		}

		if (expr == nullptr)
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
			expr = expr_cast_new(expr, _parser_parse_type(self));

		if (expr == nullptr)
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
			expr = expr_binary_new(expr, op, rhs);
		}

		if (expr == nullptr)
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
			expr = expr_binary_new(expr, op, rhs);
		}

		if (expr == nullptr)
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
				expr = expr_binary_new(expr, op, rhs);
			}
		}

		if (expr == nullptr)
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
				expr = expr_binary_new(expr, tkn, rhs);
			}
			else
			{
				break;
			}
		}

		if (expr == nullptr)
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
				expr = expr_binary_new(expr, tkn, rhs);
			}
			else
			{
				break;
			}
		}

		if (expr == nullptr)
		{
			expr->pos = tkn.pos;
			expr->rng = Rng{tkn.rng.begin, _parser_last_token(self).rng.end};
		}
		return expr;
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
}