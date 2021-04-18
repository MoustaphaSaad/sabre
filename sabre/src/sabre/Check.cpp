#include "sabre/Check.h"
#include "sabre/Unit.h"

#include <mn/IO.h>

namespace sabre
{
	inline static Scope*
	_typer_current_scope(Typer& self)
	{
		return mn::buf_top(self.scope_stack);
	}

	inline static Symbol*
	_typer_add_symbol(Typer& self, Symbol* sym)
	{
		auto current_scope = _typer_current_scope(self);
		if (auto old_sym = scope_shallow_find(current_scope, sym->name))
		{
			auto old_loc = symbol_location(old_sym);
			Err err{};
			err.loc = symbol_location(sym);
			err.loc = old_loc;
			if (old_loc.pos.line > 0)
				err.msg = mn::strf("'{}' symbol redefinition, first declared in {}:{}", sym->name, old_loc.pos.line, old_loc.pos.col);
			else
				err.msg = mn::strf("'{}' symbol redefinition", sym->name);
			unit_err(self.unit, err);
			return old_sym;
		}
		scope_add(current_scope, sym);
		return sym;
	}

	inline static void
	_typer_shallow_process_decl(Typer& self, Decl* decl)
	{
		switch (decl->kind)
		{
		case Decl::KIND_CONST:
			for (size_t i = 0; i < decl->const_decl.names.count; ++i)
			{
				// TODO(Moustapha): this assumes that we don't have multiple return values
				auto name = decl->const_decl.names[i];
				auto sign = decl->const_decl.type;
				Expr* value = nullptr;
				if (i < decl->const_decl.values.count)
					value = decl->const_decl.values[i];
				_typer_add_symbol(self, symbol_const_new(self.unit->symbols_arena, name, decl, sign, value));
			}
			break;
		case Decl::KIND_VAR:
			for (size_t i = 0; i < decl->var_decl.names.count; ++i)
			{
				// TODO(Moustapha): this assumes that we don't have multiple return values
				auto name = decl->var_decl.names[i];
				auto sign = decl->var_decl.type;
				Expr* value = nullptr;
				if (i < decl->var_decl.values.count)
					value = decl->var_decl.values[i];
				_typer_add_symbol(self, symbol_const_new(self.unit->symbols_arena, name, decl, sign, value));
			}
			break;
		case Decl::KIND_FUNC:
			_typer_add_symbol(self, symbol_func_new(self.unit->symbols_arena, decl->name, decl));
			break;
		default:
			assert(false && "unreachable");
			break;
		}
	}

	inline static void
	_typer_shallow_walk(Typer& self)
	{
		for (auto decl: self.unit->decls)
			_typer_shallow_process_decl(self, decl);
	}

	// API
	Typer
	typer_new(Unit* unit)
	{
		Typer self{};
		self.unit = unit;
		self.global_scope = unit->global_scope;

		mn::buf_push(self.scope_stack, self.global_scope);
		return self;
	}

	void
	typer_free(Typer& self)
	{
		mn::buf_free(self.scope_stack);
	}

	void
	typer_check(Typer& self)
	{
		_typer_shallow_walk(self);
		return;
	}
}