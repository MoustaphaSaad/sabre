#include "sabre/SPIRV.h"
#include "sabre/IR.h"
#include "sabre/Unit.h"
#include "sabre/Scope.h"
#include "sabre/Type_Interner.h"

namespace sabre
{
	inline static spirv::Type*
	_spirv_type_gen(SPIRV& self, Type* type)
	{
		if (auto it = mn::map_lookup(self.spirv_type_table, type))
			return it->value;

		spirv::Type* res = nullptr;

		switch (type->kind)
		{
		case Type::KIND_VOID:
			res = spirv::module_type_void_new(self.out);
			break;
		case Type::KIND_INT:
			res = spirv::module_type_int_new(self.out, 32, true);
			break;
		case Type::KIND_FUNC:
		{
			auto return_type = _spirv_type_gen(self, type->func.return_type);
			res = spirv::module_type_func_new(self.out, return_type);
			for (auto arg_type: type->func.args.types)
				spirv::module_type_func_arg(res, _spirv_type_gen(self, arg_type));
		}
		default:
			assert(false && "unreachable");
			break;
		}

		if (res)
			mn::map_insert(self.spirv_type_table, type, res);

		return res;
	}

	inline static void
	_spirv_func_gen(SPIRV& self, Symbol* sym)
	{
		auto func = spirv::module_func_new(self.out, _spirv_type_gen(self, sym->type));
	}

	inline static void
	_spirv_symbol_gen(SPIRV& self, Symbol* sym)
	{
		switch(sym->kind)
		{
		case Symbol::KIND_FUNC:
			_spirv_func_gen(self, sym);
			break;
		default:
			assert(false && "unreachable");
			break;
		}
	}

	// API
	SPIRV
	spirv_new(Unit_Package* unit)
	{
		SPIRV self{};
		self.unit = unit;
		self.out = spirv::module_new();
		return self;
	}

	void
	spirv_free(SPIRV& self)
	{
		spirv::module_free(self.out);
		mn::map_free(self.spirv_type_table);
	}

	void
	spirv_gen(SPIRV& self)
	{
		// loop over all reachable symbols and generate them
		for (auto sym: self.unit->reachable_symbols)
		{
			_spirv_symbol_gen(self, sym);
		}
	}
}