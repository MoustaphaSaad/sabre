#include "sabre/Reflect.h"
#include "sabre/Unit.h"

#include <mn/Log.h>

namespace sabre
{
	inline static void
	_generate_entry_shader_io(Unit_Package* unit, Symbol* entry)
	{
		auto compilation_unit = unit->parent_unit;

		auto decl = entry->func_sym.decl;
		auto entry_type = entry->type;
		size_t type_index = 0;

		for (size_t i = 0; i < decl->func_decl.args.count; ++i)
		{
			const auto& arg = decl->func_decl.args[i];

			for (const auto& name: arg.names)
			{
				auto arg_type = entry_type->as_func.sign.args.types[type_index++];
				switch (arg_type->kind)
				{
				case Type::KIND_STRUCT:
					for (auto field: arg_type->struct_type.fields)
					{
						mn::map_insert(compilation_unit->input_layout, field.name.str, field.type);
					}
					break;
				default:
					break;
				}
			}
		}
	}

	// API
	void
	reflect_package(Unit_Package* unit)
	{
		auto compilation_unit = unit->parent_unit;

		switch (compilation_unit->mode)
		{
		case COMPILATION_MODE_LIBRARY:
			// do nothing
			break;
		case COMPILATION_MODE_VERTEX:
		case COMPILATION_MODE_PIXEL:
			_generate_entry_shader_io(unit, compilation_unit->entry_symbol);
			break;
		default:
			assert(false && "unreachable");
			break;
		}
	}
}