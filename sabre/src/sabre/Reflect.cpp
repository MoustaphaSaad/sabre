#include "sabre/Reflect.h"
#include "sabre/Unit.h"

#include <mn/Log.h>

namespace sabre
{
	inline static void
	_generate_entry_shader_io(Entry_Point* entry)
	{
		auto decl = entry->symbol->func_sym.decl;
		auto entry_type = entry->symbol->type;
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
						mn::map_insert(entry->input_layout, field.name.str, field.type);
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
	reflect_package(Entry_Point* entry)
	{
		switch (entry->mode)
		{
		case COMPILATION_MODE_VERTEX:
		case COMPILATION_MODE_PIXEL:
		case COMPILATION_MODE_GEOMETRY:
			_generate_entry_shader_io(entry);
			break;
		case COMPILATION_MODE_LIBRARY:
			// library mode is not allowed here
		default:
			assert(false && "unreachable");
			break;
		}
	}
}