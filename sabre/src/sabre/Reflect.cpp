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
				auto arg_type = entry_type->func.args.types[type_index++];
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

	inline static void
	_process_package(Unit_Package* unit, size_t& uniform_binding_generator)
	{
		auto compilation_unit = unit->parent_unit;
		// generate reflection info
		for (auto sym: unit->reachable_symbols)
		{
			if (sym->kind == Symbol::KIND_PACKAGE)
			{
				_process_package(sym->package_sym.package, uniform_binding_generator);
				continue;
			}

			auto decl = symbol_decl(sym);
			if (decl == nullptr)
				continue;

			if (auto it = mn::map_lookup(decl->tags.table, KEYWORD_UNIFORM))
			{
				int binding = -1;
				if (auto arg_it = mn::map_lookup(it->value.args, KEYWORD_BINDING))
				{
					auto value_tkn = arg_it->value.value;
					if (value_tkn.kind == Tkn::KIND_LITERAL_INTEGER)
					{
						binding = ::atoi(value_tkn.str);
						if (binding > uniform_binding_generator)
							uniform_binding_generator = binding + 1;
					}
				}
				if (binding < 0)
					binding = uniform_binding_generator++;

				Reachable_Uniform uniform{};
				uniform.binding = binding;
				uniform.symbol = sym;
				mn::buf_push(compilation_unit->reachable_uniforms, uniform);
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

		size_t uniform_binding_generator = 0;
		_process_package(unit, uniform_binding_generator);
	}
}