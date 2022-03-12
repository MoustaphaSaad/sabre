#include "sabre/IR_Text.h"
#include "sabre/IR.h"

#include <mn/IO.h>
#include <mn/Assert.h>

namespace sabre::spirv
{
	inline static void
	_ir_text_newline(IR_Text& self)
	{
		mn::print_to(self.out, "\n");
		for (size_t i = 0; i < self.indent; ++i)
			mn::print_to(self.out, "\t");
	}

	inline static mn::Str
	_ir_text_type_gen(IR_Text& self, Type* type)
	{
		if (auto it = mn::set_lookup(self.generated_entities, type->id))
			return mn::str_tmpf("%{}", type->id);

		auto res = mn::str_tmp();
		switch (type->kind)
		{
		case Type::KIND_VOID:
			res = mn::strf(res, "%{} = OpTypeVoid", type->id);
			break;
		case Type::KIND_INT:
			res = mn::strf(res, "%{} = OpTypeInt {} {}", type->id, type->as_int.bit_width, type->as_int.is_signed);
			break;
		case Type::KIND_FUNC:
			res = mn::strf(res, "%{} = OpTypeFunction {}", type->id, _ir_text_type_gen(self, type->as_func.return_type));

			for (size_t i = 0; i < type->as_func.args.count; ++i)
			{
				res = mn::strf(res, " {}", _ir_text_type_gen(self, type->as_func.args[i]));
			}
			break;
		default:
			break;
		}

		_ir_text_newline(self);
		mn::print_to(self.out, "{}", res);
		mn::set_insert(self.generated_entities, type->id);
		return mn::str_tmpf("%{}", type->id);
	}

	inline static void
	_ir_text_bb_gen(IR_Text& self, Basic_Block* bb)
	{
		_ir_text_newline(self);
		mn::print_to(self.out, "%{} = OpLabel", bb->id);

		for (auto instruction: bb->instructions)
		{
			_ir_text_newline(self);
			switch (instruction.kind)
			{
			case Instruction::Op_IAdd:
				mn::print_to(
					self.out,
					"%{} = OpIAdd {} %{} %{}",
					instruction.as_iadd.res->id,
					_ir_text_type_gen(self, instruction.as_iadd.res->type),
					instruction.as_iadd.op1->id,
					instruction.as_iadd.op2->id
				);
				break;
			case Instruction::Op_ISub:
				mn::print_to(
					self.out,
					"%{} = OpISub {} %{} %{}",
					instruction.as_isub.res->id,
					_ir_text_type_gen(self, instruction.as_isub.res->type),
					instruction.as_isub.op1->id,
					instruction.as_isub.op2->id
				);
				break;
			case Instruction::Op_ReturnValue:
				mn::print_to(
					self.out,
					"OpReturnValue %{}",
					instruction.as_return.value->id
				);
				break;
			}
		}
	}

	inline static void
	_ir_text_func_gen(IR_Text& self, Func* func)
	{
		_ir_text_newline(self);
		mn::print_to(
			self.out,
			"%{} = OpFunction {} None {}",
			func->id,
			_ir_text_type_gen(self, func->type->as_func.return_type),
			_ir_text_type_gen(self, func->type)
		);
		++self.indent;

		for (auto arg: func->args)
		{
			_ir_text_newline(self);
			mn::print_to(
				self.out,
				"%{} = OpFunctionParameter {}",
				arg->id,
				_ir_text_type_gen(self, arg->type)
			);
		}

		for (auto bb: func->blocks)
			_ir_text_bb_gen(self, bb);

		--self.indent;
		_ir_text_newline(self);
		mn::print_to(self.out, "OpFunctionEnd");
	}

	inline static void
	_ir_text_entity_gen(IR_Text& self, const Entity& e)
	{
		switch (e.kind)
		{
		case Entity::KIND_TYPE:
			_ir_text_type_gen(self, e.as_type);
			break;
		case Entity::KIND_FUNC:
			_ir_text_func_gen(self, e.as_func);
			break;
		default:
			// mn_unreachable();
			break;
		}
	}

	// API
	IR_Text
	ir_text_new(mn::Stream out, Module* module)
	{
		IR_Text self{};
		self.out = out;
		self.module = module;
		return self;
	}

	void
	ir_text_free(IR_Text& self)
	{
		mn::set_free(self.generated_entities);
	}

	void
	ir_text_gen(IR_Text& self)
	{
		for (const auto& [_, e]: self.module->entities)
		{
			_ir_text_entity_gen(self, e);
		}
	}
}