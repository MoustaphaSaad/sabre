#include "sabre/IR_Text.h"
#include "sabre/IR.h"

#include <mn/IO.h>
#include <mn/Assert.h>

namespace sabre::spirv
{
	inline static const char*
	_ir_text_storage_class(STORAGE_CLASS storage_class)
	{
		switch (storage_class)
		{
		case STORAGE_CLASS_FUNCTION:
			return "Function";
		default:
			mn_unreachable();
			return "";
		}
	}

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
		case Type::KIND_BOOL:
			res = mn::strf(res, "%{} = OpTypeBool", type->id);
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
		case Type::KIND_PTR:
			res = mn::strf(
				res,
				"%{} = OpTypePointer {} %{}",
				type->id,
				_ir_text_storage_class(type->as_ptr.storage_class),
				type->as_ptr.base->id
			);
			break;
		default:
			break;
		}

		mn::set_insert(self.generated_entities, type->id);

		_ir_text_newline(self);
		mn::print_to(self.out, "{}", res);
		return mn::str_tmpf("%{}", type->id);
	}

	inline static void
	_ir_text_constant_gen(IR_Text& self, Value* value, Constant data)
	{
		if (mn::set_lookup(self.generated_entities, value->id))
			return;

		_ir_text_newline(self);
		auto type = _ir_text_type_gen(self, value->type);

		switch (value->type->kind)
		{
		case Type::KIND_INT:
			mn::print_to(self.out, "%{} = OpConstant {} {}", value->id, type, data.as_int);
			break;
		case Type::KIND_BOOL:
			if (data.as_bool)
				mn::print_to(self.out, "%{} = OpConstantTrue {}", value->id, type);
			else
				mn::print_to(self.out, "%{} = OpConstantFalse {}", value->id, type);
			break;
		default:
			mn_unreachable();
			break;
		}

		mn::set_insert(self.generated_entities, value->id);
	}

	inline static void
	_ir_text_bb_gen(IR_Text& self, Basic_Block* bb)
	{
		if (mn::set_lookup(self.generated_entities, bb->id))
			return;

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
			case Instruction::Op_IMul:
				mn::print_to(
					self.out,
					"%{} = OpIMul {} %{} %{}",
					instruction.as_imul.res->id,
					_ir_text_type_gen(self, instruction.as_imul.res->type),
					instruction.as_imul.op1->id,
					instruction.as_imul.op2->id
				);
				break;
			case Instruction::Op_SDiv:
				mn::print_to(
					self.out,
					"%{} = OpSDiv {} %{} %{}",
					instruction.as_sdiv.res->id,
					_ir_text_type_gen(self, instruction.as_sdiv.res->type),
					instruction.as_sdiv.op1->id,
					instruction.as_sdiv.op2->id
				);
				break;
			case Instruction::Op_BitwiseAnd:
				mn::print_to(
					self.out,
					"%{} = OpBitwiseAnd {} %{} %{}",
					instruction.as_bitwise_and.res->id,
					_ir_text_type_gen(self, instruction.as_bitwise_and.res->type),
					instruction.as_bitwise_and.op1->id,
					instruction.as_bitwise_and.op2->id
				);
				break;
			case Instruction::Op_IEqual:
				mn::print_to(
					self.out,
					"%{} = OpIEqual {} %{} %{}",
					instruction.as_iequal.res->id,
					_ir_text_type_gen(self, instruction.as_iequal.res->type),
					instruction.as_iequal.op1->id,
					instruction.as_iequal.op2->id
				);
				break;
			case Instruction::Op_Variable:
				mn::print_to(
					self.out,
					"%{} = OpVariable {} {}",
					instruction.as_variable.res->id,
					_ir_text_type_gen(self, instruction.as_variable.type),
					_ir_text_storage_class(instruction.as_variable.storage_class)
				);
				if (instruction.as_variable.init)
					mn::print_to(self.out, " %{}", instruction.as_variable.init->id);
				break;
			case Instruction::Op_Load:
				mn::print_to(
					self.out,
					"%{} = OpLoad {} %{}",
					instruction.as_load.res->id,
					_ir_text_type_gen(self, instruction.as_load.type),
					instruction.as_load.src->id
				);
				break;
			case Instruction::Op_Store:
				mn::print_to(
					self.out,
					"OpStore %{} %{}",
					instruction.as_store.dst->id,
					instruction.as_store.src->id
				);
				break;
			case Instruction::Op_ReturnValue:
				mn::print_to(
					self.out,
					"OpReturnValue %{}",
					instruction.as_return.value->id
				);
				break;
			case Instruction::Op_SelectionMerge:
				mn::print_to(
					self.out,
					"OpSelectionMerge %{} None",
					instruction.as_selection_merge.merge_branch->id
				);
				break;
			case Instruction::Op_BranchConditional:
				mn::print_to(
					self.out,
					"OpBranchConditional %{} %{} %{}",
					instruction.as_branch_conditional.cond->id,
					instruction.as_branch_conditional.true_branch->id,
					instruction.as_branch_conditional.false_branch->id
				);
				_ir_text_bb_gen(self, instruction.as_branch_conditional.true_branch);
				_ir_text_bb_gen(self, instruction.as_branch_conditional.false_branch);
				_ir_text_bb_gen(self, instruction.as_branch_conditional.merge_branch);
				break;
			case Instruction::Op_Branch:
				mn::print_to(
					self.out,
					"OpBranch %{}",
					instruction.as_branch.branch->id
				);
				_ir_text_bb_gen(self, instruction.as_branch.branch);
				break;
			case Instruction::Op_Unreachable:
				mn::print_to(
					self.out,
					"OpUnreachable"
				);
				break;
			default:
				mn_unreachable();
				break;
			}
		}

		mn::set_insert(self.generated_entities, bb->id);
	}

	inline static void
	_ir_text_func_gen(IR_Text& self, Func* func)
	{
		if (mn::set_lookup(self.generated_entities, func->id))
			return;

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

		_ir_text_bb_gen(self, func->entry);

		--self.indent;
		_ir_text_newline(self);
		mn::print_to(self.out, "OpFunctionEnd");

		mn::set_insert(self.generated_entities, func->id);
	}

	inline static void
	_ir_text_entity_gen(IR_Text& self, const Entity& e)
	{
		switch (e.kind)
		{
		case Entity::KIND_TYPE:
			_ir_text_type_gen(self, e.as_type);
			break;
		case Entity::KIND_CONSTANT:
			_ir_text_constant_gen(self, e.as_constant.value, e.as_constant.data);
			break;
		case Entity::KIND_BASIC_BLOCK:
			_ir_text_bb_gen(self, e.as_basic_block);
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
			if (e.kind == Entity::KIND_TYPE)
				_ir_text_entity_gen(self, e);
		}
		for (const auto& [_, e]: self.module->entities)
		{
			if (e.kind == Entity::KIND_CONSTANT)
				_ir_text_entity_gen(self, e);
		}
		for (const auto& [_, e]: self.module->entities)
		{
			if (e.kind == Entity::KIND_FUNC)
				_ir_text_entity_gen(self, e);
		}
	}
}