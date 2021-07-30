#include "sabre/Expr_Value.h"
#include "sabre/Type_Interner.h"

namespace sabre
{
	inline static Type*
	_get_subtype_by_index(Type* type, size_t index)
	{
		switch (type->kind)
		{
		case Type::KIND_VOID:
		case Type::KIND_BOOL:
		case Type::KIND_INT:
		case Type::KIND_UINT:
		case Type::KIND_FLOAT:
		case Type::KIND_DOUBLE:
		case Type::KIND_FUNC:
		case Type::KIND_TEXTURE:
		case Type::KIND_PACKAGE:
		case Type::KIND_FUNC_OVERLOAD_SET:
			return nullptr;
		case Type::KIND_VEC:
			if (type->vec.width > index)
				return type->vec.base;
			else
				return nullptr;
		case Type::KIND_MAT:
			if (type->mat.width > index)
				return type->mat.base;
			else
				return nullptr;
		case Type::KIND_STRUCT:
			if (type->struct_type.fields.count > index)
				return type->struct_type.fields[index].type;
			else
				return nullptr;
		case Type::KIND_ARRAY:
			if (index < type->array.count)
				return type->array.base;
			else
				return nullptr;
		case Type::KIND_ENUM:
			if (index < type->enum_type.fields.count)
				return type_int;
			else
				return nullptr;
		default:
			break;
		}
	}

	inline static Expr_Value
	_expr_value_for_type(Type* type)
	{
		if (type_is_equal(type, type_void))
		{
			return Expr_Value{};
		}
		else if (type_is_equal(type, type_bool))
		{
			return expr_value_bool(false);
		}
		else if (type_is_equal(type, type_int) || type_is_equal(type, type_uint))
		{
			return expr_value_int(0);
		}
		else if (type_is_equal(type, type_float) || type_is_equal(type, type_double))
		{
			return expr_value_double(0);
		}
		else
		{
			assert(false && "unhandled expression value type");
		}
	}

	// API
	Expr_Value
	expr_value_bool(bool v)
	{
		Expr_Value self{};
		self.type = type_bool;
		self.as_bool = v;
		return self;
	}

	Expr_Value
	expr_value_int(int64_t v)
	{
		Expr_Value self{};
		self.type = type_int;
		self.as_int = v;
		return self;
	}

	Expr_Value
	expr_value_double(double v)
	{
		Expr_Value self{};
		self.type = type_double;
		self.as_double = v;
		return self;
	}

	Expr_Value
	expr_value_aggregate(mn::Allocator arena, Type* type)
	{
		Expr_Value self{};
		self.type = type;
		self.as_aggregate = mn::map_with_allocator<size_t, Expr_Value>(arena);
		return self;
	}

	void
	expr_value_aggregate_set(Expr_Value& self, size_t index, Expr_Value value)
	{
		mn::map_insert(self.as_aggregate, index, value);
	}

	Expr_Value
	expr_value_aggregate_get(Expr_Value self, size_t index)
	{
		if (auto it = mn::map_lookup(self.as_aggregate, index))
			return it->value;

		if (auto sub_type = _get_subtype_by_index(self.type, index))
			return _expr_value_for_type(sub_type);
		return {};
	}

	Expr_Value
	expr_value_logic_or(Expr_Value a, Expr_Value b)
	{
		if (a.type != type_bool || b.type != type_bool)
			return Expr_Value{};

		return expr_value_bool(a.as_bool || b.as_bool);
	}

	Expr_Value
	expr_value_logic_and(Expr_Value a, Expr_Value b)
	{
		if (a.type != type_bool || b.type != type_bool)
			return Expr_Value{};

		return expr_value_bool(a.as_bool && b.as_bool);
	}

	Expr_Value
	expr_value_cmp(Expr_Value a, Tkn::KIND op, Expr_Value b)
	{
		int r = 0;
		if (a.type == type_int && b.type == type_int)
		{
			r = a.as_int - b.as_int;
		}
		else if (a.type == type_double && b.type == type_double)
		{
			if (a.as_double < b.as_double)
				r = -1;
			else if (a.as_double > b.as_double)
				r = 1;
			else
				r = 0;
		}
		else if (a.type == type_int && b.type == type_double)
		{
			double a_double = a.as_int;
			if (a_double < b.as_double)
				r = -1;
			else if (a_double > b.as_double)
				r = 1;
			else
				r = 0;
		}
		else if (a.type == type_double && b.type == type_int)
		{
			double b_double = b.as_double;
			if (a.as_double < b_double)
				r = -1;
			else if (a.as_double > b_double)
				r = 1;
			else
				r = 0;
		}
		else
		{
			return Expr_Value{};
		}

		switch (op)
		{
		case Tkn::KIND_LESS:
			return expr_value_bool(r < 0);
		case Tkn::KIND_GREATER:
			return expr_value_bool(r > 0);
		case Tkn::KIND_LESS_EQUAL:
			return expr_value_bool(r <= 0);
		case Tkn::KIND_GREATER_EQUAL:
			return expr_value_bool(r >= 0);
		case Tkn::KIND_EQUAL_EQUAL:
			return expr_value_bool(r == 0);
		case Tkn::KIND_NOT_EQUAL:
			return expr_value_bool(r != 0);
		default:
			assert(false && "unreachable");
			return Expr_Value{};
		}
	}

	Expr_Value
	expr_value_add(Expr_Value a, Expr_Value b)
	{
		if (a.type == type_int && b.type == type_int)
		{
			return expr_value_int(a.as_int + b.as_int);
		}
		else if (a.type == type_double && b.type == type_double)
		{
			return expr_value_double(a.as_double + b.as_double);
		}
		else if (a.type == type_int && b.type == type_double)
		{
			double a_double = a.as_int;
			return expr_value_double(a_double + b.as_double);
		}
		else if (a.type == type_double && b.type == type_int)
		{
			double b_double = b.as_double;
			return expr_value_double(a.as_double + b_double);
		}
		else
		{
			return Expr_Value{};
		}
	}

	Expr_Value
	expr_value_sub(Expr_Value a, Expr_Value b)
	{
		if (a.type == type_int && b.type == type_int)
		{
			return expr_value_int(a.as_int - b.as_int);
		}
		else if (a.type == type_double && b.type == type_double)
		{
			return expr_value_double(a.as_double - b.as_double);
		}
		else if (a.type == type_int && b.type == type_double)
		{
			double a_double = a.as_int;
			return expr_value_double(a_double - b.as_double);
		}
		else if (a.type == type_double && b.type == type_int)
		{
			double b_double = b.as_double;
			return expr_value_double(a.as_double - b_double);
		}
		else
		{
			return Expr_Value{};
		}
	}

	Expr_Value
	expr_value_mul(Expr_Value a, Expr_Value b)
	{
		if (a.type == type_int && b.type == type_int)
		{
			return expr_value_int(a.as_int * b.as_int);
		}
		else if (a.type == type_double && b.type == type_double)
		{
			return expr_value_double(a.as_double * b.as_double);
		}
		else if (a.type == type_int && b.type == type_double)
		{
			double a_double = a.as_int;
			return expr_value_double(a_double * b.as_double);
		}
		else if (a.type == type_double && b.type == type_int)
		{
			double b_double = b.as_double;
			return expr_value_double(a.as_double * b_double);
		}
		else
		{
			return Expr_Value{};
		}
	}

	Expr_Value
	expr_value_div(Expr_Value a, Expr_Value b)
	{
		if (a.type == type_int && b.type == type_int)
		{
			return expr_value_int(a.as_int / b.as_int);
		}
		else if (a.type == type_double && b.type == type_double)
		{
			return expr_value_double(a.as_double / b.as_double);
		}
		else if (a.type == type_int && b.type == type_double)
		{
			double a_double = a.as_int;
			return expr_value_double(a_double / b.as_double);
		}
		else if (a.type == type_double && b.type == type_int)
		{
			double b_double = b.as_double;
			return expr_value_double(a.as_double / b_double);
		}
		else
		{
			return Expr_Value{};
		}
	}

	Expr_Value
	expr_value_mod(Expr_Value a, Expr_Value b)
	{
		if (a.type == type_int && b.type == type_int)
		{
			return expr_value_int(a.as_int % b.as_int);
		}
		else
		{
			return Expr_Value{};
		}
	}

	Expr_Value
	expr_value_binar_op(Expr_Value a, Tkn::KIND op, Expr_Value b)
	{
		switch (op)
		{
		case Tkn::KIND_LOGICAL_OR:
			return expr_value_logic_or(a, b);
		case Tkn::KIND_LOGICAL_AND:
			return expr_value_logic_and(a, b);
		case Tkn::KIND_LESS:
		case Tkn::KIND_GREATER:
		case Tkn::KIND_LESS_EQUAL:
		case Tkn::KIND_GREATER_EQUAL:
		case Tkn::KIND_EQUAL_EQUAL:
		case Tkn::KIND_NOT_EQUAL:
			return expr_value_cmp(a, op, b);
		case Tkn::KIND_PLUS:
			return expr_value_add(a, b);
		case Tkn::KIND_MINUS:
			return expr_value_sub(a, b);
		case Tkn::KIND_STAR:
			return expr_value_mul(a, b);
		case Tkn::KIND_DIVIDE:
			return expr_value_div(a, b);
		case Tkn::KIND_MODULUS:
			return expr_value_mod(a, b);
		default:
			assert(false && "unreachable");
			return Expr_Value{};
		}
	}

	Expr_Value
	expr_value_unary_op(Expr_Value a, Tkn::KIND op)
	{
		switch (op)
		{
		case Tkn::KIND_INC:
			if (a.type == type_int)
				return expr_value_int(a.as_int + 1);
			else if (a.type == type_double)
				return expr_value_double(a.as_double + 1);
			else
				return Expr_Value{};
		case Tkn::KIND_DEC:
			if (a.type == type_int)
				return expr_value_int(a.as_int - 1);
			else if (a.type == type_double)
				return expr_value_double(a.as_double - 1);
			else
				return Expr_Value{};
		case Tkn::KIND_LOGICAL_NOT:
			if (a.type == type_bool)
				return expr_value_bool(!a.as_bool);
			else
				return Expr_Value{};
		case Tkn::KIND_PLUS:
			return a;
		case Tkn::KIND_MINUS:
			if (a.type == type_int)
				return expr_value_int(-a.as_int);
			else if (a.type == type_double)
				return expr_value_double(-a.as_double);
			else
				return Expr_Value{};
		default:
			assert(false && "unreachable");
			return Expr_Value{};
		}
	}
}