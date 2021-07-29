#include "sabre/Expr_Value.h"
#include "sabre/Type_Interner.h"

namespace sabre
{
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
	expr_value_array(Type* type, mn::Buf<Expr_Value> values)
	{
		Expr_Value self{};
		self.type = type;
		self.as_array = values;
		return self;
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