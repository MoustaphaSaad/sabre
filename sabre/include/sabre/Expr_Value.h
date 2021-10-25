#pragma once

#include "sabre/Exports.h"
#include "sabre/Tkn.h"

#include <mn/Buf.h>
#include <mn/Json.h>

#include <stdint.h>

namespace sabre
{
	struct Type;

	// expression values, used for compile time const values
	struct Expr_Value
	{
		Type* type;
		union
		{
			bool as_bool;
			int64_t as_int;
			double as_double;
			mn::Map<size_t, Expr_Value> as_aggregate;
		};
	};

	// creates a new bool expression value
	SABRE_EXPORT Expr_Value
	expr_value_bool(bool v);

	// creates a new int expression value
	SABRE_EXPORT Expr_Value
	expr_value_int(int64_t v);

	// creates a new double expression value
	SABRE_EXPORT Expr_Value
	expr_value_double(double v);

	// creates a new aggregate expression value
	SABRE_EXPORT Expr_Value
	expr_value_aggregate(mn::Allocator arena, Type* type);

	// sets the expression value at the given index in aggregate expression
	SABRE_EXPORT void
	expr_value_aggregate_set(Expr_Value& self, size_t index, Expr_Value value);

	// gets the expression value at the given index, if it doesn't exist it will return the empty value
	SABRE_EXPORT Expr_Value
	expr_value_aggregate_get(Expr_Value self, size_t index);

	// performs a logical or between two booleans, returns none value if one of them is not a bool
	SABRE_EXPORT Expr_Value
	expr_value_logic_or(Expr_Value a, Expr_Value b);

	// performs a logical and between two booleans, returns none value if one of them is not a bool
	SABRE_EXPORT Expr_Value
	expr_value_logic_and(Expr_Value a, Expr_Value b);

	// performs a compare and returns a boolean with the compare result
	SABRE_EXPORT Expr_Value
	expr_value_cmp(Expr_Value a, Tkn::KIND op, Expr_Value b);

	// performs an addition between 2 numerical values
	SABRE_EXPORT Expr_Value
	expr_value_add(Expr_Value a, Expr_Value b);

	// performs a subtraction between 2 numerical values
	SABRE_EXPORT Expr_Value
	expr_value_sub(Expr_Value a, Expr_Value b);

	// performs a multiplication between 2 numerical values
	SABRE_EXPORT Expr_Value
	expr_value_mul(Expr_Value a, Expr_Value b);

	// performs a division between 2 numerical values
	SABRE_EXPORT Expr_Value
	expr_value_div(Expr_Value a, Expr_Value b);

	// performs a mod between 2 numerical values
	SABRE_EXPORT Expr_Value
	expr_value_mod(Expr_Value a, Expr_Value b);

	// performs a bitwise or between 2 numerical values
	SABRE_EXPORT Expr_Value
	expr_value_bit_or(Expr_Value a, Expr_Value b);

	// performs a bitwise and between 2 numerical values
	SABRE_EXPORT Expr_Value
	expr_value_bit_and(Expr_Value a, Expr_Value b);

	// performs a bitwise xor between 2 numerical values
	SABRE_EXPORT Expr_Value
	expr_value_bit_xor(Expr_Value a, Expr_Value b);

	// performs a bitwise left shift between 2 numerical values
	SABRE_EXPORT Expr_Value
	expr_value_bit_left_shift(Expr_Value a, Expr_Value b);

	// performs a bitwise right shift between 2 numerical values
	SABRE_EXPORT Expr_Value
	expr_value_bit_right_shift(Expr_Value a, Expr_Value b);

	// performs a binary operation between 2 expression values
	SABRE_EXPORT Expr_Value
	expr_value_binary_op(Expr_Value a, Tkn::KIND op, Expr_Value b);

	// performs a bitwise not on the given value
	SABRE_EXPORT Expr_Value
	expr_value_bit_not(Expr_Value a);

	// performs an unary operation on an expression value
	SABRE_EXPORT Expr_Value
	expr_value_unary_op(Expr_Value a, Tkn::KIND op);

	// returns the default expr value for said type, the arena is used in case of aggregate types
	SABRE_EXPORT Expr_Value
	expr_value_zero(mn::Allocator arena, Type* t);

	// converts an expression value to json value
	SABRE_EXPORT mn::json::Value
	expr_value_to_json(Expr_Value a);
}