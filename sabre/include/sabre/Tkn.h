#pragma once

#include "sabre/Token_List.h"

#include <stdint.h>

namespace sabre
{
	struct Pos
	{
		uint32_t line, col;
	};

	struct Rng
	{
		const char *begin, *end;
	};

	struct Unit;

	struct Location
	{
		Pos pos;
		Rng rng;
		Unit* unit;
	};

	struct Tkn
	{
		enum KIND
		{
			#define TOKEN(TKN, NAME) KIND_##TKN
				TOKEN_LIST(TOKEN)
			#undef TOKEN
		};

		inline static const char* NAMES[] = {
			#define TOKEN(TKN, NAME) NAME
				TOKEN_LIST(TOKEN)
			#undef TOKEN
		};

		KIND kind;
		const char* str;
		Location loc;

		inline operator bool() const { return kind != KIND_NONE; }
	};

	// returns whether a token kind can be ignored
	inline static bool
	tkn_can_ignore(Tkn::KIND kind)
	{
		return kind == Tkn::KIND_COMMENT;
	}

	// returns whether a token is a compare token (used for precedence)
	inline static bool
	tkn_is_cmp(Tkn::KIND kind)
	{
		return (
			kind == Tkn::KIND_LESS ||
			kind == Tkn::KIND_GREATER ||
			kind == Tkn::KIND_LESS_EQUAL ||
			kind == Tkn::KIND_GREATER_EQUAL ||
			kind == Tkn::KIND_EQUAL_EQUAL ||
			kind == Tkn::KIND_NOT_EQUAL
		);
	}

	// returns whether a token is of an add precedence
	inline static bool
	tkn_is_add(Tkn::KIND kind)
	{
		return (
			kind == Tkn::KIND_PLUS ||
			kind == Tkn::KIND_MINUS
		);
	}

	// returns whether a token is a unary operator
	inline static bool
	tkn_is_unary(Tkn::KIND kind)
	{
		return (
			kind == Tkn::KIND_INC ||
			kind == Tkn::KIND_DEC ||
			kind == Tkn::KIND_PLUS ||
			kind == Tkn::KIND_MINUS ||
			kind == Tkn::KIND_LOGICAL_NOT
		);
	}

	// returns whether a token is of an multiply precedence
	inline static bool
	tkn_is_mul(Tkn::KIND kind)
	{
		return (
			kind == Tkn::KIND_STAR ||
			kind == Tkn::KIND_DIVIDE ||
			kind == Tkn::KIND_MODULUS
		);
	}

	// returns whether an operator is an assignment operator
	inline static bool
	tkn_is_assign(Tkn::KIND kind)
	{
		return (
			kind == Tkn::KIND_EQUAL ||
			kind == Tkn::KIND_PLUS_EQUAL ||
			kind == Tkn::KIND_MINUS_EQUAL ||
			kind == Tkn::KIND_STAR_EQUAL ||
			kind == Tkn::KIND_DIVIDE_EQUAL ||
			kind == Tkn::KIND_MODULUS_EQUAL
		);
	}
}
#undef TOKEN_LIST