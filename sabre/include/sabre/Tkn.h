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
		Rng rng;
		Pos pos;

		inline operator bool() const { return kind != KIND_NONE; }
	};
}