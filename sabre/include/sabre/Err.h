#pragma once

#include "sabre/Tkn.h"

#include <mn/Str.h>

namespace sabre
{
	struct Err
	{
		Pos pos;
		Rng rng;
		mn::Str msg;
	};

	inline static Err
	err_str(mn::Str msg)
	{
		Err self{};
		self.msg = msg;
		return self;
	}

	inline static Err
	err_tkn(const Tkn& tkn, mn::Str msg)
	{
		Err self{};
		self.pos = tkn.pos;
		self.rng = tkn.rng;
		self.msg = msg;
		return self;
	}

	inline static void
	err_free(Err& self)
	{
		mn::str_free(self.msg);
	}

	inline static void
	destruct(Err& self)
	{
		err_free(self);
	}
}