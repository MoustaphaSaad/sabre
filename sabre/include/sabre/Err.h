#pragma once

#include "sabre/Tkn.h"

#include <mn/Str.h>

namespace sabre
{
	struct Err
	{
		Location loc;
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
		self.loc = tkn.loc;
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