#pragma once

#include "sabre/Exports.h"
#include "sabre/Tkn.h"

#include <mn/Str.h>
#include <mn/Stream.h>

namespace sabre
{
	struct Err
	{
		Location loc;
		mn::Str msg;
		bool is_note;
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

	// dumps the given error to the output stream
	SABRE_EXPORT void
	err_dump(const Err& err, mn::Stream out);
}