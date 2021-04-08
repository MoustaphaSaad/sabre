#include "sabre/Unit.h"

#include <mn/Path.h>

namespace sabre
{
	Unit*
	unit_from_file(const mn::Str& filepath)
	{
		auto self = mn::alloc_zerod<Unit>();
		self->filepath = clone(filepath);
		self->content = mn::file_content_str(filepath);
		return self;
	}

	void
	unit_free(Unit* self)
	{
		mn::str_free(self->filepath);
		mn::str_free(self->content);
		mn::str_free(self->program);
		mn::buf_free(self->lines);
		mn::free(self);
	}
}