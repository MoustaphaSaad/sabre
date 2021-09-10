#pragma once

#include "sabre/Exports.h"
#include "sabre/IR.h"

#include <mn/Stream.h>
#include <mn/Map.h>

namespace sabre::spirv
{
	// prints spirv IR in human readable form
	struct IR_Text
	{
		mn::Stream out;
		size_t indent;
		Module* module;

		mn::Set<ID> generated_entities;
	};

	// creates a new IR printer instance
	SABRE_EXPORT IR_Text
	ir_text_new(mn::Stream out, Module* module);

	// frees the given IR printer instance
	SABRE_EXPORT void
	ir_text_free(IR_Text& self);

	// prints the given module
	SABRE_EXPORT void
	ir_text_gen(IR_Text& self);
}