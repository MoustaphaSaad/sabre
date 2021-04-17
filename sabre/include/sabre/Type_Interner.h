#pragma once

#include "sabre/Exports.h"

#include <mn/Memory.h>
#include <mn/Buf.h>
#include <mn/Map.h>

namespace sabre
{
	struct Type;

	// describes a function signature
	struct Func_Sign
	{
		mn::Buf<Type*> args;
		Type* return_type;

		bool
		operator==(const Func_Sign& other) const
		{
			if (args.count != other.args.count)
				return false;

			for (size_t i = 0; i < args.count; ++i)
				if (args[i] != other.args[i])
					return false;

			if (return_type != other.return_type)
				return false;

			return true;
		}

		bool
		operator!=(const Func_Sign& other) const
		{
			return !operator==(other);
		}
	};

	// creates a new function signature
	inline static Func_Sign
	func_sign_new()
	{
		return Func_Sign{};
	}

	// frees the given function signature
	inline static void
	func_sign_free(Func_Sign& self)
	{
		mn::buf_free(self.args);
	}

	inline static void
	destruct(Func_Sign& self)
	{
		func_sign_free(self);
	}

	// used to hash a function signature
	struct Func_Sign_Hasher
	{
		inline size_t
		operator()(const Func_Sign& sign) const
		{
			return mn::hash_mix(mn::murmur_hash(block_from(sign.args)), mn::Hash<Type*>()(sign.return_type));
		}
	};

	// represents a data type
	struct Type
	{
		enum KIND
		{
			KIND_VOID,
			KIND_BOOL,
			KIND_INT,
			KIND_UINT,
			KIND_FLOAT32,
			KIND_FLOAT64,
			KIND_FUNC,
		};

		KIND kind;
		union
		{
			Func_Sign func;
		};
	};

	// language basic datatypes
	SABRE_EXPORT extern Type* type_void;
	SABRE_EXPORT extern Type* type_bool;
	SABRE_EXPORT extern Type* type_int;
	SABRE_EXPORT extern Type* type_uint;
	SABRE_EXPORT extern Type* type_float32;
	SABRE_EXPORT extern Type* type_float64;

	// interns the different types to make comparisons and memory management easier
	struct Type_Interner
	{
		mn::Allocator arena;
		mn::Map<Func_Sign, Type*, Func_Sign_Hasher> func_table;
	};

	// creates a new type interner
	SABRE_EXPORT Type_Interner
	type_interner_new();

	// frees the given type interner
	SABRE_EXPORT void
	type_interner_free(Type_Interner& self);

	inline static void
	destruct(Type_Interner& self)
	{
		type_interner_free(self);
	}

	// interns a function signature into a type, it will consume the given function signature
	SABRE_EXPORT Type*
	type_interner_func(Type_Interner& self, Func_Sign sign);
}