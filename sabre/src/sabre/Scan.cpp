#include "sabre/Scan.h"
#include "sabre/Unit.h"

#include <mn/IO.h>

namespace sabre
{
	inline static bool
	_is_whitespace(mn::Rune c)
	{
		return (
			c == ' '  ||
			c == '\n' ||
			c == '\t' ||
			c == '\r' ||
			c == '\v'
		);
	}

	inline static int
	_digit_value(mn::Rune c)
	{
		if (c >= '0' && c <= '9')
			return c - '0';
		else if (c >= 'a' && c <= 'f')
			return c - 'a' + 10;
		else if (c >= 'A' && c <= 'F')
			return c - 'A' + 10;
		else
			return 16;
	}

	inline static bool
	_scanner_eof(const Scanner& self)
	{
		return self.it >= end(self.unit->content);
	}

	inline static void
	_scanner_ensure_line_is_added(Scanner& self)
	{
		mn::buf_push(self.unit->lines, Rng{
			self.line_begin,
			end(self.unit->content)
		});
	}

	inline static bool
	_scanner_eat(Scanner& self)
	{
		if (_scanner_eof(self))
		{
			_scanner_ensure_line_is_added(self);
			return false;
		}

		auto prev = self.c;
		auto prev_it = self.it;

		self.it = mn::rune_next(self.it);
		self.c = mn::rune_read(self.it);

		++self.pos.col;
		if (prev == '\n')
		{
			self.pos.col = 1;
			++self.pos.line;
			mn::buf_push(self.unit->lines, Rng{
				self.line_begin,
				prev_it
			});
			self.line_begin = self.it;
		}
		return true;
	}

	inline static void
	_scanner_skip_whitespace(Scanner& self)
	{
		while (_is_whitespace(self.c))
			if (_scanner_eat(self) == false)
				break;
	}

	inline static const char*
	_scanner_scan_id(Scanner& self)
	{
		auto begin_it = self.it;
		while (mn::rune_is_letter(self.c) || mn::rune_is_number(self.c) || self.c == '_')
			if (_scanner_eat(self) == false)
				break;
		return unit_intern(self.unit, begin_it, self.it);
	}

	inline static bool
	_scanner_scan_digits(Scanner& self, int base)
	{
		bool found = false;
		while (_digit_value(self.c) < base)
		{
			found = true;
			if (_scanner_eat(self) == false)
				break;
		}
		return found;
	}

	inline static void
	_scanner_scan_num(Scanner& self, Tkn& tkn)
	{
		auto begin_it = self.it;
		auto begin_pos = self.pos;
		tkn.kind = Tkn::KIND_LITERAL_INTEGER;

		if (self.c == '0')
		{
			auto backup_it = self.it;
			auto backup_pos = self.pos;
			_scanner_eat(self); // for the 0

			int base = 0;
			switch (self.c)
			{
			case 'b':
			case 'B':
				base = 2;
				break;
			case 'o':
			case 'O':
				base = 8;
				break;
			case 'd':
			case 'D':
				base = 10;
				break;
			case 'x':
			case 'X':
				base = 16;
				break;
			}

			if (base != 0)
			{
				if (_scanner_scan_digits(self, base) == false)
				{
					Err err{};
					err.loc.pos = begin_pos;
					err.loc.rng = Rng{begin_it, self.it};
					err.loc.unit = self.unit;
					err.msg = mn::strf("illegal int literal {:c}", self.c);
					unit_err(self.unit, err);
				}
				tkn.str = unit_intern(self.unit, begin_it, self.it);
				return;
			}

			// this is not a 0x number
			self.it = backup_it;
			self.c = mn::rune_read(self.it);
			self.pos = backup_pos;
		}

		// since this is not a 0x number
		// it might be an integer or float so parse a decimal number anyway
		if (_scanner_scan_digits(self, 10) == false)
		{
			Err err{};
			err.loc.pos = begin_pos;
			err.loc.rng = Rng{begin_it, self.it};
			err.loc.unit = self.unit;
			err.msg = mn::strf("illegal int literal {:c}", self.c);
			unit_err(self.unit, err);
		}

		// float part .
		if (self.c == '.')
		{
			tkn.kind = Tkn::KIND_LITERAL_FLOAT;
			_scanner_eat(self); // for the .
			// for the after . part
			if (_scanner_scan_digits(self, 10) == false)
			{
				Err err{};
				err.loc.pos = begin_pos;
				err.loc.rng = Rng{begin_it, self.it};
				err.loc.unit = self.unit;
				err.msg = mn::strf("illegal float literal {:c}", self.c);
				unit_err(self.unit, err);
			}
		}

		// scientific notation part
		if (self.c == 'e' || self.c == 'E')
		{
			tkn.kind = Tkn::KIND_LITERAL_FLOAT;
			_scanner_eat(self); // for the e
			if (self.c == '-' || self.c == '+')
				_scanner_eat(self);
			if (_scanner_scan_digits(self, 10) == false)
			{
				Err err{};
				err.loc.pos = begin_pos;
				err.loc.rng = Rng{begin_it, self.it};
				err.loc.unit = self.unit;
				err.msg = mn::strf("illegal float literal {:c}", self.c);
				unit_err(self.unit, err);
			}
		}

		tkn.str = unit_intern(self.unit, begin_it, self.it);
	}

	inline static const char*
	_scanner_scan_comment(Scanner& self)
	{
		auto begin_it = self.it;
		auto end_it = self.it;

		while (self.c == '\n')
		{
			// windows style /r/n
			if (self.c == '\r')
			{
				if (_scanner_eat(self) == false || self.c == '\n')
					break;
			}

			if (_scanner_eof(self) == false)
				break;
			end_it = self.it;
		}

		_scanner_eat(self); // for the \n
		return unit_intern(self.unit, begin_it, end_it);
	}

	inline static const char*
	_scanner_scan_string(Scanner& self)
	{
		auto begin_it = self.it;
		auto end_it = self.it;

		auto prev = self.c;
		// eat all the runes even those escaped by \ like \"
		while (self.c != '"' || prev == '\\')
		{
			if (_scanner_eat(self) == false)
				break;
			prev = self.c;
		}

		end_it = self.it;
		_scanner_eat(self); // for the "
		return unit_intern(self.unit, begin_it, end_it);
	}


	// API
	Scanner
	scanner_new(Unit_File* unit)
	{
		Scanner self{};
		self.unit = unit;
		self.it = unit->content.ptr;
		self.c = mn::rune_read(self.it);
		self.pos.line = 1;
		self.line_begin = self.it;

		for (int i = Tkn::KIND_KEYWORDS__BEGIN + 1; i < Tkn::KIND_KEYWORDS__END; ++i)
			self.keywords[i - Tkn::KIND_KEYWORDS__BEGIN] = unit_intern(self.unit, Tkn::NAMES[i]);

		return self;
	}

	Tkn
	scanner_scan(Scanner& self)
	{
		_scanner_skip_whitespace(self);

		Tkn tkn{};
		if (_scanner_eof(self))
		{
			_scanner_ensure_line_is_added(self);
			tkn.kind = Tkn::KIND_EOF;
			return tkn;
		}

		tkn.loc.pos = self.pos;
		tkn.loc.rng.begin = self.it;
		tkn.loc.unit = self.unit;

		if (mn::rune_is_letter(self.c))
		{
			tkn.kind = Tkn::KIND_ID;
			tkn.str = _scanner_scan_id(self);

			for (int i = Tkn::KIND_KEYWORDS__BEGIN + 1; i < Tkn::KIND_KEYWORDS__END; ++i)
			{
				if (tkn.str == self.keywords[i - Tkn::KIND_KEYWORDS__BEGIN])
				{
					tkn.kind = (Tkn::KIND)i;
					break;
				}
			}
		}
		else if (mn::rune_is_number(self.c))
		{
			_scanner_scan_num(self, tkn);
		}
		else
		{
			auto c = self.c;
			auto begin_pos = self.pos;
			_scanner_eat(self);
			bool no_intern = false;

			switch (c)
			{
			case '(':
				tkn.kind = Tkn::KIND_OPEN_PAREN;
				break;
			case ')':
				tkn.kind = Tkn::KIND_CLOSE_PAREN;
				break;
			case '{':
				tkn.kind = Tkn::KIND_OPEN_CURLY;
				break;
			case '}':
				tkn.kind = Tkn::KIND_CLOSE_CURLY;
				break;
			case '[':
				tkn.kind = Tkn::KIND_OPEN_BRACKET;
				break;
			case ']':
				tkn.kind = Tkn::KIND_CLOSE_BRACKET;
				break;
			case ':':
				tkn.kind = Tkn::KIND_COLON;
				break;
			case ';':
				tkn.kind = Tkn::KIND_SEMICOLON;
				break;
			case ',':
				tkn.kind = Tkn::KIND_COMMA;
				break;
			case '.':
				tkn.kind = Tkn::KIND_DOT;
				break;
			case '@':
				tkn.kind = Tkn::KIND_AT;
				break;
			case '!':
				tkn.kind = Tkn::KIND_LOGICAL_NOT;
				break;
			case '<':
				tkn.kind = Tkn::KIND_LESS;
				if (self.c == '=')
				{
					tkn.kind = Tkn::KIND_LESS_EQUAL;
					_scanner_eat(self);
				}
				break;
			case '>':
				tkn.kind = Tkn::KIND_GREATER;
				if (self.c == '=')
				{
					tkn.kind = Tkn::KIND_GREATER_EQUAL;
					_scanner_eat(self);
				}
				break;
			case '=':
				tkn.kind = Tkn::KIND_EQUAL;
				if (self.c == '=')
				{
					tkn.kind = Tkn::KIND_EQUAL_EQUAL;
					_scanner_eat(self);
				}
				break;
			case '+':
				tkn.kind = Tkn::KIND_PLUS;
				if (self.c == '=')
				{
					tkn.kind = Tkn::KIND_PLUS_EQUAL;
					_scanner_eat(self);
				}
				else if (self.c == '+')
				{
					tkn.kind = Tkn::KIND_INC;
					_scanner_eat(self);
				}
				break;
			case '-':
				tkn.kind = Tkn::KIND_MINUS;
				if (self.c == '=')
				{
					tkn.kind = Tkn::KIND_MINUS_EQUAL;
					_scanner_eat(self);
				}
				else if (self.c == '-')
				{
					tkn.kind = Tkn::KIND_DEC;
					_scanner_eat(self);
				}
				break;
			case '*':
				tkn.kind = Tkn::KIND_STAR;
				if (self.c == '=')
				{
					tkn.kind = Tkn::KIND_STAR_EQUAL;
					_scanner_eat(self);
				}
				break;
			case '/':
				tkn.kind = Tkn::KIND_DIVIDE;
				if (self.c == '=')
				{
					tkn.kind = Tkn::KIND_DIVIDE_EQUAL;
					_scanner_eat(self);
				}
				else if (self.c == '/')
				{
					tkn.kind = Tkn::KIND_COMMENT;
					_scanner_eat(self); // for the second /
					tkn.str = _scanner_scan_comment(self);
					no_intern = true;
				}
				break;
			case '%':
				tkn.kind = Tkn::KIND_MODULUS;
				if (self.c == '=')
				{
					tkn.kind = Tkn::KIND_MODULUS_EQUAL;
					_scanner_eat(self);
				}
				break;
			case '|':
				if (self.c == '|')
				{
					tkn.kind = Tkn::KIND_LOGICAL_OR;
				}
				break;
			case '&':
				if (self.c == '&')
				{
					tkn.kind = Tkn::KIND_LOGICAL_AND;
				}
				break;
			case '"':
				tkn.kind = Tkn::KIND_LITERAL_STRING;
				tkn.str = _scanner_scan_string(self);
				no_intern = true;
				break;
			default:
			{
				Err err{};
				err.loc.pos = begin_pos;
				err.loc.unit = self.unit;
				err.msg = mn::strf("illegal rune {:c}", c);
				unit_err(self.unit, err);
				break;
			}
			}

			if (no_intern == false)
			{
				tkn.str = unit_intern(self.unit, tkn.loc.rng.begin, self.it);
			}
		}

		tkn.loc.rng.end = self.it;
		return tkn;
	}
}