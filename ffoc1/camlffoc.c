/* Copyright 2011  Petter Urkedal
 *
 * This file is part of the Fform Standard Library.
 *
 * The Fform Standard Library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * Fform is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Fform.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <stdio.h>
#include <assert.h>

static value ustring_of_utf8 = 0;
static value ustring_to_utf8 = 0;

#define CALLBACK(x) ((x)? (x) : load_callback(&x, #x))

static value
load_callback(value *p, char const *s)
{
    value *q = caml_named_value(s);
    if (q == NULL) {
	fprintf(stderr,
		"A closure named \"%s\" should have been registered.\n", s);
	abort();
    }
    return *p = *q;
}

value
ffoc_some(value x)
{
    value xopt;
    xopt = caml_alloc(1, 0);
    Store_field(xopt, 0, x);
    return xopt;
}

value
ffoc_ustring_of_utf8(value x)
{
    CAMLparam1 (x);
    CAMLreturn (caml_callback(CALLBACK(ustring_of_utf8), x));
}

value
ffoc_ustring_to_utf8(value x)
{
    CAMLparam1 (x);
    CAMLreturn (caml_callback(CALLBACK(ustring_to_utf8), x));
}

value
ffoc_copy_ustring(char const *s)
{
    CAMLparam0 ();
    CAMLlocal1 (v);
    v = caml_copy_string(s);
    CAMLreturn (ffoc_ustring_of_utf8(v));
}
