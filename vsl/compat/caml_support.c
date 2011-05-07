/* Copyright 2011  Petter Urkedal
 *
 * This file is part of the Viz Standard Library <http://www.vizlang.org/>.
 *
 * The Viz Standard Library (VSL) is free software: you can redistribute it
 * and/or modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * The VSL is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with the VSL.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <stdio.h>
#include <stdint.h>
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
cviz_some(value x)
{
    value xopt;
    xopt = caml_alloc(1, 0);
    Store_field(xopt, 0, x);
    return xopt;
}

value
cviz_ustring_of_utf8(value x)
{
    CAMLparam1 (x);
    CAMLreturn (caml_callback(CALLBACK(ustring_of_utf8), x));
}

value
cviz_ustring_to_utf8(value x)
{
    CAMLparam1 (x);
    CAMLreturn (caml_callback(CALLBACK(ustring_to_utf8), x));
}

value
cviz_copy_ustring(char const *s)
{
    CAMLparam0 ();
    CAMLlocal1 (v);
    v = caml_copy_string(s);
    CAMLreturn (cviz_ustring_of_utf8(v));
}

value cviz_nat32_mul(value x, value y)
{ return caml_copy_int32((uint32_t)Int32_val(x) * (uint32_t)Int32_val(y)); }

value cviz_nat32_div(value x, value y)
{ return caml_copy_int32((uint32_t)Int32_val(x) / (uint32_t)Int32_val(y)); }

value cviz_nat32_mod(value x, value y)
{ return caml_copy_int32((uint32_t)Int32_val(x) % (uint32_t)Int32_val(y)); }

value cviz_nat64_mul(value x, value y)
{ return caml_copy_int64((uint64_t)Int64_val(x) * (uint64_t)Int64_val(y)); }

value cviz_nat64_div(value x, value y)
{ return caml_copy_int64((uint64_t)Int64_val(x) / (uint64_t)Int64_val(y)); }

value cviz_nat64_mod(value x, value y)
{ return caml_copy_int64((uint64_t)Int64_val(x) % (uint64_t)Int64_val(y)); }

value cviz_nat32_as_int(value x)
{ return Val_int((uint32_t)Int32_val(x)); }

value cviz_nat64_as_int(value x)
{ return Val_int((uint32_t)Int64_val(x)); }

value cviz_nat32_of_int(value x)
{ return caml_copy_int32((uint32_t)Int_val(x)); }

value cviz_nat64_of_int(value x)
{ return caml_copy_int64((uint64_t)Int_val(x)); }
