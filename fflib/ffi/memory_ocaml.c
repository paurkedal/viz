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
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <stdio.h>

static struct custom_operations ptr_ops = {
    "org.eideticdew.ffoc.ptr",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

#define value_as_ptr(v) (*(void **)Data_custom_val(v))
#define value_as_offset(v) Long_val(v)

value ffoc_copy_ustring(char const *);

static value
value_of_ptr(void *p)
{
    value v = alloc_custom(&ptr_ops, sizeof(void *), 0, 1);
    value_as_ptr(v) = p;
    return v;
}

CAMLprim value
ffoc_is_null(value p)
{
    return value_as_ptr(p)? Val_true : Val_false;
}

CAMLprim value
ffoc_ptr_add(value offset, value p)
{
    return value_of_ptr((char *)value_as_ptr(p) + Long_val(offset));
}

CAMLprim value
ffoc_ptr_diff(value pb, value pa)
{
    return Val_long((char *)pb - (char *)pa);
}

CAMLprim value
ffoc_show_ptr(value p)
{
    char buf[sizeof(void *)*2 + 13];
    sprintf(buf, "%p", value_as_ptr(p));
    return ffoc_copy_ustring(buf);
}


#define ADDR(t, off, p) (t*)((char *)value_as_ptr(p) + value_as_offset(off))


CAMLprim value ffoc_unsafe_load_ptr(value off, value p)
{ return value_of_ptr(*ADDR(void *, off, p)); }

CAMLprim value ffoc_unsafe_load_u8(value off, value p)
{ return Val_int(*ADDR(unsigned char, off, p)); }

CAMLprim value ffoc_unsafe_load_s8(value off, value p)
{ return Val_int(*ADDR(signed char, off, p)); }

CAMLprim value ffoc_unsafe_load_u16(value off, value p)
{ return Val_int(*ADDR(unsigned short, off, p)); }

CAMLprim value ffoc_unsafe_load_s16(value off, value p)
{ return Val_int(*ADDR(short, off, p)); }

CAMLprim value ffoc_unsafe_load_int32(value off, value p)
{ return Val_int(*ADDR(int32, off, p)); }

CAMLprim value ffoc_unsafe_load_int64(value off, value p)
{ return Val_int(*ADDR(int64, off, p)); }


CAMLprim value ffoc_unsafe_store_ptr(value off, value p, value x)
{ *ADDR(void *, off, p) = value_as_ptr(x); return Val_unit; }

CAMLprim value ffoc_unsafe_store_8(value off, value p, value x)
{ *ADDR(unsigned char, off, p) = Int_val(x); return Val_unit; }

CAMLprim value ffoc_unsafe_store_16(value off, value p, value x)
{ *ADDR(unsigned short, off, p) = Int_val(x); return Val_unit; }

CAMLprim value ffoc_unsafe_store_int32(value off, value p, value x)
{ *ADDR(int32, off, p) = Int32_val(x); return Val_unit; }

CAMLprim value ffoc_unsafe_store_int64(value off, value p, value x)
{ *ADDR(int64, off, p) = Int64_val(x); return Val_unit; }


CAMLprim value
ffoc_unsafe_custom_address(value obj)
{
    return value_of_ptr(Data_custom_val(obj));
}

CAMLprim value
ffoc_unsafe_custom_load_ptr(value obj)
{
    return value_of_ptr(*(void **)Data_custom_val(obj));
}
