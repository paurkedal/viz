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
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <stdio.h>
#include <stdint.h>

#define LOAD_PTR_IS_IDENT 1

/* Use the nativeint rather than the int O'Caml type for offset and size.  If
 * this is changed, also change camlviz/ast_to_cstubs.ml,
 * camlviz/ast_to_costs.ml, and vsl/foreign/C/memory.vz. */
#define OFFSET_IS_NATIVEINT 0

#define DEBUG_STORE 0

#define Voidp_val(v) (*(void **)Data_custom_val(v))
#if OFFSET_IS_NATIVEINT
#  define Offset_val(v) Nativeint_val(v)
#  define copy_offset(i) caml_copy_nativeint(i)
#else
#  define Offset_val(v) Long_val(v)
#  define copy_offset(i) Val_long(i)
#endif

value cviz_copy_ustring(char const *);
value cviz_copy_ptr(void *p);

CAMLprim value
cviz_ptr_cmp(value p, value q)
{
    if (Voidp_val(p) < Voidp_val(q)) return Val_int(0); /* tprec */
    if (Voidp_val(p) > Voidp_val(q)) return Val_int(2); /* tsucc */
    return Val_int(1); /* tcoin */
}

CAMLprim value
cviz_ptr_eq(value p, value q)
{
    return Voidp_val(p) == Voidp_val(q)? Val_true : Val_false;
}

CAMLprim value
cviz_ptr_is_zero(value p)
{
    return Voidp_val(p)? Val_true : Val_false;
}

CAMLprim value
cviz_ptr_get_zero(value p)
{
    return cviz_copy_ptr(NULL);
}

CAMLprim value
cviz_ptr_add(value offset, value p)
{
    return cviz_copy_ptr((char *)Voidp_val(p) + Offset_val(offset));
}

CAMLprim value
cviz_ptr_sub(value offset, value p)
{
    return cviz_copy_ptr((char *)Voidp_val(p) - Offset_val(offset));
}

CAMLprim value
cviz_ptr_diff(value pb, value pa)
{
    return copy_offset((char *)pb - (char *)pa);
}

CAMLprim value
cviz_ptr_show(value p)
{
    char buf[sizeof(void *)*2 + 13];
    sprintf(buf, "%p", Voidp_val(p));
    return cviz_copy_ustring(buf);
}


#define ADDR(t, off, p) (t*)((char *)Voidp_val(p) + Offset_val(off))


CAMLprim value cviz_unsafe_load_ptr(value off, value p)
{ return cviz_copy_ptr(*ADDR(void *, off, p)); }

CAMLprim value cviz_unsafe_load_u8(value off, value p)
{ return Val_int(*ADDR(unsigned char, off, p)); }

CAMLprim value cviz_unsafe_load_s8(value off, value p)
{ return Val_int(*ADDR(signed char, off, p)); }

CAMLprim value cviz_unsafe_load_u16(value off, value p)
{ return Val_int(*ADDR(unsigned short, off, p)); }

CAMLprim value cviz_unsafe_load_s16(value off, value p)
{ return Val_int(*ADDR(short, off, p)); }

CAMLprim value cviz_unsafe_load_int32(value off, value p)
{ return caml_copy_int32(*ADDR(int32, off, p)); }

CAMLprim value cviz_unsafe_load_int64(value off, value p)
{ return caml_copy_int64(*ADDR(int64, off, p)); }



CAMLprim value
cviz_unsafe_store_ptr(value off, value p, value x)
{
#if DEBUG_STORE
    printf("Storing pointer %p to %p + %ld = %p\n", Voidp_val(x),
	   Voidp_val(p), Offset_val(off), ADDR(unsigned char, off, p));
#endif
    *ADDR(void *, off, p) = Voidp_val(x);
    return Val_unit;
}

CAMLprim value
cviz_unsafe_store_8(value off, value p, value x)
{
#if DEBUG_STORE
    printf("Storing byte %x to %p + %ld = %p\n", Int_val(x),
	   Voidp_val(p), Offset_val(off), ADDR(unsigned char, off, p));
#endif
    *ADDR(unsigned char, off, p) = Int_val(x);
    return Val_unit;
}

CAMLprim value
cviz_unsafe_store_16(value off, value p, value x)
{
#if DEBUG_STORE
    printf("Storing 16 bit value %#x to %p + %ld = %p\n", Int_val(x),
	   Voidp_val(p), Offset_val(off), ADDR(unsigned char, off, p));
#endif
    *ADDR(unsigned short, off, p) = Int_val(x);
    return Val_unit;
}

CAMLprim value
cviz_unsafe_store_int32(value off, value p, value x)
{
#if DEBUG_STORE
    printf("Storing 32 bit value %#x to %p + %ld = %p\n", Int32_val(x),
	   Voidp_val(p), Offset_val(off), ADDR(unsigned char, off, p));
#endif
    *ADDR(int32, off, p) = Int32_val(x);
    return Val_unit;
}

CAMLprim value
cviz_unsafe_store_int64(value off, value p, value x)
{
#if DEBUG_STORE
    printf("Storing to %p + %ld = %p\n",
	   Voidp_val(p), Offset_val(off), ADDR(unsigned char, off, p));
#endif
    *ADDR(int64, off, p) = Int64_val(x);
    return Val_unit;
}


CAMLprim value
cviz_unsafe_custom_address(value obj)
{
#if DEBUG_STORE
    printf("Using custom address %p.\n", Data_custom_val(obj));
#endif
    return cviz_copy_ptr(Data_custom_val(obj));
}

CAMLprim value
cviz_unsafe_custom_load_ptr(value obj)
{
#if LOAD_PTR_IS_IDENT
    return obj;
#else
    return cviz_copy_ptr(*(void **)Data_custom_val(obj));
#endif
}
