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

#define LOAD_PTR_IS_IDENT 1
#define DEBUG_STORE 0

#define Voidp_val(v) (*(void **)Data_custom_val(v))
#define Offset_val(v) Nativeint_val(v)

value ffoc_copy_ustring(char const *);
value ffoc_copy_ptr(void *p);

CAMLprim value
ffoc_ptr_cmp(value p, value q)
{
    if (Voidp_val(p) < Voidp_val(q)) return Val_int(0); /* tprec */
    if (Voidp_val(p) > Voidp_val(q)) return Val_int(2); /* tsucc */
    return Val_int(1); /* tcoin */
}

CAMLprim value
ffoc_ptr_eq(value p, value q)
{
    return Voidp_val(p) == Voidp_val(q)? Val_true : Val_false;
}

CAMLprim value
ffoc_is_null(value p)
{
    return Voidp_val(p)? Val_true : Val_false;
}

CAMLprim value
ffoc_ptr_add(value offset, value p)
{
    return ffoc_copy_ptr((char *)Voidp_val(p) + Long_val(offset));
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
    sprintf(buf, "%p", Voidp_val(p));
    return ffoc_copy_ustring(buf);
}


#define ADDR(t, off, p) (t*)((char *)Voidp_val(p) + Offset_val(off))


CAMLprim value ffoc_unsafe_load_ptr(value off, value p)
{ return ffoc_copy_ptr(*ADDR(void *, off, p)); }

CAMLprim value ffoc_unsafe_load_u8(value off, value p)
{ return Val_int(*ADDR(unsigned char, off, p)); }

CAMLprim value ffoc_unsafe_load_s8(value off, value p)
{ return Val_int(*ADDR(signed char, off, p)); }

CAMLprim value ffoc_unsafe_load_u16(value off, value p)
{ return Val_int(*ADDR(unsigned short, off, p)); }

CAMLprim value ffoc_unsafe_load_s16(value off, value p)
{ return Val_int(*ADDR(short, off, p)); }

CAMLprim value ffoc_unsafe_load_int32(value off, value p)
{ return caml_copy_int32(*ADDR(int32, off, p)); }

CAMLprim value ffoc_unsafe_load_int64(value off, value p)
{ return caml_copy_int64(*ADDR(int64, off, p)); }


CAMLprim value
ffoc_unsafe_store_ptr(value off, value p, value x)
{
#if DEBUG_STORE
    printf("Storing pointer %p to %p + %ld = %p\n", Voidp_val(x),
	   Voidp_val(p), Offset_val(off), ADDR(unsigned char, off, p));
#endif
    *ADDR(void *, off, p) = Voidp_val(x);
    return Val_unit;
}

CAMLprim value
ffoc_unsafe_store_8(value off, value p, value x)
{
#if DEBUG_STORE
    printf("Storing byte %d to %p + %ld = %p\n", Int_val(x),
	   Voidp_val(p), Offset_val(off), ADDR(unsigned char, off, p));
#endif
    *ADDR(unsigned char, off, p) = Int_val(x);
    return Val_unit;
}

CAMLprim value
ffoc_unsafe_store_16(value off, value p, value x)
{
#if DEBUG_STORE
    printf("Storing 16 bit %d to %p + %ld = %p\n", Int_val(x),
	   Voidp_val(p), Offset_val(off), ADDR(unsigned char, off, p));
#endif
    *ADDR(unsigned short, off, p) = Int_val(x);
    return Val_unit;
}

CAMLprim value
ffoc_unsafe_store_int32(value off, value p, value x)
{
#if DEBUG_STORE
    printf("Storing 32 bits %d to %p + %ld = %p\n", Int32_val(x),
	   Voidp_val(p), Offset_val(off), ADDR(unsigned char, off, p));
#endif
    *ADDR(int32, off, p) = Int32_val(x);
    return Val_unit;
}

CAMLprim value
ffoc_unsafe_store_int64(value off, value p, value x)
{
#if DEBUG_STORE
    printf("Storing to %p + %ld = %p\n",
	   Voidp_val(p), Offset_val(off), ADDR(unsigned char, off, p));
#endif
    *ADDR(int64, off, p) = Int64_val(x);
    return Val_unit;
}


CAMLprim value
ffoc_unsafe_custom_address(value obj)
{
    return ffoc_copy_ptr(Data_custom_val(obj));
}

CAMLprim value
ffoc_unsafe_custom_load_ptr(value obj)
{
#if LOAD_PTR_IS_IDENT
    return obj;
#else
    return ffoc_copy_ptr(*(void **)Data_custom_val(obj));
#endif
}
