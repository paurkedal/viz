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


/* Stubs for record.vz
 * =================== */

CAMLprim value cviz_load_value(value p)
{ return *(value *)Voidp_val(p); }
CAMLprim value cviz_store_value(value x, value p)
{ *(value *)Voidp_val(p) = x; return Val_unit; }
CAMLprim value cviz_load_ptr(value p)
{ return cviz_copy_ptr(*(void **)Voidp_val(p)); }
CAMLprim value cviz_store_ptr(value x, value p)
{ *(void **)Voidp_val(p) = Voidp_val(x); return Val_unit; }

#define CONV(tn, vn, t, of_val, to_val)					\
									\
    CAMLprim value cviz_##tn##_load_##vn(value p)			\
    {									\
	return to_val(*(t *)Voidp_val(p));				\
    }									\
									\
    CAMLprim value cviz_##tn##_store_##vn(value x, value p)		\
    {									\
	*(t *)Voidp_val(p) = of_val(x);					\
	return Val_unit;						\
    }
CONV(intnat, int, intnat, Long_val, Val_long)
CONV(int8,  int,   int8_t, Long_val, Val_long)
CONV(nat8,  int,   uint8_t, Long_val, Val_long)
CONV(int16, int,   int16_t, Long_val, Val_long)
CONV(nat16, int,   uint16_t, Long_val, Val_long)
CONV(int32, int,   int32_t, Long_val, Val_long)
CONV(nat32, int,   uint32_t, Long_val, Val_long)
CONV(int32, int32, int32_t, Int32_val, caml_copy_int32)
CONV(int64, int,   int64_t, Long_val, Val_long)
CONV(nat64, int,   uint64_t, Long_val, Val_long)
CONV(int64, int64, int64_t, Int64_val, caml_copy_int64)
CONV(sshort,	int, short, Long_val, Val_long)
CONV(ushort,	int, unsigned short, Long_val, Val_long)
CONV(sint,	int, int, Long_val, Val_long)
CONV(uint,	int, unsigned int, Long_val, Val_long)
CONV(slong,	int, long, Long_val, Val_long)
CONV(ulong,	int, unsigned long, Long_val, Val_long)
CONV(slonglong,	int, long long, Long_val, Val_long)
CONV(ulonglong,	int, unsigned long long, Long_val, Val_long)
CONV(sintptr,	int, intptr_t, Long_val, Val_long)
CONV(uintptr,	int, uintptr_t, Long_val, Val_long)
CONV(sintmax,	int, intmax_t, Long_val, Val_long)
CONV(uintmax,	int, uintmax_t, Long_val, Val_long)
CONV(sint,	nint, int, Nativeint_val, caml_copy_nativeint)
CONV(uint,	nint, unsigned int, Nativeint_val, caml_copy_nativeint)
CONV(slong,	nint, long, Nativeint_val, caml_copy_nativeint)
CONV(ulong,	nint, unsigned long, Nativeint_val, caml_copy_nativeint)
CONV(slonglong,	nint, long long, Nativeint_val, caml_copy_nativeint)
CONV(ulonglong,	nint, unsigned long long, Nativeint_val, caml_copy_nativeint)
CONV(sintptr,	nint, intptr_t, Nativeint_val, caml_copy_nativeint)
CONV(uintptr,	nint, uintptr_t, Nativeint_val, caml_copy_nativeint)
CONV(sintmax,	nint, intmax_t, Nativeint_val, caml_copy_nativeint)
CONV(uintmax,	nint, uintmax_t, Nativeint_val, caml_copy_nativeint)
#undef CONV


static struct custom_operations _record_ops = {
    "camlviz.record",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
};

CAMLprim value
cviz_record_alloc(value size)
{
    return caml_alloc_custom(&_record_ops, Offset_val(size), 0, 1);
}
