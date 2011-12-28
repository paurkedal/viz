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

#ifndef VSL_LENSTYPE
#  error "This file must be included from libvsl/lens.h."
#endif

VSL_LENSTYPE(int8_t,	int,   int8_t, Long_val, Val_long)
VSL_LENSTYPE(uint8_t,	int,   uint8_t, Long_val, Val_long)
VSL_LENSTYPE(int16_t,	int,   int16_t, Long_val, Val_long)
VSL_LENSTYPE(uint16_t,	int,   uint16_t, Long_val, Val_long)
VSL_LENSTYPE(xint32_t,	int32, int32_t, Int32_val, caml_copy_int32)
VSL_LENSTYPE(xint64_t,	int64, int64_t, Int64_val, caml_copy_int64)
VSL_LENSTYPE(sshort, nint, short, Nativeint_val, caml_copy_nativeint)
VSL_LENSTYPE(ushort, nnat, unsigned short, Nativeint_val, caml_copy_nativeint)
VSL_LENSTYPE(sint,   nint, int, Nativeint_val, caml_copy_nativeint)
VSL_LENSTYPE(uint,   nnat, unsigned int, Nativeint_val, caml_copy_nativeint)
VSL_LENSTYPE(slong,  nint, long, Nativeint_val, caml_copy_nativeint)
VSL_LENSTYPE(ulong,  nnat, unsigned long, Nativeint_val, caml_copy_nativeint)
/*
VSL_LENSTYPE(slonglong,	nint, long long, Nativeint_val, caml_copy_nativeint)
VSL_LENSTYPE(ulonglong,	nnat, unsigned long long, Nativeint_val, caml_copy_nativeint)
*/
VSL_LENSTYPE(ptrdiff_t,	nint, ptrdiff_t, Nativeint_val, caml_copy_nativeint)
VSL_LENSTYPE(size_t,	nnat, size_t, Nativeint_val, caml_copy_nativeint)
VSL_LENSTYPE(intptr_t,	nint, intptr_t, Nativeint_val, caml_copy_nativeint)
VSL_LENSTYPE(uintptr_t,	nnat, uintptr_t, Nativeint_val, caml_copy_nativeint)
/*
VSL_LENSTYPE(sintmax,	nint, intmax_t, Nativeint_val, caml_copy_nativeint)
VSL_LENSTYPE(uintmax,	nnat, uintmax_t, Nativeint_val, caml_copy_nativeint)
*/
