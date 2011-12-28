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

#ifndef LIBVSL_PREREQ_H
#define LIBVSL_PREREQ_H

#ifdef __cplusplus
#  define VSL_BEGIN_DECLS extern "C" {
#  define VSL_END_DECLS }
#else
#  define VSL_BEGIN_DECLS
#  define VSL_END_DECLS
#endif

#if defined(VSL_HAVE_C99_INLINE)
#  define VSL_INLINE inline
#elif defined(VSL_HAVE_GNU89_INLINE)
#  define VSL_INLINE extern inline
#else
#  define VSL_INLINE static inline
#endif

#define LIBVSL_IN_PREREQ
#include <libvsl/caml/prereq.h>
#undef LIBVSL_IN_PREREQ

#define VSL_MIN(x, y) ((x) <= (y)? (x) : (y))
#define vsl_malloc malloc
#define vsl_mfree free

#endif
