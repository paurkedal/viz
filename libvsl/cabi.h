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

#ifndef LIBVSL_LENS_H
#define LIBVSL_LENS_H

#include <libvsl/prereq.h>
#include <stddef.h>

VSL_BEGIN_DECLS

vsl_value vsl_lensv_new(vsl_value rec, void *field);
vsl_value vsl_lensv_of_ptr(void *field);

void *vsl_lensv_ptr(vsl_value lens);

vsl_value vsls_lens_focus(vsl_value offset, vsl_value lens);
vsl_value vsls_lens_load_value(vsl_value lens);
vsl_value vsls_lens_store_value(vsl_value v, vsl_value lens);
vsl_value vsls_lens_load_ptr(vsl_value lens);
vsl_value vsls_lens_store_ptr(vsl_value v, vsl_value lens);
#define VSL_LENSTYPE(tn, vn, t, of_val, to_val) \
    vsl_value vsls_##tn##_load_##vn(vsl_value); \
    vsl_value vsls_##tn##_store_##vn(vsl_value, vsl_value);
#include <libvsl/lenstypes.h>
#undef VSL_LENSTYPE

vsl_value vsls_record_alloc(vsl_value size);
vsl_value vsls_record_alloc_finalized(vsl_value size, vsl_value finalize);
vsl_value vsls_record_focus(vsl_value v);

VSL_END_DECLS

#endif
