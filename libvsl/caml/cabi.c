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

#include <libvsl/cabi.h>
#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <stdint.h>

/* Lens
 * ==== */

typedef struct vsl_lens *vsl_lens_t;
struct vsl_lens
{
    value record;
    void *field;
};

#define VSL_LENS_OF_VALUE(l) ((vsl_lens_t)Data_custom_val(l))
#define VSL_LENSV_PTR(l) VSL_LENS_OF_VALUE(l)->field

static struct custom_operations _lens_ops = {
    "libvsl.lens",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
};

value vsl_lensv_new(value rec, void *field)
{
    CAMLparam1 (rec);
    value v = caml_alloc_custom(&_lens_ops, sizeof(struct vsl_lens), 0, 1);
    vsl_lens_t lens = VSL_LENS_OF_VALUE(v);
    lens->record = rec;
    lens->field = field;
    CAMLreturn (v);
}

value vsl_lensv_of_ptr(void *field)
{
    return vsl_lensv_new(Val_int(0), field);
}

void *vsl_lensv_ptr(value lens)
{
    return VSL_LENSV_PTR(lens);
}

CAMLprim value vsls_lens_get_null(value _)
{
    return vsl_lensv_new(Val_int(0), NULL);
}

CAMLprim value vsls_lens_cmp(value lens0, value lens1)
{
    char *p0 = VSL_LENSV_PTR(lens0);
    char *p1 = VSL_LENSV_PTR(lens1);
    if (p0 < p1) return Val_int(0); /* tprec */
    if (p0 > p1) return Val_int(2); /* tsucc */
    return Val_int(1); /* tcoin */
}

CAMLprim value vsls_lens_focus(value offset_, value lens_)
{
    size_t offset = Long_val(offset_);
    vsl_lens_t lens = Data_custom_val(lens_);
    return vsl_lensv_new(lens->record, (char *)lens->field + offset);
}

CAMLprim value vsls_lens_load_value(value lens)
{
    return *(value *)VSL_LENSV_PTR(lens);
}

CAMLprim value vsls_lens_store_value(value x, value lens)
{
    *(value *)VSL_LENSV_PTR(lens) = x;
    return Val_unit;
}

CAMLprim value vsls_lens_load_ptr(value lens_)
{
    vsl_lens_t lens = VSL_LENS_OF_VALUE(lens_);
    return vsl_lensv_new(lens->record, *(void **)lens->field);
}

CAMLprim value vsls_lens_store_ptr(value x, value lens)
{
    *(void **)VSL_LENSV_PTR(lens) = VSL_LENSV_PTR(x);
    return Val_unit;
}


#define VSL_LENSTYPE(tn, vn, t, of_val, to_val)				\
    CAMLprim value vsls_##tn##_load_##vn(value p)			\
    {									\
	return to_val(*(t *)VSL_LENSV_PTR(p));				\
    }									\
    CAMLprim value vsls_##tn##_store_##vn(value x, value p)		\
    {									\
	*(t *)VSL_LENSV_PTR(p) = of_val(x);				\
	return Val_unit;						\
    }
#include <libvsl/lenstypes.h>
#undef VSL_LENSTYPE


/* Record Allocation
 * ================= */

static struct custom_operations _record_ops = {
    "camlviz.record",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
};

CAMLprim value
vsls_record_alloc(value size)
{
    return caml_alloc_custom(&_record_ops, Long_val(size), 0, 1);
}

static void _finalize_record(value v)
{
    value finalize = *(value *)Data_custom_val(v);
    caml_callback(finalize, v);
}

static struct custom_operations _finalized_record_ops = {
    "camlviz.finalized_record",
    _finalize_record,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
};

CAMLprim value
vsls_record_alloc_finalized(value size_, value finalize)
{
    CAMLparam2 (size_, finalize);
    CAMLlocal1 (v);
    size_t size = Long_val(size_) + sizeof(value);
    v = caml_alloc_custom(&_finalized_record_ops, size, 0, 1);
    caml_modify((value *)Data_custom_val(v), finalize);
    CAMLreturn (v);
}

CAMLprim value
vsls_record_focus(value obj)
{
    if (Custom_ops_val(obj) == &_finalized_record_ops)
	return vsl_lensv_new(obj, (value *)Data_custom_val(obj) + 1);
    else
	return vsl_lensv_new(obj, Data_custom_val(obj));
}
