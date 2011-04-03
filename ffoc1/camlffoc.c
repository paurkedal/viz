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

value
ffoc_some(value x)
{
    value xopt;
    xopt = caml_alloc(1, 0);
    Store_field(xopt, 0, x);
    return xopt;
}
