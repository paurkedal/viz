(* Copyright (C) 2011--2016  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This file is part of the Viz Compiler <http://www.vizlang.org/>.
 *
 * The Viz Compiler is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 *
 * The Viz Compiler is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with the Viz Compiler.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Leaf_types
open Cst_types
open Cst_core

type 'a rewriter = {
  rw_cpred : 'a rewriter -> stratum -> cpred * 'a -> cpred * 'a;
  rw_ctrm  : 'a rewriter -> stratum -> ctrm  * 'a -> ctrm  * 'a;
  rw_cdef  : 'a rewriter -> stratum -> cdef  * 'a -> cdef  * 'a;
}

val subterm_rewrite_cpred : 'a rewriter -> stratum -> cpred * 'a -> cpred * 'a
val subterm_rewrite_ctrm : 'a rewriter -> stratum -> ctrm * 'a -> ctrm * 'a
val subterm_rewrite_cdef : 'a rewriter -> stratum -> cdef * 'a -> cdef * 'a

val default_rewrite_cpred : 'a rewriter -> stratum -> cpred * 'a -> cpred * 'a
val default_rewrite_ctrm : 'a rewriter -> stratum -> ctrm * 'a -> ctrm * 'a
val default_rewrite_cdef : 'a rewriter -> stratum -> cdef * 'a -> cdef * 'a

val default_rewriter : 'a rewriter

val rewrite_ctrm : stratum -> ctrm -> ctrm
