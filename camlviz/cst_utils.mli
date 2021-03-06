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

(** Utility Functions Operating on the Concrete Syntax Tree *)

open Leaf_types
open Cst_types

val extract_ctrm_coercion : ctrm -> ctrm * ctrm option

val extract_term_typing : ctrm -> ctrm * ctrm
(** Assuming the top-level of [u] represents [v : t], then [extract_term_typing
    u] returns a tuple [(v, t)], else raises Error_at. *)

val extract_cidr_typing : ctrm -> cidr * ctrm
(** Assuming the top-level of [u] represents [v : t], where [v] is an
    identifier, [extract_idr_typing u] returns the tuple [(v, t)], otherwise
    raises Error_at. *)

val extract_term_cname_opt : ctrm -> ctrm * string option

val count_formal_args : ctrm -> int
(** Given a function formal argument list, return the number of arguments.
    Returns 0 when applied to a constructor. *)

val fold_formal_args : (ctrm -> 'a -> 'a) -> ctrm * 'a -> ctrm * 'a
val fold_functor_args : (ctrm -> 'a -> 'a) -> ctrm * 'a -> ctrm * 'a

val is_formal : ctrm -> bool

val formal_idr : ctrm -> idr

val cpred_uses_shadowed : idr -> cpred -> bool

val collect_pattern_vars : ctrm -> idr list

val move_typing : ctrm * ctrm -> ctrm * ctrm
(** [move_typing (src, dst)] moves a typing ([v : t]) from the top-level of
    [src] to the top-level of [dst].  If [src] is not a typing, returns the pair
    unchanged. *)

val move_applications : ctrm * cpred -> ctrm * cpred
(** [move_applications (src, dst)] recursively takes top-level applications of
    [src] and turns them into abstractions around [dst]. *)

val flatten_tycon_application : ctrm -> cidr * ctrm list

val flatten_arrow : ctrm -> ctrm * ctrm list

val fold_on_semicolon : (ctrm -> 'a -> 'a) -> ctrm -> 'a -> 'a
val fold_on_comma : (ctrm -> 'a -> 'a) -> ctrm -> 'a -> 'a

val cpred_is_pure : cpred -> bool
val ctrm_is_pure : ctrm -> bool

val ctrm_is_exception_type : ctrm -> bool

val cpred_if_ctrm : loc -> ctrm -> ctrm -> ctrm -> cpred

val cpred_failure : loc -> ctrm option -> cpred
