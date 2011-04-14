(* Copyright 2010--2011  Petter Urkedal
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

(** A Pretty-Printing Utility *)

type tag = [`Error | `Keyword | `Label | `Literal | `Name | `Operator]

type t

val create : ?indent : int
    -> ?enter : (t -> tag -> unit)
    -> ?leave : (t -> tag -> unit)
    -> unit -> t

val contents : t -> string

val add_indent : t -> int -> unit

val enter : t -> tag -> unit

val leave : t -> tag -> unit

val enter_indent : t -> unit

val leave_indent : t -> unit

val put_char : t -> char -> unit

val put_string : t -> string -> unit

val newline : t -> unit

val space : t -> unit

val put : t -> tag -> string -> unit

(** A shortcut to format a binary operator.  This ensures a space before and
 ** after the operator. *)
val put_op : t -> string -> unit

(** A shortcut to format a keyword.  This ensures that there is whitespace
 ** before and after the keyword. *)
val put_kw : t -> string -> unit
