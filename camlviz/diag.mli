(* Copyright 2010--2016  Petter A. Urkedal
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

(** Diagnostic Functions and Exceptions *)

exception Error_at of Textloc.t * string

val errf_at : Textloc.t -> ('b, unit, string, 'a) format4 -> 'b
val warnf_at : Textloc.t -> ('b, unit, string, unit) format4 -> 'b

val dlog_en_for : string -> bool
val dlogf_for : string -> ?loc : Textloc.t
	     -> ('a, unit, string, unit) format4 -> 'a
