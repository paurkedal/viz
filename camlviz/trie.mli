(* Copyright (C) 2010--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

(** A Trie Implementation *)

open FfPervasives

module type S = Trie_intf.S

module Make (Key_element : Map.OrderedType)
    : S with type 'r key = (Key_element.t, 'r) Sequence.t
