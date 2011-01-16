(* Copyright 2011  Petter Urkedal
 *
 * This file is part of Fform/OC <http://www.eideticdew.org/p/fform/>.
 *
 * Fform/OC is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fform/OC is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Fform/OC.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Cst

val extract_term_typing : trm -> trm * trm
(** Assuming the top-level of [u] represents {[v : t]}, [extract_term_typing u]
 ** returns a tuple [(v, t)], else raises Error_at. *)

val extract_cidr_typing : trm -> cidr * trm
(** Assuming the top-level of [u] represents {[v : t]}, where [v] is an
 ** identifier, [extract_idr_typing u] returns the tuple [(v, t)], otherwise
 ** raises Error_at. *)

val move_typing : trm * trm -> trm * trm
(** [move_typing (src, dst)] moves a typing ({[v : t]}) from the top-level of
 ** [src] to the top-level of [dst].  If [src] is not a typing, returns the
 ** pair unchanged. *)

val move_applications : trm * trm -> trm * trm
(** [move_applications (src, dst) recursively takes top-level applications of
 ** [src] and turns them into abstractions around [dst]. *)

val flatten_tycon_application : trm -> cidr * trm list
