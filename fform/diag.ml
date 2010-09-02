(* Copyright 2010  Petter Urkedal
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

open FfPervasives

module String_set = Set.Make(String)

let dtags =
    try
	let xs = String.split_on_char ':' (Unix.getenv "FFORM_DTAGS") in
	List.fold String_set.add xs String_set.empty
    with Not_found ->
	String_set.empty

let dlog_en_for tag = String_set.mem tag dtags

let dlogf_for tag ?loc fmt =
    let print msg =
	let puts = output_string stderr in
	Option.iter (fun loc -> puts (Location.to_string loc ^ ": ")) loc;
	puts msg;
	puts "\n" in
    Printf.ksprintf print fmt

