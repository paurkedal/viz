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

open FfPervasives
open Printf

exception Error_at of Location.t * string

let errf_at loc msg = ksprintf (fun s -> raise (Error_at (loc, s))) msg

let warnf_at loc msg =
    ksprintf (fun s -> eprintf "%s: %s\n" (Location.to_string loc) s) msg

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
    ksprintf print fmt

