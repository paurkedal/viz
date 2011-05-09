(* Copyright 2011  Petter Urkedal
 *
 * This file is part of the Viz Standard Library <http://www.vizlang.org/>.
 *
 * The Viz Standard Library (VSL) is free software: you can redistribute it
 * and/or modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * The VSL is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with the VSL.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Ocaml_prereq
open Ocaml_unicode

module Bool_ops = struct
    let op2_U003d = Pervasives.(=)
    let op2_U2260 = Pervasives.(<>)
    let op2_U003c = Pervasives.(<)
    let op2_U003e = Pervasives.(>)
    let op2_U2264 = Pervasives.(<=)
    let op2_U2265 = Pervasives.(>=)
end

module Int_misc = struct
    let show i = UTF8string.as_string (Pervasives.string_of_int i)
end

module Pervasive = struct
    let stdin = Pervasives.stdin
    let stdout = Pervasives.stdout
    let stderr = Pervasives.stderr
    let flush chan = __unsafe_action (fun () -> Pervasives.flush chan)
    let flush_all = __unsafe_action (fun () -> Pervasives.flush_all ())
    let print s = __unsafe_action
	(fun () -> Pervasives.print_string (UTF8string.of_string s))
    let eprint s = __unsafe_action
	(fun () -> Pervasives.prerr_string (UTF8string.of_string s))
    let fprint ochan s = __unsafe_action
	(fun () -> Pervasives.output_string ochan (UTF8string.of_string s))

    let __failure loc msg =
	Printf.eprintf "%s: %s\n" (UTF8string.of_string loc)
	    (UTF8string.of_string msg);
	Pervasives.flush_all ();
	assert false
    let __trace loc bindings =
	Printf.eprintf "%s: trace:" (UTF8string.of_string loc);
	let print_binding (v, x) =
	    Printf.eprintf " %s = %s;" (UTF8string.of_string v)
				       (UTF8string.of_string x) in
	List.iter print_binding bindings;
	Printf.eprintf "\n"
end
