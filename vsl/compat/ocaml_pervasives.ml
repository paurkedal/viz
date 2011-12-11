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

module Float_misc = struct
    let op2_U003c = Pervasives.(<)
    let op2_U003e = Pervasives.(>)
    let op2_U2264 = Pervasives.(<=)
    let op2_U2265 = Pervasives.(>=)
    let cmp = __generic_cmp
    let eq = __generic_eq

    let zero = 0.0
    let one = 1.0
    let minimum = min_float
    let maximum = max_float

    let neg = Pervasives.(~-.)
    let add = Pervasives.(+.)
    let sub = Pervasives.(-.)
    let mul = Pervasives.( *. )
    let div = Pervasives.(/.)

    let itrunc = Pervasives.truncate
    let of_int = Pervasives.float_of_int

    let show x = Utf8_string.as_string (Pervasives.string_of_float x)
end

module Pervasive = struct
    let stdin = Pervasives.stdin
    let stdout = Pervasives.stdout
    let stderr = Pervasives.stderr
    let flush chan = __builtin_effect (fun () -> Pervasives.flush chan)
    let flush_all = __builtin_effect (fun () -> Pervasives.flush_all ())
    let print s = __builtin_effect
	(fun () -> Pervasives.print_string (Utf8_string.of_string s))
    let eprint s = __builtin_effect
	(fun () -> Pervasives.prerr_string (Utf8_string.of_string s))
    let fprint ochan s = __builtin_effect
	(fun () -> Pervasives.output_string ochan (Utf8_string.of_string s))

    let __failure loc msg =
	Printf.eprintf "%s: %s\n" (Utf8_string.of_string loc)
	    (Utf8_string.of_string msg);
	Pervasives.flush_all ();
	assert false
    let __trace loc bindings =
	Printf.eprintf "%s: trace:" (Utf8_string.of_string loc);
	let print_binding (v, x) =
	    Printf.eprintf " %s = %s;" (Utf8_string.of_string v)
				       (Utf8_string.of_string x) in
	List.iter print_binding bindings;
	Printf.eprintf "\n"
end
