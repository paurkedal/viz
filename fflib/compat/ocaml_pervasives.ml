(* Copyright 2011  Petter Urkedal
 *
 * This file is part of the Fform Standard Library.
 *
 * The Fform Standard Library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * Fform is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Fform.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Ocaml_prereq
open Ocaml_unicode

module Bool_ops = struct
    let op1_U00ac = Pervasives.not
    let op2_U2227 = Pervasives.( && )
    let op2_U2228 = Pervasives.( || )

    let op2_U003d = Pervasives.(=)
    let op2_U2260 = Pervasives.(<>)
    let op2_U003c = Pervasives.(<)
    let op2_U003e = Pervasives.(>)
    let op2_U2264 = Pervasives.(<=)
    let op2_U2265 = Pervasives.(>=)
end

module Int_ops = struct
    let op1_U2212 = Pervasives.(~-)
    let op2_U002b = Pervasives.(+)
    let op2_U2212 = Pervasives.(-)
    let op2_U002a = Pervasives.( * )
    let op2_U002f = Pervasives.(/)
    let op2_mod = Pervasives.(mod)

    let bitand = Pervasives.(land)
    let bitor = Pervasives.(lor)
    let bitxor = Pervasives.(lxor)
    let bitlsl = Pervasives.(lsl)
    let bitlsr = Pervasives.(lsr)
    let bitasr = Pervasives.(asr)
end

module Int_misc = struct
    let show i = String_.of_utf8 (Pervasives.string_of_int i)
end

module Pervasive = struct
    let print s = __unsafe_action
	(fun () -> Pervasives.print_string (String_.as_utf8 s))
    let eprint s = __unsafe_action
	(fun () -> Pervasives.prerr_string (String_.as_utf8 s))
    let fprint ochan s = __unsafe_action
	(fun () -> Pervasives.output_string ochan (String_.as_utf8 s))

    let __failure loc msg =
	Printf.eprintf "%s: %s\n" (String_.as_utf8 loc) (String_.as_utf8 msg);
	assert false
end
