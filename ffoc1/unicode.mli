(* Copyright 2010--2011  Petter Urkedal
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

(** Some Additions to the Camomile Unicode Library *)

open CamomileLibrary.Default.Camomile
open Sexplib

module UChar : sig
    include module type of UChar with type t = UChar.t

    val is_idrchr : t -> bool
    val is_space : t -> bool
    val is_hspace : t -> bool
    val are_tied : t -> t -> bool

    val is_ascii_digit : t -> bool
    val is_ascii_lower : t -> bool
    val is_ascii_upper : t -> bool
    val is_ascii_alpha : t -> bool
    val is_ascii_alnum : t -> bool

    val is_greek_lower : t -> bool
    val is_greek_upper : t -> bool
    val is_greek_alpha : t -> bool

    val is_ocaml_idrfst : t -> bool
    val is_ocaml_idrcnt : t -> bool

    val ch_tab : t
    val ch_nl : t
    val ch_space : t
    val ch_dash : t
    val ch_underscore : t
    val ch_grave_accent : t

    val to_utf8 : t -> string
end

module UChar_map : Map.S with type key = UChar.t

module UString : sig
    include module type of UText with type t = UText.t

    val empty : t

    val after : int -> t -> t

    val of_list : UChar.t list -> t

    val of_sequence_n : int -> (UChar.t, 'r) Sequence.t -> t

    val of_utf8 : string -> t
    val to_utf8 : t -> string

    val t_of_sexp : Sexp.t -> t
    val sexp_of_t : t -> Sexp.t
end

module UString_sequence : sig
    type t = (UChar.t, UString.t * UString.index * UString.index) Sequence.t

    val create : UString.t -> t
end

module UString_trie : sig
    include Trie.S with type 'r key = (UChar.t, 'r) Sequence.t

    val add_utf8 : string -> 'a -> 'a t -> 'a t
end
