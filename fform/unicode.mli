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
 * along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
 *)

open CamomileLibrary.Default.Camomile

module UChar : sig
    include module type of UChar with type t = UChar.t

    val is_idrchr : t -> bool
    val is_space : t -> bool
    val are_tied : t -> t -> bool

    val ch_tab : t
    val ch_nl : t
    val ch_space : t
    val ch_dash : t
    val ch_underscore : t
end

module UString : sig
    include module type of UCS4 with type t = UCS4.t

    val empty : t

    val of_list : UChar.t list -> t

    val of_sequence_n : int -> (UChar.t, 'r) Sequence.t -> t

    val of_utf8 : string -> t
    val to_utf8 : t -> string
end

module UString_sequence : sig
    type t = (UChar.t, UString.t * UString.index * UString.index) Sequence.t

    val create : UString.t -> t

    val length : t -> int
    val position : t -> int
end

module UString_trie : sig
    include Trie.S with type 'r key = (UChar.t, 'r) Sequence.t

    val add_utf8 : string -> 'a -> 'a t -> 'a t
end
