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

include Trie_intf

module Make (Key_element : Map.OrderedType) = struct
    module M = Map.Make (Key_element)

    type 'r key = (Key_element.t, 'r) Sequence.t

    type 'a t = Trie of 'a option * 'a t M.t

    let empty = Trie (None, M.empty)

    let is_empty = function (Trie (None, m)) -> M.is_empty m | _ -> false

    let head (Trie (x, _)) = x

    let rec walk ks trie =
	match Sequence.pop ks with
	| None -> trie
	| Some (k, ks') ->
	    let Trie (_, m) = trie in
	    try walk ks' (M.find k m)
	    with Not_found -> empty

    let find ks trie = head (walk ks trie)

    let rec singleton ks d =
	match Sequence.pop ks with
	| None -> Trie (Some d, M.empty)
	| Some (k, ks') -> Trie (None, M.singleton k (singleton ks' d))

    let rec add ks d' (Trie (d, m)) =
	match Sequence.pop ks with
	| None -> Trie (Some d', m)
	| Some (k, ks') ->
	    let subtrie' =
		try add ks' d' (M.find k m)
		with Not_found -> singleton ks' d' in
	    Trie (d, M.add k subtrie' m)

    let rec remove ks (Trie (d, m)) =
	match Sequence.pop ks with
	| None -> Trie (None, m)
	| Some (k, ks') ->
	    let m' =
		try
		    let subtrie' = remove ks' (M.find k m) in
		    if is_empty subtrie'
		    then M.remove k m
		    else M.add k subtrie' m
		with Not_found -> m in
	    Trie (d, m')

    let rec prefix_fold f ks (Trie (d, m)) accu =
	let accu' = Option.fold (f ks) d accu in
	match Sequence.pop ks with
	| None -> accu'
	| Some (k, ks') ->
	    try prefix_fold f ks' (M.find k m) accu'
	    with Not_found -> accu'

    let rec prefix_optfold f (g, ks) (Trie (d, m)) accu =
	let accu' = f (g, ks) d accu in
	match g ks with
	| None -> accu'
	| Some (k, ks') ->
	    try prefix_optfold f (g, ks') (M.find k m) accu'
	    with Not_found -> accu'
end
