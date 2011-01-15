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

let default x0 = function None -> x0 | Some x -> x
let default_opt x0 = function None -> x0 | x -> x
let fold f = function None -> fun x -> x | Some x -> f x
let iter f = function None -> () | Some x -> f x
let map f = function None -> None | Some x -> Some (f x)
let for_all f = function None -> true | Some x -> f x
let exists f = function None -> false | Some x -> f x
let filter f = function None -> None | Some x -> if f x then Some x else None