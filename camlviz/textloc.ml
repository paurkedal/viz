(* Copyright (C) 2010--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Lexing
open CamomileLibraryDefault.Camomile.UPervasives
open Prereq

let min_lnum, min_cnum =
  try
    let s = Unix.getenv "VIZ_LOCATION_ORIGIN" in
    begin match String.split_on_char ',' s with
      | [sl; sc] -> int_of_string sl, int_of_string sc
      | _ -> (1, 0)
    end
  with Not_found | Failure _ -> (1, 0)

let string_of_lineno lnum = string_of_int (min_lnum + lnum)
let string_of_colno cnum = string_of_int (min_cnum + cnum)

module Bound = struct
  type t = position * int

  let init p = {pos_fname = p; pos_lnum = 0; pos_cnum = 0; pos_bol = 0}, 0

  let init_lc p l c =
      {pos_fname = p; pos_lnum = l; pos_cnum = c; pos_bol = 0}, c

  let dummy = (dummy_pos, -1)

  let path (b, _) = b.pos_fname
  let lineno (b, _) = b.pos_lnum
  let charno (b, _) = b.pos_cnum
  let bol_charno (b, _) = b.pos_bol
  let column (_, c) = c

  let skip_n n (pos, col) =
    {pos with pos_cnum = pos.pos_cnum + n},
    (if col = -1 then -1 else col + n)

  let skip_tab (pos, col) =
    {pos with pos_cnum = pos.pos_cnum + 1},
    (if col = -1 then -1 else (col / 8 + 1) * 8)

  let skip_newline (pos, col) =
    {pos with
        pos_lnum = pos.pos_lnum + 1;
        pos_cnum = pos.pos_cnum + 1;
        pos_bol  = pos.pos_cnum + 1;
    },
    (if col = -1 then -1 else 0)

  let skip_char ch =
    match int_of_uchar ch with
    | 0x9 -> skip_tab
    | 0xa -> skip_newline
    | _   -> skip_n 1

  let compare (pos, col) (pos', col') =
    let cmp_fname = String.compare pos.pos_fname pos'.pos_fname in
    if cmp_fname <> 0 then cmp_fname else
    if pos.pos_lnum < pos'.pos_lnum then -1 else
    if pos.pos_lnum > pos'.pos_lnum then  1 else
    if pos.pos_cnum < pos'.pos_cnum then -1 else
    if pos.pos_cnum > pos'.pos_cnum then  1 else
    0

  let min b b' = if compare b b' <= 0 then b else b'
  let max b b' = if compare b b' >  0 then b else b'

  let to_string (pos, _) =
    let buf = Buffer.create 8 in
    Buffer.add_string buf pos.pos_fname;
    Buffer.add_char buf ':';
    Buffer.add_string buf (string_of_lineno pos.pos_lnum);
    Buffer.add_char buf ':';
    Buffer.add_string buf (string_of_colno (pos.pos_cnum - pos.pos_bol));
    Buffer.contents buf

  let of_lexing_position pos = (pos, -1)

  let to_lexing_position (pos, _) = pos
end

type t = {
  loc_fname : string;
  loc_lb_lnum : int;
  loc_lb_bol : int;
  loc_lb_cnum : int;
  loc_lb_col : int;
  loc_ub_lnum : int;
  loc_ub_bol : int;
  loc_ub_cnum : int;
  loc_ub_col : int;
}

let between (lb, lbcol) (ub, ubcol) =
  assert (lb.pos_fname = ub.pos_fname);
  {
    loc_fname = lb.pos_fname;
    loc_lb_lnum = lb.pos_lnum;
    loc_lb_bol = lb.pos_bol;
    loc_lb_cnum = lb.pos_cnum;
    loc_lb_col = lbcol;
    loc_ub_lnum = ub.pos_lnum;
    loc_ub_bol = ub.pos_bol;
    loc_ub_cnum = ub.pos_cnum;
    loc_ub_col = ubcol;
  }

let at locb = between locb locb

let dummy = between Bound.dummy Bound.dummy

let path loc = loc.loc_fname

let lbound loc =
  {
    pos_fname = loc.loc_fname;
    pos_lnum = loc.loc_lb_lnum;
    pos_bol = loc.loc_lb_bol;
    pos_cnum = loc.loc_lb_cnum;
  }, loc.loc_lb_col

let ubound loc =
  {
    pos_fname = loc.loc_fname;
    pos_lnum = loc.loc_ub_lnum;
    pos_bol = loc.loc_ub_bol;
    pos_cnum = loc.loc_ub_cnum;
  }, loc.loc_ub_col

let span = function
  | [] -> invalid_arg "Textloc.span"
  | loc :: locs ->
    let rec loop lb ub = function
      | [] -> between lb ub
      | loc :: locs ->
        loop (Bound.min lb (lbound loc))
             (Bound.max ub (ubound loc)) locs in
    loop (lbound loc) (ubound loc) locs

let to_string loc =
  let buf = Buffer.create 8 in
  Buffer.add_string buf loc.loc_fname;
  Buffer.add_char buf ':';
  Buffer.add_string buf (string_of_lineno loc.loc_lb_lnum);
  Buffer.add_char buf ',';
  Buffer.add_string buf (string_of_colno (loc.loc_lb_cnum - loc.loc_lb_bol));
  let same_lnum = loc.loc_lb_lnum = loc.loc_ub_lnum in
  let same_cnum = loc.loc_lb_cnum = loc.loc_ub_cnum in
  if not same_lnum || not same_cnum then begin
    Buffer.add_char buf '-';
    if not same_lnum then begin
      Buffer.add_string buf (string_of_lineno loc.loc_ub_lnum);
      Buffer.add_char buf ','
    end;
    Buffer.add_string buf (string_of_colno (loc.loc_ub_cnum - loc.loc_ub_bol))
  end;
  Buffer.contents buf
