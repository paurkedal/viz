/* Copyright 2010--2011  Petter Urkedal
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
 */

%{
open Cst_types
open Cst_core
open Leaf_types
open Leaf_core
open Unicode

let mkloc lb ub =
    Location.between (Location.Bound.of_lexing_position lb)
		     (Location.Bound.of_lexing_position ub)

let rec quantify loc qs e =
    match qs with
    | [] -> e
    | (l, q, v) :: qs' -> Ctrm_quantify (loc, q, v, quantify l qs' e)
let apply loc f x = Ctrm_apply (loc, f, x)
let apply2 loc f x y = apply loc (apply loc f x) y

let ctrm_idr loc name = Ctrm_ref (Cidr (loc, name), Ih_none)

let apply_infix lb ub lbf ubf (_, f) =
    apply2 (mkloc lb ub) (ctrm_idr (mkloc lbf ubf) f)
let apply_prefix lb ub lbf ubf (f, _) =
    apply (mkloc lb ub) (ctrm_idr (mkloc lbf ubf) f)
let apply_suffix lb ub lbf ubf f =
    apply (mkloc lb ub) (ctrm_idr (mkloc lbf ubf) f)

let apply_infix_script lb ub lbf ubf f =
    apply2 (mkloc lb ub) (ctrm_idr (mkloc lbf ubf) f)
let apply_prefix_script lb ub lbf ubf f =
    apply (mkloc lb ub) (ctrm_idr (mkloc lbf ubf) f)

let apply_fence loc name0 name1 =
    assert (name0 = name1); (* FIXME *)
    apply loc (Ctrm_ref (Cidr (loc, name0), Ih_none))

let cpred_failure loc msg_opt =
    let cloc = Ctrm_literal (loc, Lit_string
		(UString.of_utf8 (Location.to_string loc))) in
    let msg =
	match msg_opt with
	| Some msg -> msg
	| None -> Ctrm_literal (loc, Lit_string
		(UString.of_utf8 "This point should be unreachable.")) in
    let failure = Ctrm_ref (Cidr (loc, Idr "__failure"), Ih_none) in
    Cpred_be (loc, Ctrm_apply (loc, Ctrm_apply (loc, failure, cloc), msg))
%}

%token EOF
%token BEGIN END

%token <Leaf_types.abi> OPEN
%token INCLUDE
%token IN
%token SIG
%token TYPE INJ
%token <Cst_types.cmonad option> LET WHAT WHICH
%token <Leaf_types.val_info> VAL
%token WHERE WITH
%token SKIP ENDSKIP /* Hack for ffoc1pp only. */

%token ASSERT BE FAIL
%token <Cst_types.cmonad> DO WHEN
%token RAISE
%token UPON

%token LEX
%token LEXALIAS
%token LEXOPEN

%token <Leaf_types.idr> LBRACKET RBRACKET
%token IF ELSE OTHERWISE
%token AT
%token DOT

/* Logic Operators */
%token <Leaf_types.idr * Leaf_types.idr> LOGIC0 LOGIC1 LOGIC2 LOGIC3
%token <Leaf_types.idr * Leaf_types.idr> LOGIC4 LOGIC5 LOGIC6 LOGIC7 LOGIC8
%token <Leaf_types.idr> QUANTIFIER
%left  LOGIC0
%right LOGIC1
%left  LOGIC2
%right LOGIC3
%left  LOGIC4
%right LOGIC5
%left  LOGIC6
%right LOGIC7
%left  LOGIC8

/* Relation Operators */
%token <Leaf_types.idr> RELATION JUDGEMENT

/* Arithmetic Operators */
%token <Leaf_types.idr> ARITH0_S ARITH1_S ARITH2_S ARITH3_S
%token <Leaf_types.idr> ARITH4_S ARITH5_S ARITH6_S ARITH7_S
%token <Leaf_types.idr> ARITH8_S ARITH9_S
%token <Leaf_types.idr * Leaf_types.idr> ARITH0 ARITH1 ARITH2 ARITH3
%token <Leaf_types.idr * Leaf_types.idr> ARITH4 ARITH5 ARITH6 ARITH7
%token <Leaf_types.idr * Leaf_types.idr> ARITH8 ARITH9
%token <Leaf_types.idr> LABEL FENCE
%token <Leaf_types.idr> SCRIPT0_P SCRIPT0_S SCRIPT0_I
%token <Leaf_types.idr> SCRIPT1_P SCRIPT1_S SCRIPT1_I
%token <Leaf_types.idr> SCRIPT2_P SCRIPT2_S SCRIPT2_I PROJECT_LBRACKET
%token <Leaf_types.idr> PROJECT

%left  ARITH0 ARITH0_S
%right ARITH1 ARITH1_S
%left  ARITH2 ARITH2_S
%right ARITH3 ARITH3_S LABEL
%left  ARITH4 ARITH4_S
%right ARITH5 ARITH5_S
%left  ARITH6 ARITH6_S
%right ARITH7 ARITH7_S
%left  ARITH8 ARITH8_S
%right ARITH9 ARITH9_S
%left  SCRIPT0_P SCRIPT0_S SCRIPT0_I
%right SCRIPT1_P SCRIPT1_S SCRIPT1_I
%left  SCRIPT2_P SCRIPT2_S SCRIPT2_I

/* Atoms */
%token <Leaf_types.lit> LITERAL
%token <Leaf_types.idr> IDENTIFIER
%token <Leaf_types.idr * Cst_types.idrhint> HINTED_IDENTIFIER
%token <Cst_types.cdef> PREPARED_DEF

%type <Cst_types.ctrm> main
%type <Cst_types.ctrm> expr
%type <Cst_types.cpred> predicate_block
%start main
%%

main:
    EOF { Ctrm_where (mkloc $startpos $endpos, []) }
  | structure_block EOF { $1 }
  | expr EOF { $1 }
  /* The following tokens are only used internally by the lexer.  We add them
     only to silence Menhir. */
  | LEX | LEXALIAS | LEXOPEN { assert false }
  ;

signature_block:
    BEGIN signature_clause_seq END {
	let body_loc = mkloc $startpos($2) $endpos($2) in
	Ctrm_with (body_loc, None, List.rev $2)
    }
  ;
signature_clause_seq:
    /* empty */ { [] }
  | signature_clause_seq signature_clause { $2 :: $1 }
  ;
structure_block:
    BEGIN structure_clause_seq END {
	let body_loc = mkloc $startpos($2) $endpos($2) in
	Ctrm_where (body_loc, List.rev $2)
    }
  ;
structure_clause_seq:
    /* empty */ { [] }
  | structure_clause_seq structure_clause { $2 :: $1 }
  | structure_clause_seq SKIP structure_clause_seq ENDSKIP { $1 }
  ;

signature_clause:
    modular_clause { $1 }
  | IN structure_pattern signature_block
    { Cdef_in (mkloc $startpos $endpos, $2, $3) }
  ;

structure_clause:
    modular_clause { $1 }
  | IN structure_pattern structure_block
    { Cdef_in (mkloc $startpos $endpos, $2, $3) }
  | LET term_pattern predicate_block
    { Cdef_let (mkloc $startpos $endpos, $1, $2, $3) }
  ;

modular_clause:
    OPEN projection
    { Cdef_open (mkloc $startpos $endpos, $1, $2) }
  | INCLUDE expr
    { Cdef_include (mkloc $startpos $endpos, $2) }
  | SIG identifier
    { Cdec_sig (mkloc $startpos $endpos, $2) }
  | SIG identifier signature_block
    { Cdef_sig (mkloc $startpos $endpos, $2, $3) }
  | TYPE type_equation
    { Cdef_type (mkloc $startpos $endpos, $2) }
  | INJ term_pattern
    { Cdef_inj (mkloc $startpos $endpos, $2) }
  | VAL term_pattern
    { Cdef_val (mkloc $startpos $endpos, $1, $2) }
  | PREPARED_DEF { $1 }
  ;

type_equation: expr {$1};
term_pattern: expr {$1};
structure_pattern: expr {$1};
term: expr {$1};


/* Predicates */

predicate_block: BEGIN participle_seq compound_predicate END { $2 $3 };
atomic_predicate:
    BE term { Cpred_be (mkloc $startpos $endpos, $2) }
  | FAIL { cpred_failure (mkloc $startpos $endpos) None }
  | FAIL term { cpred_failure (mkloc $startpos $endpos) (Some $2) }
  | RAISE term { Cpred_raise (mkloc $startpos $endpos, $2) }
  ;
compound_predicate:
    nonfunction_predicate { $1 }
  | at_predicate { Cpred_at (mkloc $startpos $endpos, $1) }
  ;
nonfunction_predicate:
    atomic_predicate { $1 }
  | ASSERT term nonfunction_predicate_with_participle
    { Cpred_assert (mkloc $startpos $endpos, $2, $3) }
  | atomic_predicate WHICH predicate_block
    { let that = Cidr (mkloc $startpos($2) $endpos($2), Idr "that") in
      let that_trm = Ctrm_ref (that, Ih_none) in
      Cpred_let (mkloc $startpos $endpos, $2, that_trm, $3, $1) }
  | postif_predicate { $1 }
  | if_predicate { $1 }
  | do_predicate { $1 }
  ;
nonfunction_predicate_with_participle:
    nonfunction_predicate { $1 }
  | participle nonfunction_predicate_with_participle { $1 $2 }
  ;
if_predicate:
    IF term predicate_block if_predicate_cont
    { Cpred_if (mkloc $startpos $endpos, $2, $3, $4) }
if_predicate_cont:
    nonfunction_predicate_with_participle { $1 }
  | ELSE predicate_block { $2 }
  ;
at_predicate:
    AT term_pattern predicate_block
    { [($2, $3)] }
  | AT term_pattern predicate_block at_predicate
    { ($2, $3) :: $4 }
  ;
postif_predicate:
    atomic_predicate BEGIN IF term END postif_predicate
    { Cpred_if (mkloc $startpos $endpos, $4, $1, $6) }
  | atomic_predicate BEGIN OTHERWISE END { $1 }
  ;
do_predicate:
    DO expr
    { Cpred_do1 (mkloc $startpos $endpos, $1, $2) }
  | DO expr nonfunction_predicate_with_participle
    { Cpred_do2 (mkloc $startpos $endpos, $1, $2, $3) }
  | WHEN expr predicate_block
    {
	let loc = mkloc $startpos $endpos in
	Cpred_if (loc, $2, $3, Cpred_be (loc, Ctrm_literal (loc, Lit_unit)))
    }
  | WHEN expr predicate_block nonfunction_predicate_with_participle
    {
	let loc = mkloc $startpos $endpos in
	let m = Cpred_if (loc, $2, $3,
		    Cpred_be (loc, Ctrm_literal (loc, Lit_unit))) in
	Cpred_do2 (loc, $1, Ctrm_what (loc, Some $1, m), $4)
    }
  ;

/* Participles */

participle_seq:
    /* empty */ { fun x -> x }
  | participle_seq participle { fun x -> $1 ($2 x) }
  ;
participle:
    LET term_pattern predicate_block
    { fun x -> Cpred_let (mkloc $startpos $endpos, $1, $2, $3, x) }
  ;

/* Expressions */

expr: judgement_expr {$1};

judgement_expr:
    qlogic_expr { $1 }
  | qlogic_expr judgement_seq { Ctrm_rel (mkloc $startpos $endpos, $1, $2) }
  ;
judgement_seq:
    judgement_comp { [$1] }
  | judgement_comp judgement_seq { $1 :: $2 }
  ;
judgement_comp:
    JUDGEMENT qlogic_expr
    { (mkloc $startpos $endpos,
       Cidr (mkloc $startpos($1) $endpos($1), $1), $2) }
  ;

qlogic_expr:
    qseq logic_expr { quantify (mkloc $startpos $endpos) $1 $2 }
  ;
/* Replacing (qseq logic_expr) with (qlogic_expr) below causes shift/reduce
 * conflicts unless inlined, and positions are not supported when inlining.
 * Thus, the verbosity. */
logic_expr:
    relational_expr { $1 }
  | logic_expr LOGIC0 qseq logic_expr
    { let rhs = quantify (mkloc $startpos($3) $endpos($4)) $3 $4 in
      apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 rhs }
  | LOGIC0 qseq logic_expr
    { let rhs = quantify (mkloc $startpos($2) $endpos($3)) $2 $3 in
      apply_prefix $startpos $endpos $startpos($1) $endpos($1) $1 rhs }

  | logic_expr LOGIC1 logic_expr
    { apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 $3 }
  | logic_expr LOGIC1 nonempty_qseq logic_expr
    { let rhs = quantify (mkloc $startpos($3) $endpos($4)) $3 $4 in
      apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 rhs }
  | logic_expr LOGIC1
    { apply_suffix $startpos $endpos $startpos($2) $endpos($2) (fst $2) $1 }

  | logic_expr LOGIC2 qseq logic_expr
    { let rhs = quantify (mkloc $startpos($3) $endpos($4)) $3 $4 in
      apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 rhs }
  | LOGIC2 qseq logic_expr
    { let rhs = quantify (mkloc $startpos($2) $endpos($3)) $2 $3 in
      apply_prefix $startpos $endpos $startpos($1) $endpos($1) $1 rhs }

  | logic_expr LOGIC3 logic_expr
    { apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 $3 }
  | logic_expr LOGIC3 nonempty_qseq logic_expr
    { let rhs = quantify (mkloc $startpos($3) $endpos($4)) $3 $4 in
      apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 rhs }
  | logic_expr LOGIC3
    { apply_suffix $startpos $endpos $startpos($2) $endpos($2) (fst $2) $1 }

  | logic_expr LOGIC4 qseq logic_expr
    { let rhs = quantify (mkloc $startpos($3) $endpos($4)) $3 $4 in
      apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 rhs }
  | LOGIC4 qseq logic_expr
    { let rhs = quantify (mkloc $startpos($2) $endpos($3)) $2 $3 in
      apply_prefix $startpos $endpos $startpos($1) $endpos($1) $1 rhs }

  | logic_expr LOGIC5 logic_expr
    { apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 $3 }
  | logic_expr LOGIC5 nonempty_qseq logic_expr
    { let rhs = quantify (mkloc $startpos($3) $endpos($4)) $3 $4 in
      apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 rhs }
  | logic_expr LOGIC5
    { apply_suffix $startpos $endpos $startpos($2) $endpos($2) (fst $2) $1 }

  | logic_expr LOGIC6 qseq logic_expr
    { let rhs = quantify (mkloc $startpos($3) $endpos($4)) $3 $4 in
      apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 rhs }
  | LOGIC6 qseq logic_expr
    { let rhs = quantify (mkloc $startpos($2) $endpos($3)) $2 $3 in
      apply_prefix $startpos $endpos $startpos($1) $endpos($1) $1 rhs }

  | logic_expr LOGIC7 logic_expr
    { apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 $3 }
  | logic_expr LOGIC7 nonempty_qseq logic_expr
    { let rhs = quantify (mkloc $startpos($3) $endpos($4)) $3 $4 in
      apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 rhs }
  | logic_expr LOGIC7
    { apply_suffix $startpos $endpos $startpos($2) $endpos($2) (fst $2) $1 }

  | logic_expr LOGIC8 qseq logic_expr
    { let rhs = quantify (mkloc $startpos($3) $endpos($4)) $3 $4 in
      apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 rhs }
  | LOGIC8 qseq logic_expr
    { let rhs = quantify (mkloc $startpos($2) $endpos($3)) $2 $3 in
      apply_prefix $startpos $endpos $startpos($1) $endpos($1) $1 rhs }
  ;

qseq:
    /* empty */ { [] }
  | qseq quantifier { $2 :: $1 }
  ;
nonempty_qseq:
    quantifier { [$1] }
  | nonempty_qseq quantifier { $2 :: $1 }
  ;
quantifier:
    QUANTIFIER expr DOT
    { (mkloc $startpos $endpos,
       Cidr (mkloc $startpos($1) $endpos($1), idr_1q $1), $2) }
  ;

relational_expr:
    arith { $1 }
  | arith relation_seq { Ctrm_rel (mkloc $startpos $endpos, $1, $2) }
  ;
relation_seq:
    relation_comp { [$1] }
  | relation_comp relation_seq { $1 :: $2 }
  ;
relation_comp:
    RELATION arith
    { (mkloc $startpos $endpos,
       Cidr (mkloc $startpos($1) $endpos($1), $1), $2) }
  ;
arith:
    application
    { $1 }
  | ARITH0 arith
    { apply_prefix $startpos $endpos $startpos($1) $endpos($1) $1 $2 }
  | arith ARITH0_S
    { apply_suffix $startpos $endpos $startpos($2) $endpos($2) $2 $1 }
  | arith ARITH0 arith
    { apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 $3 }
  | ARITH1 arith
    { apply_prefix $startpos $endpos $startpos($1) $endpos($1) $1 $2 }
  | arith ARITH1_S
    { apply_suffix $startpos $endpos $startpos($2) $endpos($2) $2 $1 }
  | arith ARITH1 arith
    { apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 $3 }
  | ARITH2 arith
    { apply_prefix $startpos $endpos $startpos($1) $endpos($1) $1 $2 }
  | arith ARITH2_S
    { apply_suffix $startpos $endpos $startpos($2) $endpos($2) $2 $1 }
  | arith ARITH2 arith
    { apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 $3 }
  | LABEL  arith
    { let label = Cidr (mkloc $startpos($1) $endpos($1), $1) in
      Ctrm_label (mkloc $startpos $endpos, label, $2) }
  | ARITH3 arith
    { apply_prefix $startpos $endpos $startpos($1) $endpos($1) $1 $2 }
  | arith ARITH3_S
    { apply_suffix $startpos $endpos $startpos($2) $endpos($2) $2 $1 }
  | arith ARITH3 arith
    { apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 $3 }
  | ARITH4 arith
    { apply_prefix $startpos $endpos $startpos($1) $endpos($1) $1 $2 }
  | arith ARITH4_S
    { apply_suffix $startpos $endpos $startpos($2) $endpos($2) $2 $1 }
  | arith ARITH4 arith
    { apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 $3 }
  | ARITH5 arith
    { apply_prefix $startpos $endpos $startpos($1) $endpos($1) $1 $2 }
  | arith ARITH5_S
    { apply_suffix $startpos $endpos $startpos($2) $endpos($2) $2 $1 }
  | arith ARITH5 arith
    { apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 $3 }
  | ARITH6 arith
    { apply_prefix $startpos $endpos $startpos($1) $endpos($1) $1 $2 }
  | arith ARITH6_S
    { apply_suffix $startpos $endpos $startpos($2) $endpos($2) $2 $1 }
  | arith ARITH6 arith
    { apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 $3 }
  | ARITH7 arith
    { apply_prefix $startpos $endpos $startpos($1) $endpos($1) $1 $2 }
  | arith ARITH7_S
    { apply_suffix $startpos $endpos $startpos($2) $endpos($2) $2 $1 }
  | arith ARITH7 arith
    { apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 $3 }
  | ARITH8 arith
    { apply_prefix $startpos $endpos $startpos($1) $endpos($1) $1 $2 }
  | arith ARITH8_S
    { apply_suffix $startpos $endpos $startpos($2) $endpos($2) $2 $1 }
  | arith ARITH8 arith
    { apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 $3 }
  | ARITH9 arith
    { apply_prefix $startpos $endpos $startpos($1) $endpos($1) $1 $2 }
  | arith ARITH9_S
    { apply_suffix $startpos $endpos $startpos($2) $endpos($2) $2 $1 }
  | arith ARITH9 arith
    { apply_infix $startpos $endpos $startpos($2) $endpos($2) $2 $1 $3 }
  ;
application:
    script { $1 }
  | application script { apply (mkloc $startpos $endpos) $1 $2 }
  | application LABEL script
    {
	let loc = mkloc $startpos $endpos in
	let label = Cidr (mkloc $startpos($2) $endpos($2), $2) in
	apply loc $1 (Ctrm_label (loc, label, $3))
    }
  | FENCE arith FENCE { apply_fence (mkloc $startpos $endpos) $1 $3 $2 }
  ;
script:
    projection
    { $1 }
  | SCRIPT0_P script
    { apply_prefix_script $startpos $endpos $startpos($1) $endpos($1) $1 $2 }
  | script SCRIPT0_S
    { apply_suffix $startpos $endpos $startpos($2) $endpos($2) $2 $1 }
  | script SCRIPT0_I script
    { apply_infix_script $startpos $endpos $startpos($2) $endpos($2) $2 $1 $3 }
  | SCRIPT1_P script
    { apply_prefix_script $startpos $endpos $startpos($1) $endpos($1) $1 $2 }
  | script SCRIPT1_S
    { apply_suffix $startpos $endpos $startpos($2) $endpos($2) $2 $1 }
  | script SCRIPT1_I script
    { apply_infix_script $startpos $endpos $startpos($2) $endpos($2) $2 $1 $3 }
  | SCRIPT2_P script
    { apply_prefix_script $startpos $endpos $startpos($1) $endpos($1) $1 $2 }
  | script SCRIPT2_S
    { apply_suffix $startpos $endpos $startpos($2) $endpos($2) $2 $1 }
  | script SCRIPT2_I script
    { apply_infix_script $startpos $endpos $startpos($2) $endpos($2) $2 $1 $3 }
  ;

projection:
    atomic_expr { $1 }
  | projection PROJECT
    { let p = Cidr (mkloc $startpos($2) $endpos($2), $2) in
      Ctrm_project (mkloc $startpos $endpos, p, $1) }
  | projection PROJECT_LBRACKET expr RBRACKET
    {
	let locb = mkloc $startpos($2) $endpos($2) in
	let f = Ctrm_ref (Cidr (locb, idr_2b $2 $4), Ih_none) in
	let loc = mkloc $startpos $endpos in
	let loc1 = mkloc $startpos($1) $endpos($2) in
	Ctrm_apply (loc, Ctrm_apply (loc1, f, $1), $3)
    }
  ;

atomic_expr:
    identifier { Ctrm_ref ($1, Ih_none) }
  | HINTED_IDENTIFIER
    { let idr, hint = $1 in
      Ctrm_ref (Cidr (mkloc $startpos $endpos, idr), hint) }
  | LITERAL { Ctrm_literal (mkloc $startpos $endpos, $1) }
  | LBRACKET parenthesised RBRACKET
    {
	let locb = mkloc $startpos $endpos($1) in
	let f = Ctrm_ref (Cidr (locb, idr_1b $1 $3), Ih_none) in
	Ctrm_apply (mkloc $startpos $endpos, f, $2)
    }
  | WHERE structure_block { $2 }
  | WITH signature_block { $2 }
  | WHAT predicate_block { Ctrm_what (mkloc $startpos $endpos, $1, $2) }
  ;
parenthesised:
    /* empty */ { Ctrm_literal (mkloc $startpos $endpos, Lit_unit) }
  | expr { $1 }
  ;

identifier: IDENTIFIER { Cidr (mkloc $startpos $endpos, $1) };
