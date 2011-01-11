/* Copyright 2010  Petter Urkedal
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
let rec quantify loc qs e =
    match qs with
    | [] -> e
    | (l, q, v) :: qs' -> Input.Trm_quantify (loc, q, v, quantify l qs' e)
let apply loc f x = Input.Trm_apply (loc, f, x)
let apply2 loc f x y = apply loc (apply loc f x) y

let trm_1o loc name = Input.Trm_ref (loc, Input.idr_1o name, Input.Ih_none)
let trm_2o loc name = Input.Trm_ref (loc, Input.idr_2o name, Input.Ih_none)

let apply_infix  loc name = apply2 loc (trm_2o loc name)
let apply_prefix loc name = apply  loc (trm_1o loc name)
let apply_suffix loc name = apply  loc (trm_1o loc name)

let apply_prefixq loc name (qx, x) =
    apply  loc (trm_1o loc name) (quantify loc qx x)
let apply_infixq loc name x (qy, y) =
    apply2 loc (trm_2o loc name) x (quantify loc qy y)

let apply_fence loc name0 name1 =
    assert (name0 = name1); (* FIXME *)
    apply loc (Input.Trm_ref (loc, name0, Input.Ih_none))

let mkloc lb ub =
    Location.between (Location.Bound.of_lexing_position lb)
		     (Location.Bound.of_lexing_position ub)
%}

%token EOF
%token BEGIN END

%token OPEN
%token INCLUDE
%token SIG
%token STRUCT
%token TYPE VAL INJ
%token WHERE WITH WHAT

%token IS
%token DO
%token GIVEN
%token RAISE
%token UPON

%token LEX
%token LEXALIAS
%token LEXIMPORT
%token NOTATION

%token LPAREN RPAREN
%token <Input.idr> LBRACKET RBRACKET
%token IF ELSE OTHERWISE
%token AT
%token MAPSTO
%token DOT

/* Logic Operators */
%token <Input.idr> LOGIC0 LOGIC1 LOGIC2 LOGIC3
%token <Input.idr> LOGIC4 LOGIC5 LOGIC6 LOGIC7 LOGIC8 QUANTIFIER
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
%token <Input.idr> RELATION

/* Arithmetic Operators */
%token <Input.idr> ARITH0 ARITH0_S ARITH1 ARITH1_S
%token <Input.idr> ARITH2 ARITH2_S ARITH3 ARITH3_S
%token <Input.idr> ARITH4 ARITH4_S ARITH5 ARITH5_S
%token <Input.idr> ARITH6 ARITH6_S ARITH7 ARITH7_S
%token <Input.idr> ARITH8 ARITH8_S ARITH9 ARITH9_S LABEL FENCE
%token <Input.idr> SCRIPT0_P SCRIPT0_S SCRIPT0_I
%token <Input.idr> SCRIPT1_P SCRIPT1_S SCRIPT1_I
%token <Input.idr> SCRIPT2_P SCRIPT2_S SCRIPT2_I
%token <Input.idr> PROJECT

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
%token <Input.lit> LITERAL
%token <Input.idr> IDENTIFIER
%token <Input.idr * Input.idrhint> HINTED_IDENTIFIER
%token <Input.def> PREPARED_DEF

%type <Input.trm> main
%type <Input.trm> expr
%start main
%%

main:
    BEGIN structure_body END EOF
    { Input.Trm_where (mkloc $startpos $endpos, List.rev $2) }
  ;

signature_body:
    /* empty */ { [] }
  | signature_body signature_clause { $2 :: $1 }
  ;

structure_body:
    /* empty */ { [] }
  | structure_body structure_clause { $2 :: $1 }
  ;

signature_clause:
    modular_clause { $1 }
  ;

structure_clause:
    modular_clause { $1 }
  | STRUCT structure_pattern BEGIN IS structure_expr END
    { Input.Def_struct (mkloc $startpos $endpos, $2, $5) }
  | STRUCT structure_pattern BEGIN structure_body END
    {
	let body = Input.Trm_where (mkloc $startpos($4) $endpos($4), $4) in
	Input.Def_struct (mkloc $startpos $endpos, $2, body)
    }
  | VAL term_pattern predicate
    { Input.Def_val (mkloc $startpos $endpos, $2, $3) }
  ;

modular_clause:
    OPEN projection
    { Input.Sct_open (mkloc $startpos $endpos, $2) }
  | INCLUDE projection
    { Input.Sct_include (mkloc $startpos $endpos, $2) }
  | SIG IDENTIFIER
    { Input.Dec_sig (mkloc $startpos $endpos, $2) }
  | SIG IDENTIFIER BEGIN IS signature_expr END
    { Input.Def_sig (mkloc $startpos $endpos, $2, $5) }
  | STRUCT structure_pattern
    { Input.Dec_struct (mkloc $startpos $endpos, $2) }
  | TYPE type_pattern
    { Input.Dec_type (mkloc $startpos $endpos, $2) }
  | TYPE type_pattern BEGIN IS type_expr END
    { Input.Def_type (mkloc $startpos $endpos, $2, $5) }
  | INJ term_pattern
    { Input.Dec_inj (mkloc $startpos $endpos, $2) }
  | VAL term_pattern
    { Input.Dec_val (mkloc $startpos $endpos, $2) }
  | PREPARED_DEF { $1 }
  ;

type_pattern: expr {$1};
term_pattern: expr {$1};
signature_expr: expr {$1};
structure_expr: expr {$1};
structure_pattern: expr {$1};
type_expr: expr {$1};
term: expr {$1};


/* Predicates */

predicate: BEGIN participle_seq compound_predicate END { $2 $3 };
atomic_predicate:
    IS term { $2 }
  | RAISE term { Input.Trm_raise (mkloc $startpos $endpos, $2) }
  ;
compound_predicate:
    atomic_predicate { $1 }
  | postif_predicate { $1 }
  | if_predicate { $1 }
  | at_predicate { Input.Trm_at (mkloc $startpos $endpos, $1) }
  ;
if_predicate:
    IF term predicate if_predicate
    { Input.Trm_if (mkloc $startpos $endpos, $2, $3, $4) }
  | ELSE predicate { $2 }
  ;
at_predicate:
    AT term_pattern predicate
    { [($2, $3)] }
  | AT term_pattern predicate at_predicate
    { ($2, $3) :: $4 }
  ;
postif_predicate:
    atomic_predicate BEGIN IF term END postif_predicate
    { Input.Trm_if (mkloc $startpos $endpos, $4, $1, $6) }
  | atomic_predicate BEGIN OTHERWISE END { $1 }
  ;

/* Participles */

participle_seq:
    /* empty */ { fun x -> x }
  | participle_seq participle { fun x -> $1 ($2 x) }
  ;
participle:
    GIVEN term_pattern predicate
    { fun x -> Input.Trm_let (mkloc $startpos $endpos, $2, $3, x) }
  ;

/* Expressions */

expr: conditional {$1};

conditional:
    qlogic_expr {$1}
  | qlogic_expr MAPSTO conditional
    { Input.Trm_lambda (mkloc $startpos $endpos, $1, $3) }
  ;
qlogic_expr:
    qseq logic_expr { quantify (mkloc $startpos $endpos) $1 $2 }
  ;
logic_expr:
    relational_expr { $1 }
  | logic_expr LOGIC0 qseq logic_expr
    { apply_infixq (mkloc $startpos $endpos) $2 $1 ($3, $4) }
  | LOGIC0 qseq logic_expr
    { apply_prefixq (mkloc $startpos $endpos) $1 ($2, $3) }
  | logic_expr LOGIC1 qseq logic_expr
    { apply_infixq (mkloc $startpos $endpos) $2 $1 ($3, $4) }
  | LOGIC1 qseq logic_expr
    { apply_prefixq (mkloc $startpos $endpos) $1 ($2, $3) }
  | logic_expr LOGIC2 qseq logic_expr
    { apply_infixq (mkloc $startpos $endpos) $2 $1 ($3, $4) }
  | LOGIC2 qseq logic_expr
    { apply_prefixq (mkloc $startpos $endpos) $1 ($2, $3) }
  | logic_expr LOGIC3 qseq logic_expr
    { apply_infixq (mkloc $startpos $endpos) $2 $1 ($3, $4) }
  | LOGIC3 qseq logic_expr
    { apply_prefixq (mkloc $startpos $endpos) $1 ($2, $3) }
  | logic_expr LOGIC4 qseq logic_expr
    { apply_infixq (mkloc $startpos $endpos) $2 $1 ($3, $4) }
  | LOGIC4 qseq logic_expr
    { apply_prefixq (mkloc $startpos $endpos) $1 ($2, $3) }
  | logic_expr LOGIC5 qseq logic_expr
    { apply_infixq (mkloc $startpos $endpos) $2 $1 ($3, $4) }
  | LOGIC5 qseq logic_expr
    { apply_prefixq (mkloc $startpos $endpos) $1 ($2, $3) }
  | logic_expr LOGIC6 qseq logic_expr
    { apply_infixq (mkloc $startpos $endpos) $2 $1 ($3, $4) }
  | LOGIC6 qseq logic_expr
    { apply_prefixq (mkloc $startpos $endpos) $1 ($2, $3) }
  | logic_expr LOGIC7 qseq logic_expr
    { apply_infixq (mkloc $startpos $endpos) $2 $1 ($3, $4) }
  | LOGIC7 qseq logic_expr
    { apply_prefixq (mkloc $startpos $endpos) $1 ($2, $3) }
  | logic_expr LOGIC8 qseq logic_expr
    { apply_infixq (mkloc $startpos $endpos) $2 $1 ($3, $4) }
  | LOGIC8 qseq logic_expr
    { apply_prefixq (mkloc $startpos $endpos) $1 ($2, $3) }
  ;

qseq:
    /* empty */ { [] }
  | qseq quantifier { $2 :: $1 }
  ;
quantifier:
    QUANTIFIER expr DOT { (mkloc $startpos $endpos, $1, $2) }
  ;

relational_expr:
    arith { $1 }
  | arith relation_seq { Input.Trm_rel (mkloc $startpos $endpos, $1, $2) }
  ;
relation_seq:
    relation_comp { [$1] }
  | relation_comp relation_seq { $1 :: $2 }
  ;
relation_comp:
    RELATION arith { (mkloc $startpos($1) $endpos($1), Input.idr_2o $1, $2) }
  ;
arith:
    application		{ $1 }
  | ARITH0 arith	{ apply_prefix (mkloc $startpos $endpos) $1 $2 }
  | arith ARITH0_S	{ apply_suffix (mkloc $startpos $endpos) $2 $1 }
  | arith ARITH0 arith	{ apply_infix (mkloc $startpos $endpos) $2 $1 $3 }
  | ARITH1 arith	{ apply_prefix (mkloc $startpos $endpos) $1 $2 }
  | arith ARITH1_S	{ apply_suffix (mkloc $startpos $endpos) $2 $1 }
  | arith ARITH1 arith	{ apply_infix (mkloc $startpos $endpos) $2 $1 $3 }
  | ARITH2 arith	{ apply_prefix (mkloc $startpos $endpos) $1 $2 }
  | arith ARITH2_S	{ apply_suffix (mkloc $startpos $endpos) $2 $1 }
  | arith ARITH2 arith	{ apply_infix (mkloc $startpos $endpos) $2 $1 $3 }
  | LABEL  arith	{ Input.Trm_label (mkloc $startpos $endpos, $1, $2) }
  | ARITH3 arith	{ apply_prefix (mkloc $startpos $endpos) $1 $2 }
  | arith ARITH3_S	{ apply_suffix (mkloc $startpos $endpos) $2 $1 }
  | arith ARITH3 arith	{ apply_infix (mkloc $startpos $endpos) $2 $1 $3 }
  | ARITH4 arith	{ apply_prefix (mkloc $startpos $endpos) $1 $2 }
  | arith ARITH4_S	{ apply_suffix (mkloc $startpos $endpos) $2 $1 }
  | arith ARITH4 arith	{ apply_infix (mkloc $startpos $endpos) $2 $1 $3 }
  | ARITH5 arith	{ apply_prefix (mkloc $startpos $endpos) $1 $2 }
  | arith ARITH5_S	{ apply_suffix (mkloc $startpos $endpos) $2 $1 }
  | arith ARITH5 arith	{ apply_infix (mkloc $startpos $endpos) $2 $1 $3 }
  | ARITH6 arith	{ apply_prefix (mkloc $startpos $endpos) $1 $2 }
  | arith ARITH6_S	{ apply_suffix (mkloc $startpos $endpos) $2 $1 }
  | arith ARITH6 arith	{ apply_infix (mkloc $startpos $endpos) $2 $1 $3 }
  | ARITH7 arith	{ apply_prefix (mkloc $startpos $endpos) $1 $2 }
  | arith ARITH7_S	{ apply_suffix (mkloc $startpos $endpos) $2 $1 }
  | arith ARITH7 arith	{ apply_infix (mkloc $startpos $endpos) $2 $1 $3 }
  | ARITH8 arith	{ apply_prefix (mkloc $startpos $endpos) $1 $2 }
  | arith ARITH8_S	{ apply_suffix (mkloc $startpos $endpos) $2 $1 }
  | arith ARITH8 arith	{ apply_infix (mkloc $startpos $endpos) $2 $1 $3 }
  | ARITH9 arith	{ apply_prefix (mkloc $startpos $endpos) $1 $2 }
  | arith ARITH9_S	{ apply_suffix (mkloc $startpos $endpos) $2 $1 }
  | arith ARITH9 arith	{ apply_infix (mkloc $startpos $endpos) $2 $1 $3 }
  ;
application:
    script { $1 }
  | application script { apply (mkloc $startpos $endpos) $1 $2 }
  | application LABEL script
    { let loc = mkloc $startpos $endpos
      in apply loc $1 (Input.Trm_label (loc, $2, $3)) }
  | FENCE arith FENCE { apply_fence (mkloc $startpos $endpos) $1 $3 $2 }
  ;
script:
    projection		    { $1 }
  | SCRIPT0_P script	    { apply_prefix (mkloc $startpos $endpos) $1 $2 }
  | script SCRIPT0_S	    { apply_suffix (mkloc $startpos $endpos) $2 $1 }
  | script SCRIPT0_I script { apply_infix (mkloc $startpos $endpos) $2 $1 $3 }
  | SCRIPT1_P script	    { apply_prefix (mkloc $startpos $endpos) $1 $2 }
  | script SCRIPT1_S	    { apply_suffix (mkloc $startpos $endpos) $2 $1 }
  | script SCRIPT1_I script { apply_infix (mkloc $startpos $endpos) $2 $1 $3 }
  | SCRIPT2_P script	    { apply_prefix (mkloc $startpos $endpos) $1 $2 }
  | script SCRIPT2_S	    { apply_suffix (mkloc $startpos $endpos) $2 $1 }
  | script SCRIPT2_I script { apply_infix (mkloc $startpos $endpos) $2 $1 $3 }
  ;

projection:
    atomic_expr { $1 }
  | projection PROJECT { Input.Trm_project (mkloc $startpos $endpos, $2, $1) }
  ;

atomic_expr:
    IDENTIFIER { Input.Trm_ref (mkloc $startpos $endpos, $1, Input.Ih_none) }
  | HINTED_IDENTIFIER
    { let idr, hint = $1 in Input.Trm_ref (mkloc $startpos $endpos, idr, hint) }
  | LITERAL { Input.Trm_literal (mkloc $startpos $endpos, $1) }
  | LPAREN parenthesised RPAREN { $2 }
  | LBRACKET parenthesised RBRACKET
    {
	let locb = mkloc $startpos $endpos($1) in
	let f = Input.Trm_ref (locb, Input.idr_1b $1 $3, Input.Ih_none) in
	Input.Trm_apply (mkloc $startpos $endpos, f, $2)
    }
  | WHERE BEGIN structure_body END
    { Input.Trm_where (mkloc $startpos $endpos, $3) }
  | WITH  BEGIN signature_body END
    { Input.Trm_with (mkloc $startpos $endpos, None, $3) }
  | WHAT predicate { $2 }
  ;
parenthesised:
    /* empty */ { Input.Trm_literal (mkloc $startpos $endpos, Input.Lit_unit) }
  | expr { $1 }
  ;
