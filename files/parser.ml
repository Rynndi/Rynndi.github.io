(*
   Left-corner parser in the style of UCLA CS131 HW2 OCaml grammars.

   Features:
   - CFG rule representation
   - top-down parser: Predict + Match
   - bottom-up parser: Shift + Reduce
   - left-corner parser: LC-Shift + LC-Predict + LC-Match + LC-Connect
   - Bar / NoBar stack symbols for left-corner parsing
   - ParseStep histories
   - bracket tree generator for ambiguity
   - English X-bar-ish grammar with CP/TP/VP/DP/PP/AP/AdvP
   - movement-like traces using explicit trace tokens: tDP, tWH, tT, tV

   Important:
   This is still a CFG-with-traces toy grammar, not a full Minimalist grammar.
   It can generate surface trees with traces, but it does not enforce true
   indexed movement dependencies.
*)
(* -------------------------------------------------------------------------- *)
(* Core CFG/parser types                                                       *)
(* -------------------------------------------------------------------------- *)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) rewrite_rule =
  | NTRule of 'nonterminal * 'nonterminal list
  | TRule of 'nonterminal * 'terminal
  | NoRule

type ('nonterminal, 'terminal) cfg =
  'nonterminal list * 'terminal list * 'nonterminal
  * ('nonterminal, 'terminal) rewrite_rule list

type transition =
  | NoTransition
  | Shift
  | Reduce
  | Predict
  | Match
  | Connect

type 'nonterminal stack_symbol =
  | Bar of 'nonterminal
  | NoBar of 'nonterminal

type ('nonterminal, 'terminal) config =
  'nonterminal stack_symbol list * 'terminal list

type ('nonterminal, 'terminal) parse_step =
  ParseStep of
    transition *
    ('nonterminal, 'terminal) rewrite_rule *
    ('nonterminal, 'terminal) config


(* -------------------------------------------------------------------------- *)
(* Helpers                                                                     *)
(* -------------------------------------------------------------------------- *)

let rec map_maybe f xs =
  match xs with
  | [] -> []
  | x :: rest ->
      (match f x with
       | Some y -> y :: map_maybe f rest
       | None -> map_maybe f rest)

let unwrap_stack_symbol stack_sym =
  match stack_sym with
  | Bar nt -> nt
  | NoBar nt -> nt

let rule_lhs rule =
  match rule with
  | NTRule (left, _) -> Some left
  | TRule (left, _) -> Some left
  | NoRule -> None

let is_nt_rule rule =
  match rule with
  | NTRule _ -> true
  | _ -> false

let is_rule_cnf rule =
  match rule with
  | NTRule (_, [_; _]) -> true
  | TRule (_, _) -> true
  | NoRule -> true
  | _ -> false

let is_cnf (_, _, _, rules) =
  List.for_all is_rule_cnf rules

let find_nts_generating_t rules terminal =
  let get_nt rule =
    match rule with
    | TRule (nt, t) when t = terminal -> Some nt
    | _ -> None
  in
  map_maybe get_nt rules

let rec take n xs =
  if n <= 0 then []
  else
    match xs with
    | [] -> []
    | x :: rest -> x :: take (n - 1) rest

let rec drop n xs =
  if n <= 0 then xs
  else
    match xs with
    | [] -> []
    | _ :: rest -> drop (n - 1) rest

let last_n n xs =
  drop (List.length xs - n) xs

let sum xs =
  List.fold_left ( + ) 0 xs

let minimum_or default xs =
  match xs with
  | [] -> default
  | x :: rest -> List.fold_left min x rest


(* avoids recursing forever through immediately recursive rules. *)
let min_nt_length rules nt =
  let rec helper visited curr =
    if List.mem curr visited then max_int / 4
    else
      let relevant =
        List.filter
          (fun rule ->
             match rule with
             | NTRule (left, _) -> left = curr
             | TRule (left, _) -> left = curr
             | NoRule -> false)
          rules
      in
      let lengths =
        List.map
          (fun rule ->
             match rule with
             | TRule _ -> 1
             | NTRule (_, children) ->
                 sum (List.map (helper (curr :: visited)) children)
             | NoRule -> 0)
          relevant
      in
      minimum_or (max_int / 4) lengths
  in
  helper [] nt

let min_stack_length rules stack_sym =
  min_nt_length rules (unwrap_stack_symbol stack_sym)


(* -------------------------------------------------------------------------- *)
(* Generic parser driver                                                       *)
(* -------------------------------------------------------------------------- *)

let rec parser_internal transitions rules start_config goal_config =
  let steps =
    List.concat (List.map (fun trans -> trans rules start_config) transitions)
  in
  let continue_step step =
    let ParseStep (_, _, new_config) = step in
    if new_config = goal_config then
      [[step]]
    else
      List.map
        (fun later_steps -> step :: later_steps)
        (parser_internal transitions rules new_config goal_config)
  in
  List.concat (List.map continue_step steps)

let parser transitions rules start_config goal_config =
  List.map
    (fun steps -> ParseStep (NoTransition, NoRule, start_config) :: steps)
    (parser_internal transitions rules start_config goal_config)


(* -------------------------------------------------------------------------- *)
(* Bottom-up parser                                                            *)
(* -------------------------------------------------------------------------- *)

let shift rules config =
  match config with
  | (stack, terminal :: rest_input) ->
      let nts = find_nts_generating_t rules terminal in
      List.map
        (fun nt ->
           ParseStep
             (Shift, TRule (nt, terminal), (stack @ [NoBar nt], rest_input)))
        nts
  | (_, []) -> []

let reduce rules config =
  let (stack, input) = config in
  let stack_len = List.length stack in
  let try_rule rule =
    match rule with
    | NTRule (nt, children) ->
        let children_len = List.length children in
        let wanted_suffix = List.map (fun child -> NoBar child) children in
        if stack_len >= children_len
           && last_n children_len stack = wanted_suffix
        then
          let prefix = take (stack_len - children_len) stack in
          Some
            (ParseStep
               (Reduce, rule, (prefix @ [NoBar nt], input)))
        else None
    | _ -> None
  in
  map_maybe try_rule rules

let finite_reduce rules config =
  List.filter
    (fun step ->
       match step with
       | ParseStep (_, NTRule (_, []), _) -> false
       | _ -> true)
    (reduce rules config)

let bottom_up cfg input =
  let (_, _, start, rules) = cfg in
  let starting_config = ([], input) in
  let goal_config = ([NoBar start], []) in
  parser [shift; reduce] rules starting_config goal_config

let finite_bottom_up cfg input =
  let (_, _, start, rules) = cfg in
  let starting_config = ([], input) in
  let goal_config = ([NoBar start], []) in
  parser [shift; finite_reduce] rules starting_config goal_config


(* -------------------------------------------------------------------------- *)
(* Top-down parser                                                             *)
(* -------------------------------------------------------------------------- *)

let predict rules config =
  match config with
  | (nt :: rest_stack, input) ->
      let min_needed =
        min_stack_length rules nt
        + sum (List.map (min_stack_length rules) rest_stack)
      in
      if min_needed > List.length input then []
      else
        let filtered_rules =
          List.filter
            (fun rule ->
               match rule with
               | NTRule (parent, _) -> nt = NoBar parent
               | _ -> false)
            rules
        in
        List.map
          (fun rule ->
             match rule with
             | NTRule (_, children) ->
                 ParseStep
                   (Predict, rule,
                    (List.map (fun child -> NoBar child) children @ rest_stack,
                     input))
             | _ -> failwith "unreachable")
          filtered_rules
  | ([], _) -> []

let match_top_down rules config =
  match config with
  | (nt :: rest_stack, terminal :: rest_input) ->
      let nts = find_nts_generating_t rules terminal in
      (match List.find_opt (fun found_nt -> nt = NoBar found_nt) nts with
       | Some found_nt ->
           [ParseStep
              (Match, TRule (found_nt, terminal), (rest_stack, rest_input))]
       | None -> [])
  | _ -> []

let top_down cfg input =
  let (_, _, start, rules) = cfg in
  let starting_config = ([NoBar start], input) in
  let goal_config = ([], []) in
  parser [match_top_down; predict] rules starting_config goal_config


(* -------------------------------------------------------------------------- *)
(* Left-corner parser                                                          *)
(* -------------------------------------------------------------------------- *)

let shift_lc rules config =
  match config with
  | (stack, terminal :: rest_input) ->
      let nts = find_nts_generating_t rules terminal in
      List.map
        (fun nt ->
           ParseStep
             (Shift, TRule (nt, terminal), (NoBar nt :: stack, rest_input)))
        nts
  | (_, []) -> []

let match_lc rules config =
  match config with
  | (nt :: rest_stack, terminal :: rest_input) ->
      let nts = find_nts_generating_t rules terminal in
      (match List.find_opt (fun found_nt -> nt = Bar found_nt) nts with
       | Some found_nt ->
           [ParseStep
              (Match, TRule (found_nt, terminal), (rest_stack, rest_input))]
       | None -> [])
  | _ -> []

let predict_lc rules config =
  match config with
  | (nt :: rest_stack, input) ->
      let filtered_rules =
        List.filter
          (fun rule ->
             match rule with
             | NTRule (_, child :: _) -> nt = NoBar child
             | _ -> false)
          rules
      in
      List.map
        (fun rule ->
           match rule with
           | NTRule (parent, _left_corner :: remaining_children) ->
               let barred_remaining =
                 List.map (fun child -> Bar child) remaining_children
               in
               ParseStep
                 (Predict, rule,
                  (barred_remaining @ [NoBar parent] @ rest_stack, input))
           | _ -> failwith "unreachable")
        filtered_rules
  | ([], _) -> []

let connect_lc rules config =
  match config with
  | (nt1 :: nt2 :: rest_stack, input) ->
      let filtered_rules =
        List.filter
          (fun rule ->
             match rule with
             | NTRule (parent, child :: _) ->
                 nt2 = Bar parent && nt1 = NoBar child
             | _ -> false)
          rules
      in
      List.map
        (fun rule ->
           match rule with
           | NTRule (_, _left_corner :: remaining_children) ->
               let barred_remaining =
                 List.map (fun child -> Bar child) remaining_children
               in
               ParseStep
                 (Connect, rule, (barred_remaining @ rest_stack, input))
           | _ -> failwith "unreachable")
        filtered_rules
  | _ -> []

let left_corner cfg input =
  let (_, _, start, rules) = cfg in
  let starting_config = ([Bar start], input) in
  let goal_config = ([], []) in
  parser [shift_lc; predict_lc; match_lc; connect_lc] rules starting_config goal_config


(* -------------------------------------------------------------------------- *)
(* Pretty-printing helpers                                                     *)
(* -------------------------------------------------------------------------- *)

let string_of_transition trans =
  match trans with
  | NoTransition -> "NoTransition"
  | Shift -> "Shift"
  | Reduce -> "Reduce"
  | Predict -> "Predict"
  | Match -> "Match"
  | Connect -> "Connect"

let string_of_stack_symbol show_nt stack_sym =
  match stack_sym with
  | Bar nt -> show_nt nt ^ "*"
  | NoBar nt -> show_nt nt

let string_of_rule show_nt show_t rule =
  match rule with
  | NTRule (left, right) ->
      show_nt left ^ " -> "
      ^ String.concat " " (List.map show_nt right)
  | TRule (left, right) ->
      show_nt left ^ " -> " ^ show_t right
  | NoRule -> "NoRule"

let string_of_config show_nt show_t config =
  let (stack, input) = config in
  let stack_str =
    "[" ^ String.concat "; " (List.map (string_of_stack_symbol show_nt) stack)
    ^ "]"
  in
  let input_str =
    "[" ^ String.concat "; " (List.map show_t input) ^ "]"
  in
  "(" ^ stack_str ^ ", " ^ input_str ^ ")"

let string_of_parse_step show_nt show_t step =
  let ParseStep (trans, rule, config) = step in
  string_of_transition trans
  ^ " | "
  ^ string_of_rule show_nt show_t rule
  ^ " | "
  ^ string_of_config show_nt show_t config

let string_of_parse show_nt show_t parse =
  String.concat "\n" (List.map (string_of_parse_step show_nt show_t) parse)


(* -------------------------------------------------------------------------- *)
(* Fast chart-based parse tree / bracket generation                            *)
(* -------------------------------------------------------------------------- *)

type ('nonterminal, 'terminal) parse_tree =
  | Leaf of 'nonterminal * 'terminal
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list

let rec tree_to_brackets show_nt show_t tree =
  match tree with
  | Leaf (nt, terminal) ->
      "[" ^ show_nt nt ^ " " ^ show_t terminal ^ "]"

  | Node (nt, children) ->
      "[" ^ show_nt nt ^ " "
      ^ String.concat " " (List.map (tree_to_brackets show_nt show_t) children)
      ^ "]"

let words sentence =
  List.filter
    (fun s -> s <> "")
    (String.split_on_char ' ' sentence)

let rec range a b =
  if a >= b then []
  else a :: range (a + 1) b

let list_concat_map f xs =
  List.concat (List.map f xs)

let get_chart chart nt i j =
  match Hashtbl.find_opt chart (nt, i, j) with
  | Some trees -> trees
  | None -> []

let rec add_tree chart unary_rules nt i j tree =
  let existing = get_chart chart nt i j in
  if List.mem tree existing then
    ()
  else begin
    Hashtbl.replace chart (nt, i, j) (tree :: existing);

    (* Unary closure:
       If B was added and A -> B exists, add A over the same span.
    *)
    List.iter
      (fun (parent, child) ->
         if child = nt then
           add_tree chart unary_rules parent i j
             (Node (parent, [tree])))
      unary_rules
  end

let parse_trees_chart_from cfg start_symbol input =
  let (_, _, _, rules) = cfg in
  let input_arr = Array.of_list input in
  let n = Array.length input_arr in

  let terminal_rules =
    List.fold_right
      (fun rule acc ->
         match rule with
         | TRule (lhs, terminal) -> (lhs, terminal) :: acc
         | _ -> acc)
      rules
      []
  in

  let unary_rules =
    List.fold_right
      (fun rule acc ->
         match rule with
         | NTRule (lhs, [child]) -> (lhs, child) :: acc
         | _ -> acc)
      rules
      []
  in

  let binary_rules =
    List.fold_right
      (fun rule acc ->
         match rule with
         | NTRule (lhs, [left; right]) -> (lhs, left, right) :: acc
         | _ -> acc)
      rules
      []
  in

  let chart = Hashtbl.create 1000 in

  (* Initialize lexical entries. *)
  for i = 0 to n - 1 do
    let word = input_arr.(i) in
    List.iter
      (fun (lhs, terminal) ->
         if terminal = word then
           add_tree chart unary_rules lhs i (i + 1)
             (Leaf (lhs, terminal)))
      terminal_rules
  done;

  (* Bottom-up dynamic programming over spans. *)
  for span_len = 2 to n do
    for i = 0 to n - span_len do
      let j = i + span_len in
      for k = i + 1 to j - 1 do
        List.iter
          (fun (lhs, left_cat, right_cat) ->
             let left_trees = get_chart chart left_cat i k in
             let right_trees = get_chart chart right_cat k j in
             List.iter
               (fun left_tree ->
                  List.iter
                    (fun right_tree ->
                       add_tree chart unary_rules lhs i j
                         (Node (lhs, [left_tree; right_tree])))
                    right_trees)
               left_trees)
          binary_rules
      done
    done
  done;

  List.rev (get_chart chart start_symbol 0 n)

let parse_trees_chart cfg input =
  let (_, _, start, _) = cfg in
  parse_trees_chart_from cfg start input

let bracketings_with show_nt show_t cfg input =
  List.map
    (tree_to_brackets show_nt show_t)
    (parse_trees_chart cfg input)

let bracketings_from_with show_nt show_t cfg start_symbol input =
  List.map
    (tree_to_brackets show_nt show_t)
    (parse_trees_chart_from cfg start_symbol input)

let bracket_sentence_with show_nt show_t cfg sentence =
  bracketings_with show_nt show_t cfg (words sentence)

let bracket_sentence_from_with show_nt show_t cfg start_symbol sentence =
  bracketings_from_with show_nt show_t cfg start_symbol (words sentence)

let print_bracketings bracket_list =
  List.iteri
    (fun i br ->
       print_string ("Parse " ^ string_of_int (i + 1) ^ ":\n");
       print_string br;
       print_string "\n\n")
    bracket_list


(* -------------------------------------------------------------------------- *)
(* Conservative English/X-bar grammar                                          *)
(* -------------------------------------------------------------------------- *)

type cat =
  | S

  (* clause structure *)
  | CP | CP_Decl | CP_Q | CP_WH
  | Cbar | Cbar_Decl | Cbar_Q | Cbar_WH
  | C | C_WH | C_Q | C_Q_WH

  (* TP / tense / infinitives *)
  | TP | Tbar | T
  | T_Pres | T_Past | T_Inf
  | Aux | Modal
  | InfTP

  (* verbal structure *)
  | VP | Vbar | V

  (* reduced relatives / participles *)
  | RedRel | Participle

  (* nominal structure *)
  | DP | Dbar | D | NP | Nbar | N | Pron
  | WhDP | WhD

  (* adjective/adverb/preposition *)
  | AP | Abar | A
  | AdvP | Adv
  | PP | Pbar | P
  | OfGerundPP | P_Of
  | WhPP

  (* coordination.
     Keep old CoordP/Coordbar for compatibility, but do not use them
     in the grammar rules below. Use DPCoordP and VPCoordP instead. *)
  | CoordP | Coordbar
  | DPCoordP | DPCoordbar
  | VPCoordP | VPCoordbar
  | Coord

  (* traces.
     These do not perform movement.
     They only appear if the input contains literal trace tokens:
       tDP, tWH, tT
  *)
  | TraceDP | TraceWH | TraceT

  (* number / quantity / measure / money *)
  | Num
  | QuantP
  | MoneyP
  | MeasureP
  | Unit
  | Currency

  (* gerunds *)
  | GerundP
  | Gerund

let show_cat cat =
  match cat with
  | S -> "S"

  | CP -> "CP"
  | CP_Decl -> "CP[decl]"
  | CP_Q -> "CP[+Q]"
  | CP_WH -> "CP[+WH]"

  | Cbar -> "C'"
  | Cbar_Decl -> "C'[decl]"
  | Cbar_Q -> "C'[+Q]"
  | Cbar_WH -> "C'[+WH]"

  | C -> "C"
  | C_WH -> "C[+WH]"
  | C_Q -> "C[+Q]"
  | C_Q_WH -> "C[+Q,+WH]"

  | TP -> "TP"
  | Tbar -> "T'"
  | T -> "T"
  | T_Pres -> "T[pres]"
  | T_Past -> "T[past]"
  | T_Inf -> "T[inf]"
  | Aux -> "Aux"
  | Modal -> "Modal"
  | InfTP -> "InfTP"

  | VP -> "VP"
  | Vbar -> "V'"
  | V -> "V"

  | RedRel -> "RedRel"
  | Participle -> "Participle"

  | DP -> "DP"
  | Dbar -> "D'"
  | D -> "D"
  | NP -> "NP"
  | Nbar -> "N'"
  | N -> "N"
  | Pron -> "Pron"

  | WhDP -> "WhDP"
  | WhD -> "WhD"

  | AP -> "AP"
  | Abar -> "A'"
  | A -> "A"

  | AdvP -> "AdvP"
  | Adv -> "Adv"

  | PP -> "PP"
  | Pbar -> "P'"
  | P -> "P"
  | OfGerundPP -> "OfGerundPP"
  | P_Of -> "P[of]"
  | WhPP -> "WhPP"

  | CoordP -> "CoordP"
  | Coordbar -> "Coord'"
  | DPCoordP -> "DPCoordP"
  | DPCoordbar -> "DPCoord'"
  | VPCoordP -> "VPCoordP"
  | VPCoordbar -> "VPCoord'"
  | Coord -> "Coord"

  | TraceDP -> "tDP"
  | TraceWH -> "tWH"
  | TraceT -> "tT"

  | Num -> "Num"
  | QuantP -> "QuantP"
  | MoneyP -> "MoneyP"
  | MeasureP -> "MeasureP"
  | Unit -> "Unit"
  | Currency -> "Currency"

  | GerundP -> "GerundP"
  | Gerund -> "Gerund"

let all_cats =
  [S;

   CP; CP_Decl; CP_Q; CP_WH;
   Cbar; Cbar_Decl; Cbar_Q; Cbar_WH;
   C; C_WH; C_Q; C_Q_WH;

   TP; Tbar; T;
   T_Pres; T_Past; T_Inf;
   Aux; Modal; InfTP;

   VP; Vbar; V;

   RedRel; Participle;

   DP; Dbar; D; NP; Nbar; N; Pron;
   WhDP; WhD;

   AP; Abar; A;
   AdvP; Adv;
   PP; Pbar; P; OfGerundPP; P_Of;
   WhPP;

   CoordP; Coordbar;
   DPCoordP; DPCoordbar;
   VPCoordP; VPCoordbar;
   Coord;

   TraceDP; TraceWH; TraceT;

   Num; QuantP; MoneyP; MeasureP; Unit; Currency;

   GerundP; Gerund]

(* Convenience wrappers now that show_cat exists. *)

let bracketings cfg input =
  bracketings_with show_cat (fun s -> s) cfg input

let bracketings_from cfg start_symbol input =
  bracketings_from_with show_cat (fun s -> s) cfg start_symbol input

let bracket_sentence cfg sentence =
  bracket_sentence_with show_cat (fun s -> s) cfg sentence

let bracket_sentence_from cfg start_symbol sentence =
  bracket_sentence_from_with show_cat (fun s -> s) cfg start_symbol sentence

(* -------------------------------------------------------------------------- *)
(* Conservative English X-bar rules                                            *)
(* -------------------------------------------------------------------------- *)

let english_syntax_rules = [
  (* Root: full English clauses only.
     Headline fragments should be parsed from NP/DP manually with
     print_sentence_parses_from.
  *)
  NTRule (S, [TP]);
  NTRule (S, [CP]);

  (* ---------------------------------------------------------------------- *)
  (* CP layer                                                                *)
  (* ---------------------------------------------------------------------- *)

  (* Generic CP can be declarative, yes/no question, or wh-question/embedded wh. *)
  NTRule (CP, [CP_Decl]);
  NTRule (CP, [CP_Q]);
  NTRule (CP, [CP_WH]);

  (* Declarative CP:
       that the gardener transplanted flowers
  *)
  NTRule (CP_Decl, [Cbar_Decl]);
  NTRule (Cbar_Decl, [C; TP]);

  (* Yes/no question CP:
       did John leave
  *)
  NTRule (CP_Q, [Cbar_Q]);
  NTRule (Cbar_Q, [C_Q; TP]);

  (* Wh CP:
       when did John leave
       when we should harvest potatoes

     Spec-CP is modeled by WhDP/WhPP before C'.
  *)
  NTRule (CP_WH, [WhDP; Cbar_WH]);
  NTRule (CP_WH, [WhPP; Cbar_WH]);

  (* Overt wh C:
       when did John leave

     Silent embedded wh C:
       the botanist knows when we should harvest potatoes

     The rule Cbar_WH -> TP is the CFG approximation of null C[+WH].
  *)
  NTRule (Cbar_WH, [C_Q_WH; TP]);
  NTRule (Cbar_WH, [C_WH; TP]);
  NTRule (Cbar_WH, [TP]);

  (* ---------------------------------------------------------------------- *)
  (* TP layer / EPP / tense                                                  *)
  (* ---------------------------------------------------------------------- *)

  (* EPP:
     finite TP requires a subject DP in Spec-TP.
  *)
  NTRule (TP, [DP; Tbar]);

  (* T' layer.
     Tbar -> VP is the practical shortcut for ordinary English words where
     tense is bound onto the verb, e.g. knows, appears, played.
  *)
  NTRule (Tbar, [T_Pres; VP]);
  NTRule (Tbar, [T_Past; VP]);
  NTRule (Tbar, [Aux; VP]);
  NTRule (Tbar, [Modal; VP]);
  NTRule (Tbar, [Modal; VPCoordP]);
  NTRule (Tbar, [TraceT; VP]);
  NTRule (Tbar, [VP]);

  (* Infinitival TP:
       to possess a magical touch

     This replaces the dangerous old rule:
       TP -> Tbar

     So finite TP still has EPP, but infinitives can be subjectless.
     T_Inf -> T lets your existing lexical rule T -> "to" keep working.
  *)
  NTRule (T_Inf, [T]);
  NTRule (InfTP, [T_Inf; VP]);

  (* ---------------------------------------------------------------------- *)
  (* VP layer                                                                *)
  (* ---------------------------------------------------------------------- *)

  NTRule (VP, [Vbar]);

  (* VP-internal subject trace.
     This only appears if the input contains literal tDP.
     It does not create silent movement by itself.
  *)
  NTRule (VP, [TraceDP; Vbar]);

  NTRule (Vbar, [V]);
  NTRule (Vbar, [V; DP]);
  NTRule (Vbar, [V; PP]);
  NTRule (Vbar, [V; CP]);
  NTRule (Vbar, [V; InfTP]);

  (* Real PP attachment ambiguity:
     I [saw the man] [with the binoculars].
  *)
  NTRule (Vbar, [Vbar; PP]);

  (* Optional wh trace as a VP/V' adjunct position.
     This only appears with literal tWH input.
  *)
  NTRule (Vbar, [Vbar; TraceWH]);

  (* Adverbs *)
  NTRule (Vbar, [AdvP; Vbar]);
  NTRule (Vbar, [Vbar; AdvP]);

  (* ---------------------------------------------------------------------- *)
  (* Reduced relatives / participial modifiers                               *)
  (* ---------------------------------------------------------------------- *)

  (* plans [released for a ship]
     plans [released] [for a ship]
     food [cooked for 10 people]
     [food cooked] [for 10 people]
  *)
  NTRule (RedRel, [Participle; PP]);
  NTRule (RedRel, [Participle]);
  NTRule (Nbar, [N; RedRel]);

  (* ---------------------------------------------------------------------- *)
  (* AP layer                                                                *)
  (* ---------------------------------------------------------------------- *)

  NTRule (AP, [Abar]);
  NTRule (Abar, [A]);
  NTRule (Abar, [AdvP; Abar]);

  (* Ordinary adjective + PP complements:*)
  NTRule (Abar, [A; PP]);
  (* Special gerund complement:*)
  NTRule (Abar, [A; OfGerundPP]);

  (* Postnominal AP modifier:*)
  NTRule (Nbar, [N; AP]);
  (* AdvP layer *)
  NTRule (AdvP, [Adv]);

  (* ---------------------------------------------------------------------- *)
  (* PP layer                                                                *)
  (* ---------------------------------------------------------------------- *)

  NTRule (PP, [Pbar]);
  NTRule (Pbar, [P; DP]);

  (* Specialized "of + gerund phrase" complement.
     Do NOT make this a regular PP, or the fake parse returns:
       [ship capable] [of carrying 80,000 people]
  *)
  NTRule (OfGerundPP, [P_Of; GerundP]);

  (* WhPP:
     when
     where
     or larger wh-PPs if you build them from PP
  *)
  NTRule (WhPP, [PP]);

  (* ---------------------------------------------------------------------- *)
  (* DP / NP layer                                                           *)
  (* ---------------------------------------------------------------------- *)

  NTRule (DP, [Dbar]);
  NTRule (DP, [Pron]);

  (* bare plural/common-noun DPs:
     vending machines, sharks, potatoes
  *)
  NTRule (DP, [NP]);

  (* Trace DPs.
     These only appear with literal tDP/tWH input.
  *)
  NTRule (DP, [TraceDP]);
  NTRule (DP, [TraceWH]);

  (* Cardinal DPs:
     80,000 people
     10 people
     four bedrooms
  *)
  NTRule (DP, [QuantP; NP]);

  NTRule (Dbar, [D; NP]);

  NTRule (NP, [Nbar]);
  NTRule (Nbar, [N]);

  (* Prenominal APs:
     the poor student
     the mile long ship
     vending machines
  *)
  NTRule (Nbar, [AP; Nbar]);

  (* NP-level PP attachment:
     the man [with the binoculars]
     plans [released] [for a ship]
     sense [of composition]
  *)
  NTRule (Nbar, [Nbar; PP]);

  (* WhDP:
     which book
     which boring book
  *)
  NTRule (WhDP, [WhD; NP]);

  (* ---------------------------------------------------------------------- *)
  (* Coordination                                                            *)
  (* ---------------------------------------------------------------------- *)

  (* VP coordination:
       harvest potatoes and prepare the soil
  *)
  NTRule (VPCoordP, [VP; VPCoordbar]);
  NTRule (VPCoordbar, [Coord; VP]);
  (* allow VP coordination directly after modals, but do not let every VP become a coordination phrase. *)
  (* NTRule (VP, [VPCoordP]); *)


  (* DP coordination:
       a magical touch and a unique sense of composition

     This replaces the old shared CoordP rules, which caused DP/VP
     category leakage and huge parse explosions.
  *)
  NTRule (DPCoordP, [DP; DPCoordbar]);
  NTRule (DPCoordbar, [Coord; DP]);
  NTRule (DP, [DPCoordP]);

  (* ---------------------------------------------------------------------- *)
  (* Number / measure / money rules                                          *)
  (* ---------------------------------------------------------------------- *)

  (* Quantities:
     sixteen
     billion
     sixteen billion
     80,000
  *)
  NTRule (QuantP, [Num]);
  NTRule (QuantP, [Num; Num]);

  (* Money phrases.

     Prefix version models "$16" as "$ 16":
       $ 16
       $ 16 billion

     Suffix version still allows ordinary English modifier forms:
       sixteen dollar
       sixteen billion dollar
  *)
  NTRule (MoneyP, [Currency; QuantP]);
  NTRule (MoneyP, [QuantP; Currency]);

  (* Measure phrases:
     mile
     billion mile
     four bedroom
  *)
  NTRule (MeasureP, [Unit]);
  NTRule (MeasureP, [QuantP; Unit]);

  (* Measure adjectives:
     mile long
     billion mile long
  *)
  NTRule (AP, [MeasureP; A]);

  (* Nominal money modifier:
     $ 16 billion ship
     $ 16 
     sixteen billion dollar ship
  *)
  NTRule (Nbar, [MoneyP; Nbar]);

  (* Gerund phrase:
     carrying 80,000 people
  *)
  NTRule (GerundP, [Gerund; DP])
]


(* -------------------------------------------------------------------------- *)
(* English-only lexicon                                                        *)
(* -------------------------------------------------------------------------- *)

let make_trules category word_list =
  List.map (fun word -> TRule (category, word)) word_list

let determiners =
  ["the"; "The"; "a"; "an"; "that"; "this"; "This"; "those"; "his"; "her"; "my"; "more"]

let wh_determiners =
  ["which"]

let pronouns =
  ["I"; "i"; "we"; "he"; "she"; "they"; "John"; "Mary"; "Carol"]

let nouns =
  ["man"; "student"; "burglar"; "binoculars"; "knife"; "botanist";
   "gardener"; "potatoes"; "soil"; "season"; "teacher"; "book"; "students";
   "florist"; "lawyer"; "client"; "cat"; "dog"; "telescope"; "elephant";
   "pajamas"; "ship"; "plans"; "people"; "food"; "guy"; "fiction"; "porn";
   "machine"; "machines"; "shark"; "sharks"; "touch"; "sense"; "composition";
   "pianist"; "colleague"; "piece"; "house"; "bedroom"; "tree"; "bee";
   "rock"; "city"; "room"; "window"; "garden"; "school"; "friend";
   "park"; "river"]

let adjectives =
  ["young"; "talented"; "beautiful"; "colorful"; "boring";
   "new"; "poor"; "magical"; "unique"; "next"; "angry"; "sure";
   "long"; "old"; "small"; "blue"; "big"; "red"; "happy"; "strange";
   "capable"; "vending"; "pianist's"]

let adverbs =
  ["passionately"; "finally"; "fully"; "extremely"; "quickly"; "slowly"]
let verbs =
  ["saw"; "threatened"; "knows"; "harvest"; "prepare";
   "played"; "said"; "transplanted"; "understand";
   "seem"; "be"; "appears"; "possess"; "assigned"; "have"; "shot";
   "build"; "found"; "liked"; "met"; "helped"; "watched"; "visited";
   "bought"; "gave"; "seen"; "kill"]
let participles =
  ["released"; "built"; "seen"; "found"; "cooked"]

let gerunds =
  ["carrying"; "building"; "holding"; "wearing"]

let auxiliaries =
  ["is"; "did"; "has"; "was"; "were"; "does"; "do"; "have"]

let modals =
  ["should"; "can"; "could"; "would"; "will"; "might"; "must"]

let tense_words =
  ["to"]

let prepositions =
  ["with"; "to"; "for"; "at"; "of"; "in"; "on"; "by"; "behind"; "near"; "than"]
let complementizers =
  ["that"]

let question_auxiliaries =
  ["is"; "did"; "has"; "was"; "were"; "does"; "do"]

let wh_question_auxiliaries =
  ["did"; "has"; "is"; "was"; "were"; "does"; "do"]

let wh_prepositions =
  ["when"; "where"]

let coordinators =
  ["and"; "or"]

let trace_words =
  ["tDP"; "tWH"; "tT"]

let numbers =
  ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "ten";
   "sixteen"; "million"; "billion"; "80,000"; "80000"]

let units =
  ["mile"; "miles"; "bedroom"; "bedrooms"]

let currencies =
  ["dollar"; "dollars"]

let terminals =
  determiners
  @ wh_determiners
  @ pronouns
  @ nouns
  @ adjectives
  @ adverbs
  @ verbs
  @ participles
  @ gerunds
  @ auxiliaries
  @ modals
  @ tense_words
  @ prepositions
  @ complementizers
  @ question_auxiliaries
  @ wh_question_auxiliaries
  @ wh_prepositions
  @ coordinators
  @ trace_words
  @ numbers
  @ units
  @ currencies

let english_lexical_rules =
  make_trules D determiners
  @ make_trules WhD wh_determiners
  @ make_trules Pron pronouns
  @ make_trules N nouns
  @ make_trules A adjectives
  @ make_trules Adv adverbs
  @ make_trules V verbs
  @ make_trules Participle participles
  @ make_trules Gerund gerunds
  @ make_trules Aux auxiliaries
  @ make_trules Modal modals
  @ make_trules T tense_words
  @ make_trules P prepositions
  @ make_trules C complementizers
  @ make_trules C_Q question_auxiliaries
  @ make_trules C_Q_WH wh_question_auxiliaries
  @ make_trules WhPP wh_prepositions
  @ make_trules Coord coordinators
  @ make_trules Num numbers
  @ make_trules Unit units
  @ make_trules Currency currencies
  @ [TRule (P_Of, "of");
     TRule (TraceDP, "tDP");
     TRule (TraceWH, "tWH");
     TRule (TraceT, "tT");

     (* Optional explicit feature tokens.
        Normal English sentences do not need these.
        Use them only if you manually put "pres", "past", or "whC"
        into the input string. *)
     TRule (T_Pres, "pres");
     TRule (T_Past, "past");
     TRule (C_WH, "whC")]


(* -------------------------------------------------------------------------- *)
(* Final conservative English grammar                                          *)
(* -------------------------------------------------------------------------- *)

let english_xbar_cfg =
  (
    all_cats,
    terminals,
    S,
    english_syntax_rules @ english_lexical_rules
  )

(* -------------------------------------------------------------------------- *)
(* Mandarin/X-bar-ish grammar for 3TS elicitation                              *)
(* -------------------------------------------------------------------------- *)

type mcat =
  | MS
  | MTP

  (* nominal structure *)
  | MNP | MNbar | MN | MProperN | MPron | MDet
  | MPossP | MDe
  | MClP | MNum | MCl

  (* verbal structure *)
  | MVP | MVbar | MV
  | MModal
  | MASP

  (* adjectives/adverbs/resultatives *)
  | MAP | MA | MDeg
  | MAdvP | MAdv
  | MResultP | MResult

  (* negation/comparison/locative/dative *)
  | MNegP | MNeg
  | MBiP | MBi
  | MLocP | MLoc
  | MPP | MP
  | MGeiP | MGei

let show_mcat cat =
  match cat with
  | MS -> "S"
  | MTP -> "TP"

  | MNP -> "NP"
  | MNbar -> "N'"
  | MN -> "N"
  | MProperN -> "ProperN"
  | MPron -> "Pron"
  | MDet -> "Det"

  | MPossP -> "PossP"
  | MDe -> "DE"
  | MClP -> "ClP"
  | MNum -> "Num"
  | MCl -> "Cl"

  | MVP -> "VP"
  | MVbar -> "V'"
  | MV -> "V"
  | MModal -> "Modal"
  | MASP -> "ASP"

  | MAP -> "AP"
  | MA -> "A"
  | MDeg -> "Deg"
  | MAdvP -> "AdvP"
  | MAdv -> "Adv"
  | MResultP -> "ResultP"
  | MResult -> "Result"

  | MNegP -> "NegP"
  | MNeg -> "Neg"
  | MBiP -> "BiP"
  | MBi -> "Bi"
  | MLocP -> "LocP"
  | MLoc -> "Loc"
  | MPP -> "PP"
  | MP -> "P"
  | MGeiP -> "GeiP"
  | MGei -> "Gei"

let all_mcats =
  [MS; MTP;

   MNP; MNbar; MN; MProperN; MPron; MDet;
   MPossP; MDe; MClP; MNum; MCl;

   MVP; MVbar; MV; MModal; MASP;

   MAP; MA; MDeg; MAdvP; MAdv; MResultP; MResult;

   MNegP; MNeg; MBiP; MBi; MLocP; MLoc; MPP; MP; MGeiP; MGei]

(* -------------------------------------------------------------------------- *)
(* Mandarin syntax rules                                                       *)
(* -------------------------------------------------------------------------- *)

let mandarin_syntax_rules = [
  (* Root/clause.
     Mandarin allows subject-predicate clauses and subjectless/pro-drop clauses.
  *)
  NTRule (MS, [MTP]);
  NTRule (MS, [MVP]);

  NTRule (MTP, [MNP; MVP]);
  NTRule (MTP, [MNP; MAP]);      (* lao3_zu3_mu3 hen3 lao3 *)

  (* ---------------------------------------------------------------------- *)
  (* NP layer                                                                *)
  (* ---------------------------------------------------------------------- *)

  NTRule (MNP, [MPron]);
  NTRule (MNP, [MProperN]);
  NTRule (MNP, [MNbar]);

  NTRule (MNbar, [MN]);

  (* Mandarin bare nouns and modifier+noun structures:
     hao3 jiu3, xiao3 lao3_hu3, zhi3 lao3_hu3, etc.
  *)
  NTRule (MNbar, [MAP; MNbar]);
  NTRule (MNbar, [MN; MNbar]);

  (* Determiner-like modifier:
     na3_zhong3 jiu3
  *)
  NTRule (MNP, [MDet; MNP]);

  (* de0 constructions:
     wo3_men0 de0 jian4_die2
  *)
  NTRule (MNP, [MPossP; MNP]);
  NTRule (MPossP, [MNP; MDe]);

  (* Classifier phrase placeholder. *)
  NTRule (MNP, [MClP; MNP]);
  NTRule (MClP, [MNum; MCl]);

  (* Locative:
     zhan3_lan3_guan3 li3
  *)
  NTRule (MNP, [MNP; MLoc]);
  NTRule (MLocP, [MNP; MLoc]);

  (* ---------------------------------------------------------------------- *)
  (* AP / Adv / Result                                                       *)
  (* ---------------------------------------------------------------------- *)

  NTRule (MAP, [MA]);
  NTRule (MAP, [MDeg; MA]);      (* hen3 hao3, hen3 lao3 *)

  NTRule (MAdvP, [MAdv]);

  NTRule (MResultP, [MResult]);

  (* ---------------------------------------------------------------------- *)
  (* VP layer                                                                *)
  (* ---------------------------------------------------------------------- *)

  NTRule (MVP, [MVbar]);

  NTRule (MVbar, [MV]);
  NTRule (MVbar, [MV; MNP]);        (* mai3 jiu3, mai2 ma3 *)
  NTRule (MVbar, [MV; MResultP]);   (* mai3 hao3 = buy-finish/well *)
  NTRule (MVbar, [MVbar; MNP]);     (* [mai3 hao3] jiu3 *)
  NTRule (MVbar, [MVbar; MASP]);    (* mai3 wan3 le0 *)

  (* Modal/serial-like structures:
     wo3 xiang3 mai3 hao3 jiu3
     ni3 zhi3 xiang3 wo3 mai3 hao3 jiu3
  *)
  NTRule (MVbar, [MModal; MVP]);
  NTRule (MVbar, [MModal; MTP]);

  (* Adverbs *)
  NTRule (MVbar, [MAdvP; MVbar]);

  (* Gei/dative-like structures:
     gei3 wo3 mai3 hao3 jiu3
  *)
  NTRule (MVbar, [MGeiP; MVP]);
  NTRule (MGeiP, [MGei; MNP]);

  (* Negation *)
  NTRule (MVP, [MNegP]);
  NTRule (MNegP, [MNeg; MVP]);

  (* Comparison:
     gou3 bi3 ma3 xiao3
  *)
  NTRule (MVP, [MBiP; MAP]);
  NTRule (MBiP, [MBi; MNP]);

  (* PP/locative adjuncts *)
  NTRule (MPP, [MP; MLocP]);
  NTRule (MVbar, [MPP; MVbar]);

  (* Stative/easy-to-raise style:
     gou3 hen3 hao3 yang3
  *)
  NTRule (MVP, [MAP; MV])
]

(* -------------------------------------------------------------------------- *)
(* Mandarin lexicon for elicitation set                                        *)
(* -------------------------------------------------------------------------- *)

let mandarin_proper_names =
  ["li3"; "lao3_li3"; "xiao3_qiao3_lao3_li3";
   "li3_ma3_suo3"; "mi3_nai3_han3"]

let mandarin_pronouns =
  ["wo3"; "ni3"; "wo3_men0"]

let mandarin_determiners =
  ["na3_zhong3"]

let mandarin_nouns =
  ["jiu3"; "ma3"; "shu1"; "hu3"; "gou3"; "lao3_shu3";
   "nai3"; "guan3"; "zhan3_lan3_guan3"; "zu3_mu3";
   "zhi3"; "suo3_ma3_li3"; "hai3_fei3"; "fei3_shou3";
   "jian4_die2"; "tong2_xue2"; "chao1"]

let mandarin_verbs =
  ["mai3"; "mai2"; "yang3"; "you3"; "xie3"; "deng3";
   "da3_qiu2"; "zhuan3"; "chao3"]

let mandarin_adjectives =
  ["hao3"; "lao3"; "xiao3"; "shao3"; "mei3"; "qiao3";
   "chou3"; "kuai4"]

let mandarin_degree_words =
  ["hen3"]

let mandarin_adverbs =
  ["zhi3"; "yi3"]

let mandarin_modals =
  ["xiang3"]

let mandarin_result_words =
  ["hao3"; "wan3"]

let mandarin_negation =
  ["bu4"]

let mandarin_bi =
  ["bi3"]

let mandarin_gei =
  ["gei3"]

let mandarin_localizers =
  ["li3"; "shang4"; "xia4"]

let mandarin_de =
  ["de0"]

let mandarin_aspect =
  ["le0"]

let mandarin_numbers =
  ["yi1"; "er4"; "san1"]

let mandarin_classifiers =
  ["zhong3"]

let mandarin_prepositions =
  ["zai4"; "cong2"; "dao4"]

let mandarin_terminals =
  List.sort_uniq String.compare
    (mandarin_proper_names
     @ mandarin_pronouns
     @ mandarin_determiners
     @ mandarin_nouns
     @ mandarin_verbs
     @ mandarin_adjectives
     @ mandarin_degree_words
     @ mandarin_adverbs
     @ mandarin_modals
     @ mandarin_result_words
     @ mandarin_negation
     @ mandarin_bi
     @ mandarin_gei
     @ mandarin_localizers
     @ mandarin_de
     @ mandarin_aspect
     @ mandarin_numbers
     @ mandarin_classifiers
     @ mandarin_prepositions)

let mandarin_lexical_rules =
  make_trules MProperN mandarin_proper_names
  @ make_trules MPron mandarin_pronouns
  @ make_trules MDet mandarin_determiners
  @ make_trules MN mandarin_nouns
  @ make_trules MV mandarin_verbs
  @ make_trules MA mandarin_adjectives
  @ make_trules MDeg mandarin_degree_words
  @ make_trules MAdv mandarin_adverbs
  @ make_trules MModal mandarin_modals
  @ make_trules MResult mandarin_result_words
  @ make_trules MNeg mandarin_negation
  @ make_trules MBi mandarin_bi
  @ make_trules MGei mandarin_gei
  @ make_trules MLoc mandarin_localizers
  @ make_trules MDe mandarin_de
  @ make_trules MASP mandarin_aspect
  @ make_trules MNum mandarin_numbers
  @ make_trules MCl mandarin_classifiers
  @ make_trules MP mandarin_prepositions

let mandarin_cfg =
  (
    all_mcats,
    mandarin_terminals,
    MS,
    mandarin_syntax_rules @ mandarin_lexical_rules
  )

(* -------------------------------------------------------------------------- *)
(* Mandarin tone helpers                                                       *)
(* -------------------------------------------------------------------------- *)

type mtone =
  | MT0 | MT1 | MT2 | MT3 | MT4 | MTUnknown

let tone_of_syllable syll =
  let len = String.length syll in
  if len = 0 then MTUnknown
  else
    match syll.[len - 1] with
    | '0' -> MT0
    | '1' -> MT1
    | '2' -> MT2
    | '3' -> MT3
    | '4' -> MT4
    | _ -> MTUnknown

let string_of_mtone tone =
  match tone with
  | MT0 -> "0"
  | MT1 -> "1"
  | MT2 -> "2"
  | MT3 -> "3"
  | MT4 -> "4"
  | MTUnknown -> "?"

let syllables_of_token token =
  String.split_on_char '_' token

let tones_of_token token =
  List.map tone_of_syllable (syllables_of_token token)

let tones_of_sentence sentence =
  list_concat_map tones_of_token (words sentence)

let string_of_tones tones =
  String.concat " " (List.map string_of_mtone tones)

let print_underlying_tones sentence =
  print_string (string_of_tones (tones_of_sentence sentence));
  print_string "\n"
(* -------------------------------------------------------------------------- *)
(* Mandarin testing helpers                                                    *)
(* -------------------------------------------------------------------------- *)

let mandarin_bracketings input =
  bracketings_with show_mcat (fun s -> s) mandarin_cfg input

let mandarin_bracket_sentence sentence =
  bracket_sentence_with show_mcat (fun s -> s) mandarin_cfg sentence

let mandarin_bracket_sentence_from start_symbol sentence =
  bracket_sentence_from_with show_mcat (fun s -> s) mandarin_cfg start_symbol sentence

let print_mandarin_parses sentence =
  print_bracketings (mandarin_bracket_sentence sentence)

let print_mandarin_parses_from start_symbol sentence =
  print_bracketings (mandarin_bracket_sentence_from start_symbol sentence)

let mandarin_parse_count sentence =
  List.length (mandarin_bracket_sentence sentence)

let mandarin_parse_count_from start_symbol sentence =
  List.length (mandarin_bracket_sentence_from start_symbol sentence)
(* -------------------------------------------------------------------------- *)
(* English testing helpers                                                      *)
(* -------------------------------------------------------------------------- *)

let parse_count cfg sentence =
  List.length (bracket_sentence cfg sentence)

let parse_count_from cfg start_symbol sentence =
  List.length (bracket_sentence_from cfg start_symbol sentence)

let print_sentence_parses cfg sentence =
  print_bracketings (bracket_sentence cfg sentence)

let print_sentence_parses_from cfg start_symbol sentence =
  print_bracketings (bracket_sentence_from cfg start_symbol sentence)

(* -------------------------------------------------------------------------- *)
(* Mandarin 3TS analysis layer: syntax -> prosody -> surface tones             *)
(* -------------------------------------------------------------------------- *)

(*
   This section is intentionally stimulus-specific.  It keeps the parser core
   unchanged, but adds a larger Mandarin elicitation grammar plus a prosody/T3S
   layer.  The key user-facing functions are:

     analyze_mandarin "mai3 hao3 jiu3";;
     analyze_elicitation ();;

   Input convention:
   - Use pinyin with tone numbers.
   - Separate syntactic words with spaces.
   - Use underscores inside lexicalized multisyllabic words when you want syntax
     to treat them as one word, e.g. zhan3_lan3_guan3.
   - The tone module still splits underscores back into syllables.
*)

(* -------------------------------------------------------------------------- *)
(* Small list utilities                                                        *)
(* -------------------------------------------------------------------------- *)

let rec repeat_string n x =
  if n <= 0 then [] else x :: repeat_string (n - 1) x

let unique_strings xs =
  List.fold_right
    (fun x acc -> if List.mem x acc then acc else x :: acc)
    xs
    []

let unique_by_string key xs =
  let rec helper seen rest =
    match rest with
    | [] -> []
    | x :: xs ->
        let k = key x in
        if List.mem k seen then helper seen xs
        else x :: helper (k :: seen) xs
  in
  helper [] xs

let join_words xs = String.concat " " xs

let string_of_int_list xs =
  String.concat "-" (List.map string_of_int xs)

(* -------------------------------------------------------------------------- *)
(* Expanded Mandarin CFG for the elicitation set                               *)
(* -------------------------------------------------------------------------- *)

let mandarin3_syntax_rules =
  mandarin_syntax_rules @ [
    (* Fragment roots, useful because many elicitation items are NPs/LocPs. *)
    NTRule (MS, [MNP]);
    NTRule (MS, [MLocP]);
    NTRule (MS, [MAP]);

    (* Adjectives can modify a full NP, including proper names. *)
    NTRule (MNP, [MAP; MNP]);

    (* Proper-name and noun compounding support. *)
    NTRule (MNbar, [MProperN]);
    NTRule (MNbar, [MProperN; MNbar]);
    NTRule (MNP, [MN; MNP]);

    (* More flexible serial/modal structures for the elicitation strings. *)
    NTRule (MVP, [MAdvP; MVP]);
    NTRule (MVP, [MGeiP; MVP]);
    NTRule (MVbar, [MV; MVP]);
    NTRule (MVbar, [MV; MTP]);

    (* Stative adjective predicate as a VP-like predicate. *)
    NTRule (MVP, [MAP]);

    (* A simple analysis of “already died of old age” style predicates. *)
    NTRule (MVbar, [MAdvP; MVbar])
  ]

let mandarin3_proper_names =
  unique_strings
    (mandarin_proper_names @
     ["lao3_li3"; "xiao3_lao3_li3"; "xiao3_qiao3_lao3_li3";
      "li3_ma3_suo3"; "mi3_nai3_han3"])

let mandarin3_pronouns =
  unique_strings (mandarin_pronouns @ ["wo3"; "ni3"; "wo3_men0"])

let mandarin3_determiners =
  unique_strings (mandarin_determiners @ ["na3_zhong3"])

let mandarin3_nouns =
  unique_strings
    (mandarin_nouns @ mandarin3_proper_names @
     ["zhan3"; "lan3"; "guan3"; "zhan3_lan3"; "zhan3_lan3_guan3";
      "lao3_hu3"; "zu3_mu3"; "lao3_zu3_mu3";
      "xiao3_chou3"; "chou3";
      "jia3_chao1"; "jia3_chao3"; "chao1"; "chao3";
      "lao3_si3"; "si3";
      "suo3_ma3_li3"; "hai3_fei3"; "fei3_shou3";
      "li3_ma3_suo3"; "mi3_nai3_han3"])

let mandarin3_verbs =
  unique_strings
    (mandarin_verbs @
     ["mai3"; "mai2"; "gei3"; "xiang3"; "zhuan3"; "chao3";
      "si3"; "lao3_si3"; "yang3"; "you3"])

let mandarin3_adjectives =
  unique_strings
    (mandarin_adjectives @
     ["hao3"; "lao3"; "xiao3"; "shao3"; "mei3"; "qiao3";
      "chou3"; "jia3"; "kuai4"])

let mandarin3_degree_words =
  unique_strings (mandarin_degree_words @ ["hen3"; "hao3"])

let mandarin3_adverbs =
  unique_strings (mandarin_adverbs @ ["zhi3"; "yi3"])

let mandarin3_modals =
  unique_strings (mandarin_modals @ ["xiang3"])

let mandarin3_result_words =
  unique_strings (mandarin_result_words @ ["hao3"; "wan3"])

let mandarin3_negation =
  unique_strings (mandarin_negation @ ["bu4"])

let mandarin3_bi = unique_strings (mandarin_bi @ ["bi3"])
let mandarin3_gei = unique_strings (mandarin_gei @ ["gei3"])
let mandarin3_localizers = unique_strings (mandarin_localizers @ ["li3"; "shang4"; "xia4"])
let mandarin3_de = unique_strings (mandarin_de @ ["de0"])
let mandarin3_aspect = unique_strings (mandarin_aspect @ ["le0"])
let mandarin3_numbers = unique_strings (mandarin_numbers @ ["yi1"; "er4"; "san1"])
let mandarin3_classifiers = unique_strings (mandarin_classifiers @ ["zhong3"])
let mandarin3_prepositions = unique_strings (mandarin_prepositions @ ["zai4"; "cong2"; "dao4"])

let mandarin3_terminals =
  List.sort_uniq String.compare
    (mandarin3_proper_names
     @ mandarin3_pronouns
     @ mandarin3_determiners
     @ mandarin3_nouns
     @ mandarin3_verbs
     @ mandarin3_adjectives
     @ mandarin3_degree_words
     @ mandarin3_adverbs
     @ mandarin3_modals
     @ mandarin3_result_words
     @ mandarin3_negation
     @ mandarin3_bi
     @ mandarin3_gei
     @ mandarin3_localizers
     @ mandarin3_de
     @ mandarin3_aspect
     @ mandarin3_numbers
     @ mandarin3_classifiers
     @ mandarin3_prepositions)

let mandarin3_lexical_rules =
  make_trules MProperN mandarin3_proper_names
  @ make_trules MPron mandarin3_pronouns
  @ make_trules MDet mandarin3_determiners
  @ make_trules MN mandarin3_nouns
  @ make_trules MV mandarin3_verbs
  @ make_trules MA mandarin3_adjectives
  @ make_trules MDeg mandarin3_degree_words
  @ make_trules MAdv mandarin3_adverbs
  @ make_trules MModal mandarin3_modals
  @ make_trules MResult mandarin3_result_words
  @ make_trules MNeg mandarin3_negation
  @ make_trules MBi mandarin3_bi
  @ make_trules MGei mandarin3_gei
  @ make_trules MLoc mandarin3_localizers
  @ make_trules MDe mandarin3_de
  @ make_trules MASP mandarin3_aspect
  @ make_trules MNum mandarin3_numbers
  @ make_trules MCl mandarin3_classifiers
  @ make_trules MP mandarin3_prepositions

let mandarin3_cfg =
  (
    all_mcats,
    mandarin3_terminals,
    MS,
    mandarin3_syntax_rules @ mandarin3_lexical_rules
  )

(* -------------------------------------------------------------------------- *)
(* Syntax collection helpers                                                   *)
(* -------------------------------------------------------------------------- *)

let mandarin3_start_symbols =
  [MS; MTP; MVP; MNP; MLocP; MAP]

let mandarin3_parse_trees_all_starts sentence =
  let input = words sentence in
  let trees =
    list_concat_map
      (fun start_symbol -> parse_trees_chart_from mandarin3_cfg start_symbol input)
      mandarin3_start_symbols
  in
  unique_by_string (tree_to_brackets show_mcat (fun s -> s)) trees

let mandarin3_bracket_sentence sentence =
  List.map
    (tree_to_brackets show_mcat (fun s -> s))
    (mandarin3_parse_trees_all_starts sentence)

let mandarin3_parse_count sentence =
  List.length (mandarin3_bracket_sentence sentence)

let print_mandarin3_syntax sentence =
  print_bracketings (mandarin3_bracket_sentence sentence)

(* -------------------------------------------------------------------------- *)
(* Prosodic domain candidates                                                  *)
(* -------------------------------------------------------------------------- *)

let syllables_of_sentence sentence =
  list_concat_map syllables_of_token (words sentence)

let tones_of_syllables sylls =
  List.map tone_of_syllable sylls

let rec split_n n xs =
  if n = 0 then Some ([], xs)
  else
    match xs with
    | [] -> None
    | x :: rest ->
        (match split_n (n - 1) rest with
         | None -> None
         | Some (front, back) -> Some (x :: front, back))

let rec domain_partitions max_size sylls =
  match sylls with
  | [] -> [[]]
  | _ ->
      list_concat_map
        (fun n ->
           match split_n n sylls with
           | None -> []
           | Some (front, back) ->
               List.map
                 (fun rest_partition -> front :: rest_partition)
                 (domain_partitions max_size back))
        (range 1 (max_size + 1))

let token_domains sentence =
  List.map syllables_of_token (words sentence)

let whole_domain sentence =
  [syllables_of_sentence sentence]

let left_binary_domains sentence =
  let rec helper sylls =
    match sylls with
    | [] -> []
    | [x] -> [[x]]
    | x :: y :: rest -> [x; y] :: helper rest
  in
  helper (syllables_of_sentence sentence)

let right_binary_domains sentence =
  let rec helper sylls =
    match sylls with
    | [] -> []
    | [x] -> [[x]]
    | _ ->
        let len = List.length sylls in
        let prefix = take (len - 2) sylls in
        let pair = drop (len - 2) sylls in
        helper prefix @ [pair]
  in
  helper (syllables_of_sentence sentence)

let domain_label domain =
  match List.length domain with
  | 0 -> "Empty"
  | 1 -> "sigma"
  | 2 -> "Ft"
  | 3 -> "Ft'"
  | _ -> "Ph"

let string_of_domain domain =
  "(" ^ String.concat " " domain ^ ")" ^ domain_label domain

let string_of_domains domains =
  "[Utt [Ph " ^ String.concat " " (List.map string_of_domain domains) ^ "]]"

let canonical_sentence sentence =
  join_words (words sentence)

(* These are the hand-specified mappings that correspond to the predictions in
   the paper.  Generic sentences fall back to generated p-domain candidates. *)
let special_domain_candidates sentence =
  match canonical_sentence sentence with
  | "mai3 hao3 jiu3" ->
      [ ("left-branching/resultative: [[mai3 hao3] jiu3]", [["mai3"; "hao3"; "jiu3"]]);
        ("right-branching/object NP: [mai3 [hao3 jiu3]]", [["mai3"]; ["hao3"; "jiu3"]]) ]

  | "lao3_li3 mai3 hao3 jiu3" ->
      [ ("proper name + right-branching object NP", [["lao3"; "li3"]; ["mai3"]; ["hao3"; "jiu3"]]);
        ("proper name + resultative VP", [["lao3"; "li3"]; ["mai3"; "hao3"; "jiu3"]]) ]

  | "lao3 li3 mai3 hao3 jiu3" ->
      [ ("proper name + right-branching object NP", [["lao3"; "li3"]; ["mai3"]; ["hao3"; "jiu3"]]);
        ("proper name + resultative VP", [["lao3"; "li3"]; ["mai3"; "hao3"; "jiu3"]]) ]

  | "zhan3_lan3_guan3 li3" ->
      [ ("lexicalized left-branching LocP: [[[zhan3 lan3] guan3] li3]", [["zhan3"; "lan3"; "guan3"; "li3"]]) ]

  | "zhan3 lan3 guan3 li3" ->
      [ ("lexicalized left-branching LocP: [[[zhan3 lan3] guan3] li3]", [["zhan3"; "lan3"; "guan3"; "li3"]]) ]

  | "xiao3 zhi3 lao3 hu3" ->
      [ ("balanced binary feet: (xiao3 zhi3) (lao3 hu3)", [["xiao3"; "zhi3"]; ["lao3"; "hu3"]]);
        ("right-heavy prosody: xiao3 (zhi3 lao3 hu3)", [["xiao3"]; ["zhi3"; "lao3"; "hu3"]]);
        ("single large phrase", [["xiao3"; "zhi3"; "lao3"; "hu3"]]) ]

  | "zhi3 lao3 hu3" ->
      [ ("paper + tiger as one phrase", [["zhi3"; "lao3"; "hu3"]]);
        ("paper outside tiger foot", [["zhi3"]; ["lao3"; "hu3"]]) ]

  | "lao3 hu3" ->
      [ ("tiger lexical foot", [["lao3"; "hu3"]]) ]

  | "lao3_hu3" ->
      [ ("tiger lexical foot", [["lao3"; "hu3"]]) ]

  | _ -> []

let generated_domain_candidates sentence =
  let sylls = syllables_of_sentence sentence in
  let n = List.length sylls in
  if n <= 7 then
    List.map
      (fun domains -> ("generated partition", domains))
      (domain_partitions 4 sylls)
  else
    [ ("single large phrase", whole_domain sentence);
      ("left-to-right binary feet", left_binary_domains sentence);
      ("right-to-left binary feet", right_binary_domains sentence);
      ("lexical token domains", token_domains sentence) ]

let domain_candidates sentence =
  let special = special_domain_candidates sentence in
  let raw =
    if special <> [] then special
    else generated_domain_candidates sentence
  in
  unique_by_string
    (fun (_name, domains) -> string_of_domains domains)
    raw

(* -------------------------------------------------------------------------- *)
(* Simplified 3TS surface evaluator                                             *)
(* -------------------------------------------------------------------------- *)

let rec take_t3_run tones =
  match tones with
  | MT3 :: rest ->
      let (run, after_run) = take_t3_run rest in
      (MT3 :: run, after_run)
  | _ -> ([], tones)

let rec surface_of_tones_in_domain tones =
  match tones with
  | [] -> []
  | MT3 :: _ ->
      let (run, rest) = take_t3_run tones in
      let n = List.length run in
      let this_run =
        if n <= 1 then ["3"]
        else repeat_string (n - 1) "2" @ ["3"]
      in
      this_run @ surface_of_tones_in_domain rest
  | tone :: rest ->
      string_of_mtone tone :: surface_of_tones_in_domain rest

let surface_of_domain domain =
  surface_of_tones_in_domain (tones_of_syllables domain)

let surface_of_domains domains =
  list_concat_map surface_of_domain domains

let string_of_surface surface =
  String.concat "-" surface

let print_tone_line label surface =
  print_string (label ^ string_of_surface surface ^ "\n")

(* -------------------------------------------------------------------------- *)
(* User-facing Mandarin analysis                                               *)
(* -------------------------------------------------------------------------- *)

type mandarin_analysis = {
  analysis_name : string;
  analysis_domains : string list list;
  analysis_surface : string list;
}

let make_analysis (name, domains) =
  { analysis_name = name;
    analysis_domains = domains;
    analysis_surface = surface_of_domains domains }

let mandarin_analyses sentence =
  List.map make_analysis (domain_candidates sentence)

let print_limited_bracketings max_to_print bracket_list =
  let rec helper i rest =
    match rest with
    | [] -> ()
    | br :: xs ->
        if i > max_to_print then
          print_string
            ("... " ^ string_of_int (List.length rest) ^ " more syntax candidates omitted.\n\n")
        else begin
          print_string ("Syntax " ^ string_of_int i ^ ":\n");
          print_string br;
          print_string "\n\n";
          helper (i + 1) xs
        end
  in
  helper 1 bracket_list

let analyze_mandarin sentence =
  let syntax = mandarin3_bracket_sentence sentence in
  let analyses = mandarin_analyses sentence in
  print_string "============================================================\n";
  print_string ("Input: " ^ sentence ^ "\n");
  print_string ("Tokens: " ^ String.concat ", " (words sentence) ^ "\n");
  print_string ("Syllables: " ^ String.concat ", " (syllables_of_sentence sentence) ^ "\n");
  print_string ("Underlying tones: " ^ string_of_tones (tones_of_sentence sentence) ^ "\n\n");

  print_string ("Possible syntax candidates: " ^ string_of_int (List.length syntax) ^ "\n");
  if syntax = [] then
    print_string "No syntax tree found with the current stimulus grammar. Prosody/T3S is still shown from the syllable string.\n\n"
  else
    print_limited_bracketings 20 syntax;

  print_string ("Possible prosodic domains + surface forms: " ^ string_of_int (List.length analyses) ^ "\n\n");
  List.iteri
    (fun i a ->
       print_string ("Candidate " ^ string_of_int (i + 1) ^ ": " ^ a.analysis_name ^ "\n");
       print_string ("P-domain: " ^ string_of_domains a.analysis_domains ^ "\n");
       print_tone_line "Surface: " a.analysis_surface;
       print_string "\n")
    analyses;
  print_string "============================================================\n\n"

let mandarin_surface_forms sentence =
  unique_strings
    (List.map
       (fun a -> string_of_surface a.analysis_surface)
       (mandarin_analyses sentence))

let print_mandarin_surface_forms sentence =
  List.iter
    (fun s -> print_string (s ^ "\n"))
    (mandarin_surface_forms sentence)

(* -------------------------------------------------------------------------- *)
(* Elicitation stimuli                                                         *)
(* -------------------------------------------------------------------------- *)

let elicitation_stimuli = [
  ("1. Lao Li buys wine", "lao3_li3 mai3 jiu3");
  ("2. Lao Li buys good wine", "lao3_li3 mai3 hao3 jiu3");
  ("3. Little Old Li bought too little good wine", "xiao3_lao3_li3 mai3 shao3 hao3 jiu3");
  ("4. Old grandmother is very old", "lao3_zu3_mu3 hao3 lao3");
  ("5. Which kind of wine is good", "na3_zhong3 jiu3 hao3");
  ("6. Clever little Old Li bought me good exquisite wine", "xiao3_qiao3_lao3_li3 gei3 wo3 mai3 hao3 mei3 jiu3");

  ("Basic 1. buy horse", "mai3 ma3");
  ("Basic 2. bury horse", "mai2 ma3");
  ("Basic 3. buy good wine", "mai3 hao3 jiu3");
  ("Basic 4. good wine bought late", "hao3 jiu3 mai3 wan3 le0");
  ("Basic 5. Lao Li buys good wine", "lao3_li3 mai3 hao3 jiu3");
  ("Basic 6. I want to buy good wine", "wo3 xiang3 mai3 hao3 jiu3");
  ("Basic 7. You only want me to buy good wine", "ni3 zhi3 xiang3 wo3 mai3 hao3 jiu3");
  ("Basic 8. tiger", "lao3 hu3");
  ("Basic 9. paper tiger", "zhi3 lao3 hu3");
  ("Basic 10. small paper tiger", "xiao3 zhi3 lao3 hu3");
  ("Basic 11. exhibition hall", "zhan3_lan3_guan3");
  ("Basic 12. inside exhibition hall", "zhan3_lan3_guan3 li3");
  ("Basic 13. dogs are easy to raise", "gou3 hen3 hao3 yang3");
  ("Basic 14. dogs are smaller than horses", "gou3 bi3 ma3 xiao3");
  ("Basic 15. Lao Li buys a book", "lao3_li3 mai3 shu1");
  ("Basic 16. the clown is prettier than me", "xiao3_chou3 bi3 wo3 mei3");
  ("Basic 17. the mouse has milk", "lao3_shu3 you3 nai3");
  ("Basic 18. dogs are faster than horses", "gou3 bi3 ma3 kuai4");

  ("Focus frame. I want to buy good wine", "wo3 xiang3 mai3 hao3 jiu3");

  ("Unfamiliar 1. Somalia", "suo3_ma3_li3");
  ("Unfamiliar 2. pirate", "hai3_fei3");
  ("Unfamiliar 3. bandit chieftain", "fei3_shou3");
  ("Unfamiliar 4. Rimasso", "li3_ma3_suo3");
  ("Unfamiliar 5. Minahan", "mi3_nai3_han3");
  ("Unfamiliar 6. long unfamiliar phrase", "suo3_ma3_li3 hai3_fei3 fei3_shou3 li3_ma3_suo3 mi3_nai3_han3 yi3 lao3_si3");
  ("Unfamiliar 7. transfer fake money", "gei3 wo3 zhuan3 jia3_chao1")
]

let analyze_elicitation () =
  List.iter
    (fun (label, sentence) ->
       print_string ("\n### " ^ label ^ " ###\n");
       analyze_mandarin sentence)
    elicitation_stimuli

let print_elicitation_surface_summary () =
  List.iter
    (fun (label, sentence) ->
       print_string (label ^ "\n");
       print_string ("  Input: " ^ sentence ^ "\n");
       print_string ("  Surfaces: " ^ String.concat ", " (mandarin_surface_forms sentence) ^ "\n\n"))
    elicitation_stimuli

(* -------------------------------------------------------------------------- *)
(* Fully implemented constraint scoring/ranking layer                          *)
(* -------------------------------------------------------------------------- *)

(*
   This layer turns the theory-facing mapping constraints into executable
   scoring functions.  Lower violation counts produce higher scores.

   The current implementation scores PROSODIC CANDIDATES, not full human
   probability.  It is therefore a grammar/prosody plausibility ranking.
*)

type constraint_profile = {
  align_r_violations : int;
  right_edge_closure_pressure : int;
  lexical_cohesion_violations : int;
  binarity_ft_violations : int;
  large_domain_violations : int;
  weighted_penalty : int;
  ranking_score : int;
}

type ranked_mandarin_analysis = {
  ranked_analysis : mandarin_analysis;
  ranked_profile : constraint_profile;
}

let rec int_mem x xs =
  match xs with
  | [] -> false
  | y :: ys -> x = y || int_mem x ys

let rec count_list f xs =
  match xs with
  | [] -> 0
  | x :: rest -> (if f x then 1 else 0) + count_list f rest

let domain_spans domains =
  let rec helper pos ds =
    match ds with
    | [] -> []
    | d :: rest ->
        let next_pos = pos + List.length d in
        (d, pos, next_pos) :: helper next_pos rest
  in
  helper 0 domains

let domain_end_positions domains =
  List.map (fun (_d, _start_pos, end_pos) -> end_pos) (domain_spans domains)

let interior_domain_end_positions domains =
  let ends = domain_end_positions domains in
  match List.rev ends with
  | [] -> []
  | _final_end :: reversed_interior -> List.rev reversed_interior

let token_syllable_spans sentence =
  let rec helper pos toks =
    match toks with
    | [] -> []
    | tok :: rest ->
        let len = List.length (syllables_of_token tok) in
        let next_pos = pos + len in
        (tok, pos, next_pos) :: helper next_pos rest
  in
  helper 0 (words sentence)

let token_end_positions sentence =
  List.map (fun (_tok, _start_pos, end_pos) -> end_pos) (token_syllable_spans sentence)

let boundary_is_inside_span boundary start_pos end_pos =
  start_pos < boundary && boundary < end_pos

let lexical_cohesion_violations sentence domains =
  let boundaries = interior_domain_end_positions domains in
  let token_spans = token_syllable_spans sentence in
  List.fold_left
    (fun total (tok, start_pos, end_pos) ->
       let syll_count = List.length (syllables_of_token tok) in
       if syll_count <= 1 then total
       else
         total + count_list
           (fun boundary -> boundary_is_inside_span boundary start_pos end_pos)
           boundaries)
    0
    token_spans

let align_r_violations sentence domains =
  let boundaries = interior_domain_end_positions domains in
  let token_ends = token_end_positions sentence in
  count_list (fun boundary -> not (int_mem boundary token_ends)) boundaries

let binarity_ft_violations domains =
  List.fold_left
    (fun total domain ->
       let len = List.length domain in
       total +
         match len with
         | 0 -> 0
         | 1 -> 1       (* singleton sigma: allowed, but not ideal *)
         | 2 -> 0       (* ideal Ft *)
         | 3 -> 1       (* Ft': possible, but less ideal than binary Ft *)
         | _ -> 2 + (len - 3))
    0
    domains

let large_domain_violations domains =
  List.fold_left
    (fun total domain ->
       let len = List.length domain in
       if len <= 3 then total else total + (len - 3))
    0
    domains

let right_edge_closure_pressure domains =
  match domains with
  | [] -> 0
  | [domain] ->
      let len = List.length domain in
      if len >= 4 then 2 else 0
  | first :: second :: rest ->
      let first_len = List.length first in
      let second_len = List.length second in
      let base =
        if first_len = 1 && second_len >= 3 then 1 else 0
      in
      let extra_large = large_domain_violations rest in
      base + extra_large

let score_constraint_profile profile =
  (* Weights are theory-driven but intentionally simple.
     Higher weights mean more serious violations.
  *)
  let penalty =
      (25 * profile.lexical_cohesion_violations)
    + (15 * profile.align_r_violations)
    + (8 * profile.binarity_ft_violations)
    + (6 * profile.large_domain_violations)
    + (5 * profile.right_edge_closure_pressure)
  in
  let score = max 0 (100 - penalty) in
  (penalty, score)

let constraint_profile_of_domains sentence domains =
  let base_profile = {
    align_r_violations = align_r_violations sentence domains;
    right_edge_closure_pressure = right_edge_closure_pressure domains;
    lexical_cohesion_violations = lexical_cohesion_violations sentence domains;
    binarity_ft_violations = binarity_ft_violations domains;
    large_domain_violations = large_domain_violations domains;
    weighted_penalty = 0;
    ranking_score = 0;
  } in
  let (penalty, score) = score_constraint_profile base_profile in
  { base_profile with weighted_penalty = penalty; ranking_score = score }

let rank_analysis sentence analysis =
  { ranked_analysis = analysis;
    ranked_profile = constraint_profile_of_domains sentence analysis.analysis_domains }

let compare_ranked_analysis a b =
  let score_cmp = compare b.ranked_profile.ranking_score a.ranked_profile.ranking_score in
  if score_cmp <> 0 then score_cmp
  else
    let penalty_cmp = compare a.ranked_profile.weighted_penalty b.ranked_profile.weighted_penalty in
    if penalty_cmp <> 0 then penalty_cmp
    else compare a.ranked_analysis.analysis_name b.ranked_analysis.analysis_name

let ranked_mandarin_analyses sentence =
  List.sort compare_ranked_analysis
    (List.map (rank_analysis sentence) (mandarin_analyses sentence))

let string_of_constraint_profile profile =
  "score=" ^ string_of_int profile.ranking_score
  ^ "; penalty=" ^ string_of_int profile.weighted_penalty
  ^ "; ALIGN-R=" ^ string_of_int profile.align_r_violations
  ^ "; RIGHT-EDGE-CLOSURE=" ^ string_of_int profile.right_edge_closure_pressure
  ^ "; LEXICAL-COHESION=" ^ string_of_int profile.lexical_cohesion_violations
  ^ "; BINARITY-FT=" ^ string_of_int profile.binarity_ft_violations
  ^ "; MINIMIZE-LARGE-DOMAINS=" ^ string_of_int profile.large_domain_violations

let print_ranked_constraint_line profile =
  print_string ("Ranking: " ^ string_of_constraint_profile profile ^ "\n")

let ranked_mandarin_surface_forms sentence =
  List.map
    (fun ranked -> string_of_surface ranked.ranked_analysis.analysis_surface)
    (unique_by_string
       (fun ranked -> string_of_surface ranked.ranked_analysis.analysis_surface)
       (ranked_mandarin_analyses sentence))

(* -------------------------------------------------------------------------- *)
(* Optional syntax-tree closure scoring                                        *)
(* -------------------------------------------------------------------------- *)

let rec tree_span_and_closure_ends tree start_pos =
  match tree with
  | Leaf (_, _) -> (start_pos + 1, [])
  | Node (_, children) ->
      let (end_pos, child_closures) =
        List.fold_left
          (fun (pos, closures) child ->
             let (new_pos, child_closure) = tree_span_and_closure_ends child pos in
             (new_pos, closures @ child_closure))
          (start_pos, [])
          children
      in
      (end_pos, end_pos :: child_closures)

let closure_count_at edge closures =
  count_list (fun x -> x = edge) closures

let max_right_edge_closure_count tree =
  let (end_pos, closures) = tree_span_and_closure_ends tree 0 in
  let edges = range 1 (end_pos + 1) in
  minimum_or 0 (List.map (fun x -> -x) []) |> ignore;
  List.fold_left
    (fun current_max edge -> max current_max (closure_count_at edge closures))
    0
    edges

let syntax_tree_memory_weight tree =
  let (_end_pos, closures) = tree_span_and_closure_ends tree 0 in
  let total_closures = List.length closures in
  let max_closure = max_right_edge_closure_count tree in
  total_closures + (2 * max_closure)

let ranked_mandarin_syntax_trees sentence =
  List.sort
    (fun a b -> compare (syntax_tree_memory_weight a) (syntax_tree_memory_weight b))
    (mandarin3_parse_trees_all_starts sentence)

let print_ranked_mandarin_syntax max_to_print sentence =
  let trees = ranked_mandarin_syntax_trees sentence in
  let rec helper i rest =
    match rest with
    | [] -> ()
    | tree :: xs ->
        if i > max_to_print then
          print_string
            ("... " ^ string_of_int (List.length rest) ^ " more ranked syntax candidates omitted.\n\n")
        else begin
          print_string ("Syntax " ^ string_of_int i ^ " ");
          print_string ("memory_weight=" ^ string_of_int (syntax_tree_memory_weight tree));
          print_string ("; max_right_edge_closure=" ^ string_of_int (max_right_edge_closure_count tree));
          print_string "\n";
          print_string (tree_to_brackets show_mcat (fun s -> s) tree);
          print_string "\n\n";
          helper (i + 1) xs
        end
  in
  helper 1 trees

(* -------------------------------------------------------------------------- *)
(* Redefined user-facing functions with ranked output                          *)
(* -------------------------------------------------------------------------- *)

let analyze_mandarin sentence =
  let syntax = mandarin3_bracket_sentence sentence in
  let analyses = ranked_mandarin_analyses sentence in
  print_string "============================================================\n";
  print_string ("Input: " ^ sentence ^ "\n");
  print_string ("Tokens: " ^ String.concat ", " (words sentence) ^ "\n");
  print_string ("Syllables: " ^ String.concat ", " (syllables_of_sentence sentence) ^ "\n");
  print_string ("Underlying tones: " ^ string_of_tones (tones_of_sentence sentence) ^ "\n\n");

  print_string ("Possible syntax candidates: " ^ string_of_int (List.length syntax) ^ "\n");
  if syntax = [] then
    print_string "No syntax tree found with the current stimulus grammar. Prosody/T3S is still shown from the syllable string.\n\n"
  else
    print_limited_bracketings 20 syntax;

  print_string ("Ranked prosodic domains + surface forms: " ^ string_of_int (List.length analyses) ^ "\n\n");
  List.iteri
    (fun i ranked ->
       let a = ranked.ranked_analysis in
       print_string ("Candidate " ^ string_of_int (i + 1) ^ ": " ^ a.analysis_name ^ "\n");
       print_ranked_constraint_line ranked.ranked_profile;
       print_string ("P-domain: " ^ string_of_domains a.analysis_domains ^ "\n");
       print_tone_line "Surface: " a.analysis_surface;
       print_string "\n")
    analyses;
  print_string "============================================================\n\n"

let mandarin_surface_forms sentence =
  ranked_mandarin_surface_forms sentence

let print_mandarin_surface_forms sentence =
  List.iter
    (fun s -> print_string (s ^ "\n"))
    (mandarin_surface_forms sentence)

let print_elicitation_surface_summary () =
  List.iter
    (fun (label, sentence) ->
       print_string (label ^ "\n");
       print_string ("  Input: " ^ sentence ^ "\n");
       print_string ("  Ranked surfaces: " ^ String.concat ", " (mandarin_surface_forms sentence) ^ "\n\n"))
    elicitation_stimuli

(* -------------------------------------------------------------------------- *)
(* Integrated syntax -> prosody ranking                                        *)
(* -------------------------------------------------------------------------- *)

(*
   The earlier ranked analyzer ranks prosodic candidates for the sentence.
   This layer integrates syntax and prosody more tightly:

   1. choose the best ranked syntactic trees,
   2. extract syllable spans and right edges from each tree,
   3. generate possible prosodic domains from those syntactic spans,
   4. score each syntax+prosody pair,
   5. evaluate 3TS for the resulting p-domains.
*)

let rec leaves_of_tree tree =
  match tree with
  | Leaf (_, terminal) -> [terminal]
  | Node (_, children) -> list_concat_map leaves_of_tree children

let rec nth_or default xs n =
  match xs with
  | [] -> default
  | x :: rest -> if n = 0 then x else nth_or default rest (n - 1)

let syllable_boundaries_of_tokens toks =
  let rec helper pos remaining =
    match remaining with
    | [] -> [pos]
    | tok :: rest ->
        pos :: helper (pos + List.length (syllables_of_token tok)) rest
  in
  helper 0 toks

type syntax_span = {
  syntax_span_label : mcat;
  syntax_start_token : int;
  syntax_end_token : int;
  syntax_start_syll : int;
  syntax_end_syll : int;
}

let rec tree_token_spans tree start_pos =
  match tree with
  | Leaf (_, _) -> (start_pos + 1, [])
  | Node (nt, children) ->
      let (end_pos, child_spans) =
        List.fold_left
          (fun (pos, spans) child ->
             let (new_pos, new_spans) = tree_token_spans child pos in
             (new_pos, spans @ new_spans))
          (start_pos, [])
          children
      in
      (end_pos, (nt, start_pos, end_pos) :: child_spans)

let syntax_spans_from_tree tree =
  let toks = leaves_of_tree tree in
  let boundaries = syllable_boundaries_of_tokens toks in
  let (_end_pos, token_spans) = tree_token_spans tree 0 in
  List.map
    (fun (nt, start_tok, end_tok) ->
       { syntax_span_label = nt;
         syntax_start_token = start_tok;
         syntax_end_token = end_tok;
         syntax_start_syll = nth_or 0 boundaries start_tok;
         syntax_end_syll = nth_or 0 boundaries end_tok })
    token_spans

let unique_ints xs =
  let rec helper seen rest =
    match rest with
    | [] -> seen
    | x :: xs ->
        if int_mem x seen then helper seen xs
        else helper (x :: seen) xs
  in
  List.sort compare (helper [] xs)

let syntax_right_edges_from_tree tree =
  unique_strings
    (List.map string_of_int
       (List.map (fun span -> span.syntax_end_syll) (syntax_spans_from_tree tree)))

let syntactic_edge_is_present tree boundary =
  int_mem boundary
    (List.map
       (fun s -> s.syntax_end_syll)
       (syntax_spans_from_tree tree))

let slice_list start_pos end_pos xs =
  take (end_pos - start_pos) (drop start_pos xs)

let domains_from_boundaries sylls boundaries =
  let int_boundaries = unique_ints boundaries in
  let rec helper bs =
    match bs with
    | start_pos :: end_pos :: rest ->
        slice_list start_pos end_pos sylls :: helper (end_pos :: rest)
    | _ -> []
  in
  helper int_boundaries

let domains_around_span sylls start_pos end_pos =
  let prefix = slice_list 0 start_pos sylls in
  let middle = slice_list start_pos end_pos sylls in
  let suffix = slice_list end_pos (List.length sylls) sylls in
  left_binary_domains (String.concat " " prefix)
  @ [middle]
  @ left_binary_domains (String.concat " " suffix)

let localizer_start_positions sentence =
  map_maybe
    (fun (tok, start_pos, _end_pos) ->
       if List.mem tok mandarin3_localizers then Some start_pos else None)
    (token_syllable_spans sentence)

let localizer_attachment_violations sentence domains =
  let boundaries = interior_domain_end_positions domains in
  let starts = localizer_start_positions sentence in
  count_list (fun boundary -> int_mem boundary starts) boundaries


(* Modifier-head cohesion is needed for cases like xiao3 zhi3 lao3 hu3.
   The syntax says lao3 hu3 is a close AP+N unit inside the NP. Pure
   phonological binarity may otherwise prefer candidates that strand hu3 or
   split lao3 away from hu3. This interface constraint penalizes prosodic
   boundaries inside a minimal modifier-head nominal unit, e.g. [AP lao3]
   [N hu3] or [AP hao3] [N jiu3]. *)
let tree_leaf_count tree =
  List.length (leaves_of_tree tree)

let is_modifier_tree tree =
  match tree with
  | Node (MAP, _) -> true
  | Leaf (MA, _) -> true
  | Node (MDet, _) -> true
  | Leaf (MDet, _) -> true
  | _ -> false

let is_nominal_tree tree =
  match tree with
  | Node (MNP, _) -> true
  | Node (MNbar, _) -> true
  | Leaf (MN, _) -> true
  | Leaf (MProperN, _) -> true
  | _ -> false

let is_modifier_head_parent nt =
  match nt with
  | MNP -> true
  | MNbar -> true
  | _ -> false

let modifier_head_token_spans tree =
  let rec helper start_pos tr =
    match tr with
    | Leaf (_, _) -> (start_pos + 1, [])
    | Node (nt, children) ->
        let rec scan pos kids =
          match kids with
          | [] -> (pos, [], [])
          | child :: rest ->
              let child_start = pos in
              let (child_end, child_spans) = helper child_start child in
              let (final_end, child_infos, rest_spans) = scan child_end rest in
              (final_end,
               (child, child_start, child_end) :: child_infos,
               child_spans @ rest_spans)
        in
        let (end_pos, child_infos, spans_from_children) = scan start_pos children in
        let own_spans =
          match child_infos with
          | [(left_child, left_start, _left_end);
             (right_child, _right_start, right_end)] ->
              if is_modifier_head_parent nt
                 && is_modifier_tree left_child
                 && is_nominal_tree right_child
                 && tree_leaf_count left_child = 1
                 && tree_leaf_count right_child = 1
              then [(left_start, right_end)]
              else []
          | _ -> []
        in
        (end_pos, own_spans @ spans_from_children)
  in
  let (_end_pos, spans) = helper 0 tree in
  spans

let modifier_head_syllable_spans tree =
  let toks = leaves_of_tree tree in
  let boundaries = syllable_boundaries_of_tokens toks in
  List.map
    (fun (start_tok, end_tok) ->
       (nth_or 0 boundaries start_tok, nth_or 0 boundaries end_tok))
    (modifier_head_token_spans tree)

let modifier_head_cohesion_violations tree domains =
  let boundaries = interior_domain_end_positions domains in
  let spans = modifier_head_syllable_spans tree in
  List.fold_left
    (fun total (start_pos, end_pos) ->
       total + count_list
         (fun boundary -> boundary_is_inside_span boundary start_pos end_pos)
         boundaries)
    0
    spans


(* Additional interface constraints for the paper-facing top-N model.

   These constraints are still deliberately simple and transparent.  They are
   meant to improve the ranking of generated candidates without hard-coding a
   surface form for any single example.

   - RP-LENGTH: very long rhythmic phrases are dispreferred.  Domains of 2-5
     syllables are allowed; domains longer than 5 receive penalties.
   - MAP-BALANCE: adjoining prosodic phrases should not be extremely lopsided.
     This implements a rough MAPMAX/MAPMIN-style pressure toward manageable,
     symmetric rhythmic groups.
   - SUBJECT-PREDICATE: when the syntax contains TP -> NP VP/AP, prefer a
     prosodic boundary at the subject/predicate edge.
   - NO-ADJACENT-SINGLETONS: avoid choppy prosodic mappings with two
     neighboring one-syllable domains, e.g. (xiao3)(zhi3)(lao3 hu3).

   OCP(T3) and *BOUNDARY-RISE are not added here as extra ranking constraints,
   because the tone evaluator already applies T3 sandhi inside each domain and
*)

let rp_length_violations domains =
  List.fold_left
    (fun total domain ->
       let len = List.length domain in
       if len <= 5 then total else total + (len - 5))
    0
    domains

let rec adjacent_pairs xs =
  match xs with
  | x :: y :: rest -> (x, y) :: adjacent_pairs (y :: rest)
  | _ -> []

let map_balance_violations domains =
  List.fold_left
    (fun total (left_domain, right_domain) ->
       let diff = abs (List.length left_domain - List.length right_domain) in
       if diff <= 2 then total else total + (diff - 2))
    0
    (adjacent_pairs domains)

let no_adjacent_singleton_violations domains =
  List.fold_left
    (fun total (left_domain, right_domain) ->
       if List.length left_domain = 1 && List.length right_domain = 1
       then total + 1
       else total)
    0
    (adjacent_pairs domains)

let is_predicate_tree tree =
  match tree with
  | Node (MVP, _) -> true
  | Node (MVbar, _) -> true
  | Node (MAP, _) -> true
  | Node (MNegP, _) -> true
  | Node (MBiP, _) -> true
  | _ -> false

let subject_predicate_token_boundaries tree =
  let rec helper start_pos tr =
    match tr with
    | Leaf (_, _) -> (start_pos + 1, [])
    | Node (nt, children) ->
        let rec scan pos kids =
          match kids with
          | [] -> (pos, [], [])
          | child :: rest ->
              let child_start = pos in
              let (child_end, child_bounds) = helper child_start child in
              let (final_end, child_infos, rest_bounds) = scan child_end rest in
              (final_end,
               (child, child_start, child_end) :: child_infos,
               child_bounds @ rest_bounds)
        in
        let (end_pos, child_infos, bounds_from_children) = scan start_pos children in
        let own_bounds =
          match child_infos with
          | [(left_child, _left_start, left_end);
             (right_child, _right_start, _right_end)] ->
              (match left_child with
               | Node (MNP, _) | Leaf (MPron, _) | Leaf (MProperN, _) ->
                   if nt = MTP && is_predicate_tree right_child then [left_end]
                   else []
               | _ -> [])
          | _ -> []
        in
        (end_pos, own_bounds @ bounds_from_children)
  in
  let (_end_pos, bounds) = helper 0 tree in
  unique_ints bounds

let subject_predicate_syllable_boundaries tree =
  let toks = leaves_of_tree tree in
  let boundaries = syllable_boundaries_of_tokens toks in
  List.map (nth_or 0 boundaries) (subject_predicate_token_boundaries tree)

let subject_predicate_boundary_violations tree domains =
  let actual_boundaries = interior_domain_end_positions domains in
  let required_boundaries = subject_predicate_syllable_boundaries tree in
  count_list
    (fun required -> not (int_mem required actual_boundaries))
    required_boundaries


(* VERB-OBJECT-ADJ-N COHESION / VERB-OBJECT BOUNDARY
   If the surface token sequence contains Verb + Adjective + Noun, prefer the
   prosodic boundary after the verb so that the adjective+noun object can form
   its own domain.  This is an implementation-level semantic/prosodic preference
   for readings like mai3 [hao3 jiu3] "buy good wine" over [mai3 hao3] jiu3
   when no extra context forces a resultative reading. *)
let verb_adj_noun_object_boundary_violations sentence domains =
  let toks = words sentence in
  let boundaries = syllable_boundaries_of_tokens toks in
  let actual_boundaries = interior_domain_end_positions domains in
  let rec scan i ts =
    match ts with
    | v :: a :: n :: rest ->
        let verb_boundary = nth_or 0 boundaries (i + 1) in
        let adj_noun_boundary = nth_or 0 boundaries (i + 2) in
        let is_van =
          int_mem v mandarin3_verbs
          && int_mem a mandarin3_adjectives
          && int_mem n mandarin3_nouns
        in
        let violations_here =
          if is_van then
            (if int_mem verb_boundary actual_boundaries then 0 else 1)
            + (if int_mem adj_noun_boundary actual_boundaries then 1 else 0)
          else 0
        in
        violations_here + scan (i + 1) (a :: n :: rest)
    | _ -> 0
  in
  scan 0 toks

let candidate_domains_from_tree sentence tree =
  let sylls = syllables_of_sentence sentence in
  let n = List.length sylls in
  let spans = syntax_spans_from_tree tree in
  let usable_spans =
    List.filter
      (fun span ->
         let len = span.syntax_end_syll - span.syntax_start_syll in
         len >= 2 && len <= 4 && len < n)
      spans
  in
  let syntactic_edge_domains =
    let edges =
      List.filter
        (fun edge -> edge > 0 && edge < n)
        (List.map (fun span -> span.syntax_end_syll) spans)
    in
    if edges = [] then [] else [domains_from_boundaries sylls (0 :: n :: edges)]
  in
  let span_domains =
    List.map
      (fun span ->
         domains_around_span sylls span.syntax_start_syll span.syntax_end_syll)
      usable_spans
  in
  let partition_domains =
    if n <= 7 then domain_partitions 4 sylls
    else []
  in
  let raw =
    [whole_domain sentence;
     left_binary_domains sentence;
     right_binary_domains sentence;
     token_domains sentence]
    @ syntactic_edge_domains
    @ span_domains
    @ partition_domains
  in
  unique_by_string string_of_domains raw

type integrated_mandarin_analysis = {
  integrated_tree : (mcat, string) parse_tree;
  integrated_tree_string : string;
  integrated_syntax_memory_weight : int;
  integrated_max_right_edge_closure : int;
  integrated_domains : string list list;
  integrated_surface : string list;
  integrated_profile : constraint_profile;
  integrated_extra_penalty : int;
  integrated_total_penalty : int;
  integrated_score : int;
}

let integrated_extra_penalty sentence tree domains =
  (* Localizers such as li3 attach tightly to the preceding NP/LocP, minimal
     modifier-head nominal units such as lao3 hu3 or hao3 jiu3 prefer to stay
     in the same prosodic domain, long rhythmic phrases are avoided, adjoining
     MAP-sized phrases prefer manageable balance, adjacent singleton domains are
     avoided, subject/predicate edges tend to be prosodic boundary sites, and
     V + Adj + N object sequences prefer a boundary after the verb. *)
  (20 * localizer_attachment_violations sentence domains)
  + (40 * modifier_head_cohesion_violations tree domains)
  + (20 * rp_length_violations domains)
  + (8 * map_balance_violations domains)
  + (25 * no_adjacent_singleton_violations domains)
  + (18 * subject_predicate_boundary_violations tree domains)
  + (30 * verb_adj_noun_object_boundary_violations sentence domains)

let make_integrated_analysis sentence tree domains =
  let profile = constraint_profile_of_domains sentence domains in
  let extra = integrated_extra_penalty sentence tree domains in
  let total_penalty = profile.weighted_penalty + extra in
  let score = max 0 (100 - total_penalty) in
  { integrated_tree = tree;
    integrated_tree_string = tree_to_brackets show_mcat (fun s -> s) tree;
    integrated_syntax_memory_weight = syntax_tree_memory_weight tree;
    integrated_max_right_edge_closure = max_right_edge_closure_count tree;
    integrated_domains = domains;
    integrated_surface = surface_of_domains domains;
    integrated_profile = profile;
    integrated_extra_penalty = extra;
    integrated_total_penalty = total_penalty;
    integrated_score = score }

let compare_integrated_analysis a b =
  let score_cmp = compare b.integrated_score a.integrated_score in
  if score_cmp <> 0 then score_cmp
  else
    let penalty_cmp = compare a.integrated_total_penalty b.integrated_total_penalty in
    if penalty_cmp <> 0 then penalty_cmp
    else
      let memory_cmp = compare a.integrated_syntax_memory_weight b.integrated_syntax_memory_weight in
      if memory_cmp <> 0 then memory_cmp
      else compare (string_of_domains a.integrated_domains) (string_of_domains b.integrated_domains)

let integrated_ranked_mandarin_analyses sentence =
  let syntax_trees = take 12 (ranked_mandarin_syntax_trees sentence) in
  let candidates =
    list_concat_map
      (fun tree ->
         List.map
           (fun domains -> make_integrated_analysis sentence tree domains)
           (candidate_domains_from_tree sentence tree))
      syntax_trees
  in
  unique_by_string
    (fun candidate ->
       string_of_domains candidate.integrated_domains
       ^ " -> " ^ string_of_surface candidate.integrated_surface)
    (List.sort compare_integrated_analysis candidates)

(* The function above needs sentence for the LOCALIZER-ATTACH count, so use this
   printer instead of calling string_of_integrated_profile directly. *)
let print_integrated_profile sentence candidate =
  print_string
    ("Ranking: score=" ^ string_of_int candidate.integrated_score
     ^ "; penalty=" ^ string_of_int candidate.integrated_total_penalty
     ^ "; ALIGN-R=" ^ string_of_int candidate.integrated_profile.align_r_violations
     ^ "; RIGHT-EDGE-CLOSURE=" ^ string_of_int candidate.integrated_profile.right_edge_closure_pressure
     ^ "; LEXICAL-COHESION=" ^ string_of_int candidate.integrated_profile.lexical_cohesion_violations
     ^ "; BINARITY-FT=" ^ string_of_int candidate.integrated_profile.binarity_ft_violations
     ^ "; MINIMIZE-LARGE-DOMAINS=" ^ string_of_int candidate.integrated_profile.large_domain_violations
     ^ "; LOCALIZER-ATTACH=" ^ string_of_int (localizer_attachment_violations sentence candidate.integrated_domains)
     ^ "; MODIFIER-HEAD=" ^ string_of_int (modifier_head_cohesion_violations candidate.integrated_tree candidate.integrated_domains)
     ^ "; RP-LENGTH=" ^ string_of_int (rp_length_violations candidate.integrated_domains)
     ^ "; MAP-BALANCE=" ^ string_of_int (map_balance_violations candidate.integrated_domains)
     ^ "; NO-ADJ-SINGLETONS=" ^ string_of_int (no_adjacent_singleton_violations candidate.integrated_domains)
     ^ "; SUBJECT-PRED=" ^ string_of_int (subject_predicate_boundary_violations candidate.integrated_tree candidate.integrated_domains)
     ^ "; V-OBJ-ADJ-N=" ^ string_of_int (verb_adj_noun_object_boundary_violations sentence candidate.integrated_domains)
     ^ "
")

let analyze_mandarin_integrated sentence =
  let syntax = mandarin3_bracket_sentence sentence in
  let candidates = integrated_ranked_mandarin_analyses sentence in
  print_string "============================================================\n";
  print_string ("INTEGRATED syntax -> prosody -> 3TS analysis\n");
  print_string ("Input: " ^ sentence ^ "\n");
  print_string ("Tokens: " ^ String.concat ", " (words sentence) ^ "\n");
  print_string ("Syllables: " ^ String.concat ", " (syllables_of_sentence sentence) ^ "\n");
  print_string ("Underlying tones: " ^ string_of_tones (tones_of_sentence sentence) ^ "\n\n");
  print_string ("Possible syntax candidates: " ^ string_of_int (List.length syntax) ^ "\n");
  print_string "Top syntax candidates by memory/right-edge closure:\n";
  print_ranked_mandarin_syntax 5 sentence;
  print_string ("Integrated ranked p-domains + surface forms: " ^ string_of_int (List.length candidates) ^ "\n\n");
  List.iteri
    (fun i candidate ->
       if i < 10 then begin
         print_string ("Candidate " ^ string_of_int (i + 1) ^ "\n");
         print_integrated_profile sentence candidate;
         print_string ("Syntax memory_weight=" ^ string_of_int candidate.integrated_syntax_memory_weight);
         print_string ("; max_right_edge_closure=" ^ string_of_int candidate.integrated_max_right_edge_closure ^ "\n");
         print_string ("Syntax source: " ^ candidate.integrated_tree_string ^ "\n");
         print_string ("P-domain: " ^ string_of_domains candidate.integrated_domains ^ "\n");
         print_tone_line "Surface: " candidate.integrated_surface;
         print_string "\n"
       end)
    candidates;
  if List.length candidates > 10 then
    print_string ("... " ^ string_of_int (List.length candidates - 10) ^ " more integrated candidates omitted.\n");
  print_string "============================================================\n\n"

let integrated_mandarin_surface_forms sentence =
  unique_strings
    (List.map
       (fun candidate -> string_of_surface candidate.integrated_surface)
       (integrated_ranked_mandarin_analyses sentence))

let print_integrated_mandarin_surface_forms sentence =
  List.iter
    (fun s -> print_string (s ^ "\n"))
    (integrated_mandarin_surface_forms sentence)

let print_integrated_elicitation_surface_summary () =
  List.iter
    (fun (label, sentence) ->
       let forms = take 5 (integrated_mandarin_surface_forms sentence) in
       print_string (label ^ "\n");
       print_string ("Input: " ^ sentence ^ "\n");
       print_string ("Predicted surfaces: " ^ String.concat ", " forms ^ "\n\n"))
    elicitation_stimuli

(* -------------------------------------------------------------------------- *)
(* Best-only integrated printers                                               *)
(* -------------------------------------------------------------------------- *)

(* Return only the highest-ranked integrated syntax -> prosody -> 3TS analysis.
   If several candidates tie, this chooses the deterministic first candidate
   after compare_integrated_analysis tie-breaking. *)
let best_integrated_mandarin_analysis sentence =
  match integrated_ranked_mandarin_analyses sentence with
  | [] -> None
  | best :: _ -> Some best

let integrated_best_mandarin_surface_form sentence =
  match best_integrated_mandarin_analysis sentence with
  | None -> "NO-PARSE"
  | Some best -> string_of_surface best.integrated_surface

let integrated_best_mandarin_domain_string sentence =
  match best_integrated_mandarin_analysis sentence with
  | None -> "NO-PARSE"
  | Some best -> string_of_domains best.integrated_domains

let integrated_top_tie_count sentence =
  match integrated_ranked_mandarin_analyses sentence with
  | [] -> 0
  | best :: rest ->
      1 +
      List.length
        (List.filter
           (fun candidate ->
              candidate.integrated_score = best.integrated_score
              && candidate.integrated_total_penalty = best.integrated_total_penalty)
           rest)

let print_integrated_best_analysis sentence =
  let syntax = mandarin3_bracket_sentence sentence in
  print_string "============================================================\n";
  print_string "BEST integrated syntax -> prosody -> 3TS analysis\n";
  print_string ("Input: " ^ sentence ^ "\n");
  print_string ("Tokens: " ^ String.concat ", " (words sentence) ^ "\n");
  print_string ("Syllables: " ^ String.concat ", " (syllables_of_sentence sentence) ^ "\n");
  print_string ("Underlying tones: " ^ string_of_tones (tones_of_sentence sentence) ^ "\n\n");
  print_string ("Possible syntax candidates: " ^ string_of_int (List.length syntax) ^ "\n");
  match best_integrated_mandarin_analysis sentence with
  | None ->
      print_string "No integrated candidates found.\n";
      print_string "============================================================\n\n"
  | Some best ->
      print_string ("Top tied candidates with same score/penalty: " ^ string_of_int (integrated_top_tie_count sentence) ^ "\n");
      print_integrated_profile sentence best;
      print_string ("Syntax memory_weight=" ^ string_of_int best.integrated_syntax_memory_weight);
      print_string ("; max_right_edge_closure=" ^ string_of_int best.integrated_max_right_edge_closure ^ "\n");
      print_string ("Syntax source: " ^ best.integrated_tree_string ^ "\n");
      print_string ("P-domain: " ^ string_of_domains best.integrated_domains ^ "\n");
      print_tone_line "Best surface: " best.integrated_surface;
      print_string "============================================================\n\n"

let print_integrated_best_elicitation_surface_summary () =
  List.iter
    (fun (label, sentence) ->
       print_string (label ^ "\n");
       print_string ("Input: " ^ sentence ^ "\n");
       match best_integrated_mandarin_analysis sentence with
       | None ->
           print_string "Best surface: NO-PARSE\n\n"
       | Some best ->
           print_string ("Best surface: " ^ string_of_surface best.integrated_surface ^ "\n");
           print_string ("Best p-domain: " ^ string_of_domains best.integrated_domains ^ "\n");
           print_string ("Score: " ^ string_of_int best.integrated_score
                         ^ "; penalty: " ^ string_of_int best.integrated_total_penalty
                         ^ "; top ties: " ^ string_of_int (integrated_top_tie_count sentence) ^ "\n\n"))
    elicitation_stimuli

(* Compact one-line version, useful for copying into the paper or spreadsheet. *)
let print_integrated_best_elicitation_table () =
  print_string "Label | Input | Best surface | Best p-domain | Score | Penalty | Top ties\n";
  print_string "--- | --- | --- | --- | --- | --- | ---\n";
  List.iter
    (fun (label, sentence) ->
       match best_integrated_mandarin_analysis sentence with
       | None ->
           print_string (label ^ " | " ^ sentence ^ " | NO-PARSE | NO-PARSE |  |  | \n")
       | Some best ->
           print_string
             (label ^ " | "
              ^ sentence ^ " | "
              ^ string_of_surface best.integrated_surface ^ " | "
              ^ string_of_domains best.integrated_domains ^ " | "
              ^ string_of_int best.integrated_score ^ " | "
              ^ string_of_int best.integrated_total_penalty ^ " | "
              ^ string_of_int (integrated_top_tie_count sentence) ^ "\n"))
    elicitation_stimuli


(* -------------------------------------------------------------------------- *)
(* Top-N integrated printers                                                   *)
(* -------------------------------------------------------------------------- *)

(* Return the top n ranked integrated syntax -> prosody -> 3TS analyses.
   These are candidate analyses, not necessarily unique surface forms. *)
let top_n_integrated_mandarin_analyses n sentence =
  if n <= 0 then []
  else take n (integrated_ranked_mandarin_analyses sentence)

(* Return the top n unique surface forms, preserving the ranking order of the
   best candidate that produced each surface. *)
let top_n_integrated_mandarin_surface_forms n sentence =
  if n <= 0 then []
  else take n (integrated_mandarin_surface_forms sentence)

let print_integrated_top_n_analysis n sentence =
  let syntax = mandarin3_bracket_sentence sentence in
  let candidates = top_n_integrated_mandarin_analyses n sentence in
  print_string "============================================================\n";
  print_string ("TOP " ^ string_of_int n ^ " integrated syntax -> prosody -> 3TS analyses\n");
  print_string ("Input: " ^ sentence ^ "\n");
  print_string ("Tokens: " ^ String.concat ", " (words sentence) ^ "\n");
  print_string ("Syllables: " ^ String.concat ", " (syllables_of_sentence sentence) ^ "\n");
  print_string ("Underlying tones: " ^ string_of_tones (tones_of_sentence sentence) ^ "\n\n");
  print_string ("Possible syntax candidates: " ^ string_of_int (List.length syntax) ^ "\n");
  print_string ("Showing top integrated candidates: " ^ string_of_int (List.length candidates) ^ "\n\n");
  if candidates = [] then
    print_string "No integrated candidates found.\n"
  else
    List.iteri
      (fun i candidate ->
         print_string ("Candidate " ^ string_of_int (i + 1) ^ "\n");
         print_integrated_profile sentence candidate;
         print_string ("Syntax memory_weight=" ^ string_of_int candidate.integrated_syntax_memory_weight);
         print_string ("; max_right_edge_closure=" ^ string_of_int candidate.integrated_max_right_edge_closure ^ "\n");
         print_string ("Syntax source: " ^ candidate.integrated_tree_string ^ "\n");
         print_string ("P-domain: " ^ string_of_domains candidate.integrated_domains ^ "\n");
         print_tone_line "Surface: " candidate.integrated_surface;
         print_string "\n")
      candidates;
  print_string "============================================================\n\n"

(* This is usually the most useful one for the paper: it prints the top n
   unique surface outputs, not every tied analysis that happens to produce the
   same surface. *)
let print_integrated_top_n_surface_forms n sentence =
  let forms = top_n_integrated_mandarin_surface_forms n sentence in
  if forms = [] then
    print_string "NO-PARSE\n"
  else
    List.iter
      (fun surface -> print_string (surface ^ "\n"))
      forms

let print_integrated_top_n_elicitation_surface_summary n =
  List.iter
    (fun (label, sentence) ->
       let candidates = top_n_integrated_mandarin_analyses n sentence in
       print_string (label ^ "\n");
       print_string ("Input: " ^ sentence ^ "\n");
       if candidates = [] then
         print_string "No integrated candidates found.\n\n"
       else begin
         List.iteri
           (fun i candidate ->
              print_string
                ("#" ^ string_of_int (i + 1)
                 ^ " surface: " ^ string_of_surface candidate.integrated_surface
                 ^ "; score=" ^ string_of_int candidate.integrated_score
                 ^ "; penalty=" ^ string_of_int candidate.integrated_total_penalty
                 ^ "\n");
              print_string ("   p-domain: " ^ string_of_domains candidate.integrated_domains ^ "\n"))
           candidates;
         print_string "\n"
       end)
    elicitation_stimuli

(* Compact markdown-like table.  This prints one row per candidate, so if n=3
   and there are 10 stimuli, it can print up to 30 rows. *)
let print_integrated_top_n_elicitation_table n =
  print_string "Label | Rank | Input | Surface | P-domain | Score | Penalty | Top ties\n";
  print_string "--- | --- | --- | --- | --- | --- | --- | ---\n";
  List.iter
    (fun (label, sentence) ->
       let candidates = top_n_integrated_mandarin_analyses n sentence in
       match candidates with
       | [] ->
           print_string (label ^ " |  | " ^ sentence ^ " | NO-PARSE | NO-PARSE |  |  | \n")
       | _ ->
           List.iteri
             (fun i candidate ->
                print_string
                  (label ^ " | "
                   ^ string_of_int (i + 1) ^ " | "
                   ^ sentence ^ " | "
                   ^ string_of_surface candidate.integrated_surface ^ " | "
                   ^ string_of_domains candidate.integrated_domains ^ " | "
                   ^ string_of_int candidate.integrated_score ^ " | "
                   ^ string_of_int candidate.integrated_total_penalty ^ " | "
                   ^ string_of_int (integrated_top_tie_count sentence) ^ "\n"))
             candidates)
    elicitation_stimuli

(* Compact table over unique surface forms only. This is useful when multiple
   high-ranked analyses tie but you mainly want distinct possible pronunciations. *)
let print_integrated_top_n_surface_table n =
  print_string "Label | Input | Top surface forms\n";
  print_string "--- | --- | ---\n";
  List.iter
    (fun (label, sentence) ->
       let forms = top_n_integrated_mandarin_surface_forms n sentence in
       let shown = if forms = [] then "NO-PARSE" else String.concat ", " forms in
       print_string (label ^ " | " ^ sentence ^ " | " ^ shown ^ "\n"))
    elicitation_stimuli

