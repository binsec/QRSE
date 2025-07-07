(**************************************************************************)
(*  This file is part of QRSE.                                            *)
(*                                                                        *)
(*  Copyright (C) 2025                                                    *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Binsec
open Libsse
open Types

include Cli.Make 
    (
        struct
            let name = "Quantitative Robust Symbolic Execution"

            let shortname = "qrse"
        end
    )

module Portfolio = Builder.String_option
    (
        struct
            let name = "portfolio"
            let doc = "Configure the popcon portfolio for solving quantitative robustness (format: \"option set;...>fallback batch>...\", default: \">--relax 8\")."
        end
    )

module Timeout = Builder.Integer
    (
        struct
            let name = "timeout"
            let doc = "Set the timeout for popcon (milliseconds, 300000 by default, set to 0 to turn off)."

            let default = 300000
        end
    )

module Stop = Builder.False
    (
        struct
            let name = "stop"
            let doc = "Stop analysis when all objectives are fulfilled."
        end
    )

module RR = Builder.String
    (
        struct
            let name = "rr"
            let doc = "Set the robust reachability mode: precheck = check before quantitative robustness, only = only check robust reachability or off (default: precheck)."
            let default = "precheck"
        end
    )

module RReach =
    struct
        type Formula.custom_desc += RReach of Formula.decl list * Formula.bl_term

        let hash u b =
            let uh = List.map (fun decl -> decl.Formula.decl_hash) u in
            let bh = b.Formula.bl_term_hash in
            Hashtbl.hash (bh::uh)

        let to_smtlib u b =
            let mk_bitvec_symbol size = 
                Smtlib_utils.mk_symbol @@ Printf.sprintf "(_ BitVec %i)" size
            in
            let mk_array_symbol idx elt = 
                Smtlib_utils.mk_symbol @@ Printf.sprintf "(Array (_ BitVec %i) (_ BitVec %i))" idx elt
            in
            let of_sort = function
                | Formula.BlSort -> Smtlib_utils.mk_sort_identifier @@ Smtlib_utils.mk_symbol "Bool"
                | Formula.BvSort(i) -> Smtlib_utils.mk_sort_identifier @@ mk_bitvec_symbol i
                | Formula.AxSort(i, j) -> Smtlib_utils.mk_sort_identifier @@ mk_array_symbol i j
            in
            let of_decl = function
                | Formula.BlDecl(v, ls) ->
                ( 
                    Smtlib_utils.mk_symbol v.Formula.bl_name,
                    Smtlib_utils.mk_sort_identifier @@ Smtlib_utils.mk_symbol "Bool",
                    List.map of_sort ls 
                )
                | Formula.BvDecl(v, ls) ->
                ( 
                    Smtlib_utils.mk_symbol v.Formula.bv_name,
                    Smtlib_utils.mk_sort_identifier @@ mk_bitvec_symbol v.Formula.bv_size,
                    List.map of_sort ls 
                )
                | Formula.AxDecl(v, ls) ->
                ( 
                    Smtlib_utils.mk_symbol v.Formula.ax_name,
                    Smtlib_utils.mk_sort_identifier @@ mk_array_symbol v.Formula.idx_size v.Formula.elt_size,
                    List.map of_sort ls 
                )
            in
            let of_decls decls = 
                List.map 
                    (
                        fun decl -> 
                            let symbol, sort, sorts = of_decl decl.Formula.decl_desc in
                            assert (sorts = []);
                            Smtlib_utils.mk_sorted_var symbol sort
                    )
                    decls
            in
            Smtlib_utils.mk_term_forall_term (of_decls u) @@ Formula_to_smtlib.bl_term b

        let pp fmt u b =
            Format.fprintf fmt "@[assert %a@]" Smtlib_pp.pp_term @@ to_smtlib u b

        let _ =
            let hash = function
                | RReach(u, b) -> Some(hash u b)
                | _ -> None
            in
            let pp fmt = function
                | RReach(u, b) -> 
                (
                    pp fmt u b;
                    true
                )
                | _ -> false
            in
            Formula.register_custom {Formula.hash; Formula.pp}

        let create controlled fml =
            let c = Formula.fold_backward
                (
                    fun e c ->
                        match e.Formula.entry_desc with
                        | Define({def_desc = BvDef(v, [], {bv_term_desc = BvFun(v_, []); _}); _}) ->
                        (
                            try
                                ignore @@ List.find (fun c -> c = v.bv_name) controlled;
                                v_.bv_name::c
                            with Not_found -> c
                        )
                        | _ -> c
                )
                fml []
            in
            let cdecls, udecls, defs, asserts = Formula.fold_backward
                (
                    fun e (cdecls, udecls, defs, asserts) ->
                        match e.Formula.entry_desc with
                        | Declare(decl) ->
                        (
                            match decl.Formula.decl_desc with
                            | BvDecl(v, _) ->
                            (
                                try
                                    ignore @@ List.find (fun c -> c = v.bv_name) c;
                                    decl::cdecls, udecls, defs, asserts
                                with Not_found -> cdecls, decl::udecls, defs, asserts
                            )
                            | _ -> cdecls, decl::udecls, defs, asserts
                        )
                        | Define(def) -> cdecls, udecls, def::defs, asserts
                        | Assert(bl)
                        | Assume(bl) -> cdecls, udecls, defs, bl::asserts
                        | _ -> cdecls, udecls, defs, asserts
                )
                fml ([], [], [], [])
            in
            let body = List.fold_right (fun def res -> Formula.mk_bl_let [def] res) defs
                @@ List.fold_left (fun res bl -> Formula.mk_bl_and res bl) Formula.mk_bl_true asserts
            in
            let rrentry = Formula.entry (Formula.Custom(RReach(udecls, body))) in
            let fold f l fml =
                let switch f x y = f y x in
                List.fold_left (switch f) fml l
            in
            Formula.empty
                |> fold Formula.push_front_declare cdecls
                |> Formula.push_front rrentry

        type res = Ok of bool | Error of string

        let check controlled fml =
            let fml = create controlled fml in
            Formula_options.Theory.set "ABV";
            let oldsolver = Smt.Smt_options.SMTSolver.get () in
            Smt.Smt_options.SMTSolver.set Smt.Smt_options.Bitwuzla_smtlib;
            let module Solver = (val Smt.Smt_solver.get_solver ()) in
            let res = 
                try
                    let session = Solver.open_session () in
                    let res =
                        try
                            Formula.iter_forward (Solver.put session) fml;
                            match Solver.check_sat session with
                            | Formula.SAT -> Ok(true)
                            | UNSAT -> Ok(false)
                            | TIMEOUT -> Error("timeout")
                            | UNKNOWN -> Error("unknown")
                        with e -> Error(Printexc.to_string e)
                    in
                    Solver.close_session session;
                    res
                with e -> Error(Printexc.to_string e)
            in
            Formula_options.Theory.set "QF_ABV";
            Smt.Smt_options.SMTSolver.set oldsolver;
            res
    end

type Ast.Instr.t += CheckQRInstr of string option * int * Script.Expr.t Script.loc option * float option * bool | Controlled of Ast.Loc.t Ast.loc

let pp_qr_instr fmt tag count guard thresh merge =
    let tag =
        match tag with
        | Some(tag) -> Printf.sprintf "<%s>" tag
        | _ -> ""
    in
    let merge = if merge then "merge " else "" in
    let thresh =
        match thresh with
        | Some(f) -> Printf.sprintf "> %.3g " f
        | _ -> ""
    in
    Format.fprintf fmt "qr%s %s%s" tag merge thresh;
    Ast.Instr.pp fmt (Script.Reach(count, guard, []))

let pp_ctrl fmt loc =
    Format.fprintf fmt "%a := controlled" Ast.Loc.pp loc

let pp_instr fmt = function
    | CheckQRInstr(tag, count, guard, thresh, merge) ->
    (
        pp_qr_instr fmt tag count guard thresh merge;
        true
    )
    | Controlled(loc, _) -> 
    (
        pp_ctrl fmt loc;
        true
    )
    | _ -> false

type Ast.Obj.t += Float of float | Float_opt of float option | Bool of bool | String_opt of string option

let gram_ext = 
    [
        Dyp.Bind_to_cons
            (
                [
                    "float", "Obj";
                    "float_opt", "Obj";
                    "qr_threshold", "Obj";
                    "qr_merge", "Obj";
                    "qr_tag", "Obj"
                ]
            )
        ;
        Dyp.Add_rules
            (
                [
                    (
                        "float", 
                        [
                            Dyp.Regexp
                                (
                                    RE_Seq
                                        (
                                            [
                                                RE_Char_set(['0', '9']); 
                                                RE_Option
                                                    (
                                                        RE_Seq
                                                            (
                                                                [
                                                                    RE_Char('.'); 
                                                                    RE_Char_set(['0', '9'])
                                                                ]
                                                            )
                                                    )
                                            ]
                                        )
                                )
                        ], 
                        "default_priority", 
                        []
                    ),
                    (
                        fun _ -> function
                            | [Libparser.Syntax.Lexeme_matched(f)] -> Libparser.Syntax.Obj(Float(Float.of_string f)), []
                            | _ -> assert false
                    )
                    ;

                    ("float_opt", [], "default_priority", []),
                    (fun _ _ -> Libparser.Syntax.Obj(Float_opt(None)), [])
                    ;

                    ("float_opt", [Dyp.Non_ter("float", No_priority)], "default_priority", []),
                    (
                        fun _ -> function
                            | [Libparser.Syntax.Obj(Float(f))] -> Libparser.Syntax.Obj(Float_opt(Some(f))), []
                            | _ -> assert false
                    )
                    ;

                    ("qr_threshold", [], "default_priority", []),
                    (fun _ _ -> Libparser.Syntax.Obj(Float_opt(None)), [])
                    ;

                    (
                        "qr_threshold", 
                        [
                            Dyp.Regexp(RE_String("threshold"));
                            Dyp.Non_ter("float", No_priority)
                        ],
                        "default_priority",
                        []
                    ),
                    (
                        fun _ -> function
                            | [_; Libparser.Syntax.Obj(Float(f))] -> Libparser.Syntax.Obj(Float_opt(Some(f))), []
                            | _ -> assert false
                    )
                    ;

                    ("qr_merge", [], "default_priority", []),
                    (fun _ _ -> Libparser.Syntax.Obj(Bool(false)), [])
                    ;

                    ("qr_merge", [Dyp.Regexp(RE_String("merge"))], "default_priority", []),
                    (fun _ _ -> Libparser.Syntax.Obj(Bool(true)), [])
                    ;

                    ("qr_tag", [], "default_priority", []),
                    (fun _ _ -> Libparser.Syntax.Obj(String_opt(None)), [])
                    ;

                    (
                        "qr_tag", 
                        [
                            Dyp.Regexp
                                (
                                    RE_Seq
                                        (
                                            [
                                                RE_Char('<'); 
                                                RE_Star(RE_Char_set(['a', 'z'; 'A', 'Z'; '0', '9'])); 
                                                RE_Char('>')
                                            ]
                                        )
                                )
                        ],
                        "default_priority",
                        []
                    ),
                    (
                        fun _ -> function
                            | [Libparser.Syntax.Lexeme_matched(s)] -> 
                            (
                                let s = String.sub s 1 ((String.length s) - 2) in
                                if s = ""
                                then Libparser.Syntax.Obj(String_opt(None)), []
                                else Libparser.Syntax.Obj(String_opt(Some(s))), []
                            )
                            | _ -> assert false
                    )
                    ;

                    (
                        "decl", 
                        [
                            Dyp.Regexp(RE_String("qr"));
                            Dyp.Non_ter("qr_tag", No_priority);
                            Dyp.Non_ter("qr_merge", No_priority);
                            Dyp.Non_ter("qr_threshold", No_priority);
                            Dyp.Non_ter("decl", No_priority)
                        ],
                        "default_priority",
                        []
                    ),
                    (
                        fun _ -> 
                            let add tag thresh merge e res =
                                match e with
                                | Script.Reach(count, guard, out) -> CheckQRInstr(tag, count, guard, thresh, merge)::e::res
                                | _ -> e::res
                            in
                            function
                                | 
                                    [
                                        _; 
                                        Libparser.Syntax.Obj(String_opt(tag));
                                        Libparser.Syntax.Obj(Bool(merge)); 
                                        Libparser.Syntax.Obj(Float_opt(thresh)); 
                                        Libparser.Syntax.Decl(Script.Hook(a, l, b))
                                    ] 
                                    -> Libparser.Syntax.Decl(Script.Hook(a, List.fold_right (add tag thresh merge) l [], b)), []
                                | 
                                    [
                                        _; 
                                        Libparser.Syntax.Obj(String_opt(tag));
                                        Libparser.Syntax.Obj(Bool(merge)); 
                                        Libparser.Syntax.Obj(Float_opt(thresh)); 
                                        Libparser.Syntax.Decl(Script.Return_hook(s, l))
                                    ] 
                                    -> Libparser.Syntax.Decl(Script.Return_hook(s, List.fold_right (add tag thresh merge) l [])), []
                                | [_; _; _; _; Libparser.Syntax.Decl(_) as d] -> d, []
                                | _ -> assert false
                    )
                    ;

                    (
                        "fallthrough",
                        [
                            Dyp.Non_ter("loc", No_priority);
                            Dyp.Regexp(RE_String(":="));
                            Dyp.Regexp(RE_String("controlled"))
                        ],
                        "default_priority",
                        []
                    ),
                    (
                        fun _ -> function
                            | [Libparser.Syntax.Loc(lv); _; _] -> Libparser.Syntax.Instr(Controlled(lv)), []
                            | _ -> raise Dyp.Giveup
                    )
                    ;

                    (
                        "instr",
                        [
                            Dyp.Non_ter("loc", No_priority);
                            Dyp.Regexp(RE_String(":="));
                            Dyp.Regexp(RE_String("controlled"));
                            Dyp.Non_ter("accept_newline", No_priority);
                            Dyp.Ter("AS");
                            Dyp.Non_ter("ident", No_priority)
                        ],
                        "default_priority",
                        []
                    ),
                    (
                        fun _ -> function
                            | 
                                [
                                    Libparser.Syntax.Loc(lv);
                                    _;
                                    _;
                                    _;
                                    _;
                                    Libparser.Syntax.String(name)
                                ]
                                -> 
                                (
                                    let var = Ast.Loc.var name ~size:(Ast.Size.sizeof lv), Lexing.dummy_pos in
                                    Libparser.Syntax.Stmt([Controlled(var); Ast.Instr.assign lv (Ast.Expr.loc var, Lexing.dummy_pos)]), []
                                )
                            | _ -> assert false
                    )
                ]
            )
    ]

module Popcon =
    struct
        type result = Ok of Float.t * Float.t * (string * string) list | Ko of string

        let timeout () =
            Timeout.get ()

        let options () =
            match Portfolio.get_opt () with
            | Some(s) -> List.map
                (
                    fun s ->
                        List.map
                            (
                                fun s -> 
                                    if s = ""
                                    then [||]
                                    else Array.of_list @@ String.split_on_char ' ' s
                            )
                            @@ String.split_on_char ';' s
                )
                @@ String.split_on_char '>' s
            | _ -> [[[||]];[[|"--relax"; "8"|]]]

        let pp = function
            | Ok(frac, pm, witnesses) -> List.fold_left
                (
                    fun res (var, value) ->
                        res ^ ", " ^ var ^ ": " ^ value
                )
                (Printf.sprintf "%.3g%s" frac (if pm = 0.0 then "" else Printf.sprintf "±%.3g" pm))
                witnesses
            | Ko(err) -> Printf.sprintf "error <%s>" err

        let parse s =
            let exp = Str.regexp {|\([^±,]+\)\(±\([^,]+\)\)?\(,\(.*\)\)?|} in
            let errexp = Str.regexp {|error <\(.*\)>|} in
            if Str.string_match exp s 0
            then
            (
                let frac = Float.of_string @@ Str.matched_group 1 s in
                let pm =
                    try
                        Float.of_string @@ Str.matched_group 3 s
                    with Not_found -> 0.0
                in
                let witnesses =
                    let expr = Str.regexp {| \([^:]+\): \(.*\)|} in
                    try
                        List.map 
                            (
                                fun e -> 
                                    assert (Str.string_match expr e 0);
                                    let name = Str.matched_group 1 e in
                                    let value = Str.matched_group 2 e in
                                    name, value
                            )
                            @@ String.split_on_char ','
                            @@ Str.matched_group 5 s
                    with Not_found -> []
                in
                Ok(frac, pm, witnesses)
            )
            else if Str.string_match errexp s 0
            then Ko(Str.matched_group 1 s)
            else raise (Failure("popcon result parsing failure"))

        let dir = lazy
            (
                let date = Unix.localtime @@ Unix.time () in
                let name = Printf.sprintf "/tmp/qrse-%d-%d-%d-%d:%d:%d" date.Unix.tm_year date.tm_mon date.tm_mday date.tm_hour date.tm_min date.tm_sec in
                ignore @@ Sys.command ("mkdir -p " ^ name);
                name
            )

        module StringMap = Map.Make (String)

        let compat ~controlled fml =
            (*disgusting code ahead*)
            let fix_lines s =
                let indentexp = Str.regexp {|^[ ]*|} in
                let cnt = ref 0 in
                let aux c =
                    match !cnt, c with
                    | _, '(' -> 
                    (
                        cnt := !cnt + 1;
                        c
                    )
                    | _, ')' -> 
                    (
                        cnt := !cnt - 1;
                        c
                    )
                    | 0, ' ' -> '\n'
                    | 0, _ -> c
                    | _, '\n' -> ' '
                    | _, _ -> c
                in
                String.map aux @@ Str.global_replace indentexp "" s
            in
            let fix_litterals s =
                let littexp = Str.regexp {|#\([xb]\)\([0]*\)\([a-zA-Z0-9]+\)|} in
                let replacer s =
                    let base = Str.matched_group 1 s in
                    let leadz = Str.matched_group 2 s in
                    let ns = Str.matched_group 3 s in
                    let n =
                        if (String.length ns) = 0
                        then Z.zero
                        else Z.of_string ("0" ^ base ^ ns) 
                    in
                    let size =
                        let charsize = (String.length leadz) + (String.length ns) in
                        match base with
                        | "x" -> 4 * charsize
                        | "b" -> charsize
                        | _ -> assert false
                    in
                    Printf.sprintf "(_ bv%s %d)"
                        (Z.to_string n)
                        size
                in
                Str.global_substitute littexp replacer s
            in
            let const_to_fun s =
                let constexp = Str.regexp {|declare-const \([^ ]+\)|} in
                let replacer s =
                    Printf.sprintf "declare-fun %s ()" (Str.matched_group 1 s)
                in
                Str.global_substitute constexp replacer s
            in
            let fix_names s =
                let cnt = ref 0 in
                let position = ref 0 in
                let exp = Str.regexp {|\(declare\|define\)[^ ]+ \([^ ]+\)|} in
                let rec aux s =
                    try
                        position := (Str.search_forward exp s !position) + 1;
                        let name = Str.matched_group 2 s in
                        try
                            (*don't fix controlled var names*)
                            ignore @@ List.find (fun e -> e = name) controlled;
                            s
                        with Not_found ->
                        (
                            let varexp = Str.regexp ({|\([ (]\)|} ^ (Str.quote name) ^ {|\([ )]\)|}) in
                            let replacer s =
                                let first = Str.matched_group 1 s in
                                let last = Str.matched_group 2 s in
                                Printf.sprintf "%svar_%d%s" first !cnt last
                            in
                            let res = Str.global_substitute varexp replacer s in
                            (*in case of xxx name name*)
                            let res = Str.global_substitute varexp replacer res in
                            cnt := !cnt + 1;
                            aux res
                        )
                    with Not_found -> s
                in
                aux s
            in
            let add_controlled_info s =
                let exp = Str.regexp {|(define-fun \([^ ]+\) () ([^)]*) \([^)]+\))$|} in
                let namesubst = ref StringMap.empty in
                let replacer s =
                    let l = Str.matched_string s in
                    let name = Str.matched_group 1 s in
                    let var = Str.matched_group 2 s in
                    try
                        ignore @@ List.find (fun e -> e = name) controlled;
                        namesubst := StringMap.add var name !namesubst;
                        Printf.sprintf "%s\n(set-info :controlled %s)" l var
                    with Not_found -> Str.matched_string s
                in
                let fml = Str.global_substitute exp replacer s in
                fml, !namesubst
            in
            fml         
                |> fix_lines
                |> fix_names
                |> fix_litterals
                |> const_to_fun
                |> add_controlled_info

        let cnt = ref 0

        let resexpr = Str.regexp {|Max popularity: Models: \([0-9]+\)±\([0-9]+\) ([^)]+), Influence \(-?[0-9.]+\)±\([0-9.]+\), Total bits: \([0-9]+\), Incidence \(-?[0-9.]+\)±\([0-9.]+\)|}

        let witnessexpr = Str.regexp {|\(Best model\|Lower bound reached by\): \(.*\)|}

        let timeoutexpr = Str.regexp {|.*timeout=true|}

        let query cmd =
            let session = Subprocess.spawn ~pdeathsig:Sys.sigkill cmd in
            let chan = Subprocess.stdout session in
            let res = ref (Ko("result not found")) in
            let _ =
                try
                    while true;
                    do
                        let line = input_line chan in
                        if Str.string_match resexpr line 0
                        then
                        (
                            let mc = Z.of_string @@ Str.matched_group 1 line in
                            let pm = Z.of_string @@ Str.matched_group 2 line in
                            let bits = int_of_string @@ Str.matched_group 5 line in
                            let frac = (Z.to_float mc) /. (Z.to_float @@ Z.shift_left Z.one bits) in
                            let pm =
                                if pm = Z.zero
                                then 0.0
                                else (Z.to_float pm) /. (Z.to_float @@ Z.shift_left Z.one bits)
                            in
                            res := Ok(frac, pm, []);

                            let line = input_line chan in
                            if Str.string_match witnessexpr line 0
                            then
                            (
                                let witnesses = String.split_on_char ',' @@ Str.matched_group 2 line in
                                let exp = Str.regexp {|\([^:]+\): \(.*\)|} in
                                res := Ok(frac, pm, List.filter_map 
                                        (
                                            fun s ->
                                                if Str.string_match exp s 0
                                                then 
                                                (
                                                    let var = Str.matched_group 1 s in
                                                    let value = Str.matched_group 2 s in
                                                    Some(var, value)
                                                )
                                                else None
                                        )
                                        witnesses
                                    )
                            )
                            else raise End_of_file
                        )
                        else if Str.string_match timeoutexpr line 0
                        then 
                        (
                            res := Ko("timeout");
                            raise End_of_file
                        )
                    done;
                with End_of_file -> ()
            in
            ignore @@ Subprocess.close session;
            !res

        module IntMap = Map.Make(Int)

        let query portfolio timeout fname =
            let timeout =
                if timeout > 0
                then [|"-T"; string_of_int timeout|]
                else [||]
            in
            let rec aux = function
                | batch::batches ->
                (
                    let children = ref IntMap.empty in
                    let run options =
                        let pipr, pipw = Unix.pipe () in
                        match Unix.fork () with
                        | 0 ->
                        (
                            try
                                Unix.close pipr;
                                let pipw_chan = Unix.out_channel_of_descr pipw in
                                let dead s =
                                    Printf.fprintf pipw_chan "dead\n";
                                    flush pipw_chan;
                                    close_out pipw_chan;
                                    exit 0
                                in
                                Sys.set_signal Sys.sigpipe (Sys.Signal_handle(dead));
                                let cmd = Array.concat [[|"popcon"; "--goal"; "PopCon"|]; timeout; options; [|fname|]] in
                                let res = query cmd in
                                Printf.fprintf pipw_chan "%s\n" @@ pp res;
                                flush pipw_chan;
                                close_out pipw_chan;
                                exit 0
                            with _ -> exit 0
                        )
                        | pid ->
                        (
                            Unix.close pipw;
                            children := IntMap.add pid (Unix.in_channel_of_descr pipr) !children
                        )
                        | exception e ->
                        (
                            Unix.close pipr;
                            Unix.close pipw;
                            raise e
                        )
                    in
                    List.iter run batch;
                    let res = ref (Ko("total failure")) in
                    while not @@ IntMap.is_empty !children
                    do
                        let pid, status = Unix.wait () in
                        match status with
                        | Unix.WEXITED(0) ->
                        (
                            try
                                let chan = IntMap.find pid !children in
                                let _ =
                                    try
                                        res := parse @@ input_line chan
                                    with e -> ()
                                in
                                close_in chan;
                                children := IntMap.remove pid !children;
                                match !res with
                                | Ok(_) -> 
                                (
                                    IntMap.iter (fun _ chan -> close_in chan; Unix.kill pid Sys.sigkill) !children;
                                    children := IntMap.empty
                                )
                                | _ -> ()
                            with _ -> children := IntMap.remove pid !children
                        )
                        | _ -> children := IntMap.remove pid !children 
                    done;
                    match !res with
                    | Ok(_) -> !res
                    | _ -> aux batches
                )
                | _ -> Ko("total failure")
            in
            aux portfolio

        let query ?(portfolio = options ()) ?(timeout = timeout ()) ~controlled fml =
            let fml, namesubst = compat ~controlled fml in
            let fname = Printf.sprintf "%s/popcon_query_%d.smt2" (Lazy.force dir) !cnt in
            cnt := !cnt + 1;
            try
                let chan = open_out fname in
                Printf.fprintf chan "%s" fml;
                close_out chan;
                match query portfolio timeout fname with
                | Ok(frac, pm, witnesses) -> Ok(frac, pm, List.map (fun (var, value) -> StringMap.find var namesubst, value) witnesses)
                | r -> r
            with e -> Ko(Printexc.to_string e)
    end

module State (P : Path.S) (S : STATE) =
    struct
        module P = P

        module S = S

        module Eval = Eval.Make (P) (S)

        type path = P.t

        and state = S.t

        module Controlled =
            struct
                type Ir.builtin += Add of Dba.Var.t

                let pp_add_ctrl fmt var =
                    Format.fprintf fmt "adding controlled variable %s" var.Dba.Var.name

                type t = Dba.Var.t list

                let key = P.register_key []

                let cnt = ref 0

                let get path =
                    P.get key path

                let add v path =
                    let ctrl = get path in
                    try
                        ignore @@ List.find (fun e -> e = v) ctrl
                    with Not_found -> P.set key (v::ctrl) path

                let add v addr path depth state =
                    add v path;
                    Ok(state)

                let _ =
                    P.register_at_fork
                        (
                            fun path path2 ->
                                P.set key (P.get key path) path2
                        )
            end

        module QR =
            struct
                type tag = string option * int

                let pp_tag = function
                    | Some(tag), id -> Printf.sprintf "%s:%d" tag id
                    | _, id -> string_of_int id

                type qr =
                    {
                        tag : tag;
                        mutable count : int;
                        guard : Expr.t;
                        thresh : float option;
                        merge : bool;
                        mutable merge_state : state option
                    }

                type Ir.builtin += CheckQR of qr

                let cnt = ref 0

                let active = ref 0

                let create env tag count guard thresh merge =
                    let id = !cnt in
                    cnt := !cnt + 1;
                    active := !active + 1;
                    let tag = tag, id in
                    let guard = Option.fold ~none:Dba.Expr.one ~some:(fun test -> Script.eval_expr ~size:1 test env) guard in
                    let merge_state = None in
                    {tag; count; guard; thresh; merge; merge_state}

                let pp_qr fmt qr =
                    let thresh =
                        match qr.thresh with
                        | Some(thresh) -> Printf.sprintf ">%.3g" thresh
                        | _ -> ""
                    in
                    let merge = if qr.merge then "merge" else "" in
                    let left = 
                        if qr.count > 0 
                        then Printf.sprintf "%d left" qr.count
                        else ""
                    in
                    let config = List.fold_left
                        (
                            fun res e ->
                                if e = ""
                                then res
                                else if res = ""
                                then e
                                else res ^ ", " ^ e
                        )
                        "" [merge; thresh; left]
                    in
                    let config =
                        if config = ""
                        then ""
                        else "[" ^ config ^ "]"
                    in
                    Format.fprintf fmt "qr<%s>%s" (pp_tag qr.tag) config

                let check_thresh qr = function
                    | Popcon.Ok(res, _, _) -> 
                    (
                        match qr.thresh with
                        | Some(thresh) -> res >= thresh
                        | _ -> false
                    )
                    | _ -> false

                let do_qr qr addr path depth state =
                    if qr.count <> 0
                    then
                    (
                        match Eval.test qr.guard state path with
                        | True(state_)
                        | Both({t = state_; _}) ->
                        (
                            if qr.count > 0 then qr.count <- qr.count - 1;

                            Logger.result "@[<h>%a: reached@]" pp_qr qr;

                            let state_ =
                                if qr.merge
                                then 
                                (
                                    let merge_state =
                                        match qr.merge_state with
                                        | Some(merge_state) -> S.merge ~parent:merge_state state_ merge_state
                                        | _ -> state_
                                    in
                                    qr.merge_state <- Some(merge_state);
                                    merge_state
                                )
                                else state_
                            in

                            let controlled = List.map (fun {Dba.Var.name; _} -> name) @@ Controlled.get path in

                            let fml = S.to_formula state_ in
                            let optim fml =
                                let keep = Formula.fold_backward 
                                    (
                                        fun e res ->
                                            match e.Formula.entry_desc with
                                            | Define({def_desc = Formula.BvDef(var, _, _); _}) -> 
                                            (
                                                try
                                                    ignore @@ List.find (fun name -> var.Formula.bv_name = name) controlled;
                                                    Formula.VarSet.add (Formula.BvVar(var)) res
                                                with Not_found -> res
                                            )
                                            | _ -> res
                                    )
                                    fml Formula.VarSet.empty 
                                in
                                Formula_transformation.optimize_from_options ?is_controlled:None ~keep fml
                            in
                            let fml = optim 
                                @@ BlastArrays.remove_arrays 
                                @@ optim fml
                            in
                            let do_qr () =
                                Formula_pp.pp_formula Format.str_formatter fml;
                                let fml = Format.flush_str_formatter () in
                                let res = Popcon.query ~controlled fml in
                                Logger.result "@[<h>%a: %s@]" pp_qr qr @@ Popcon.pp res;
                                if check_thresh qr res 
                                then 
                                (
                                    qr.count <- 0;
                                    Logger.result "@[<h>%a: OK@]" pp_qr qr
                                )
                                ;
                                if qr.count = 0 then active := !active - 1
                            in
                            let do_rr () =
                                match RReach.check controlled fml with
                                | RReach.Ok(true) -> 
                                (
                                    Logger.result "@[<h>%a: robustly reachable@]" pp_qr qr;
                                    let _ =
                                        match qr.thresh with
                                        | Some(_) ->
                                        (
                                            qr.count <- 0;
                                            Logger.result "@[<h>%a: OK@]" pp_qr qr
                                        )
                                        | _ -> ()
                                    in
                                    if qr.count = 0 then active := !active - 1;
                                    false
                                )
                                | Ok(false) -> true
                                | Error(msg) ->
                                (
                                    Logger.result "@[<h>%a: robust reachability query failed with <%s>@]" pp_qr qr msg;
                                    true
                                )
                            in
                            match RR.get () with
                            | "off" -> do_qr ()
                            | "only" -> if do_rr () then Logger.result "@[<h>%a: not robustly reachable@]" pp_qr qr
                            | _ -> if do_rr () then do_qr ()
                        )
                        | _ -> ()
                    )

                let do_qr qr addr path depth state =
                    do_qr qr addr path depth state;
                    if !active = 0 then Logger.result "@[<h>ALL DONE@]";
                    if !active = 0 && Stop.get()
                    then Error(Halt)
                    else 
                    (
                        if !active = 0 then active := (-1);
                        Ok(state)
                    )
            end

        let initialization_callback = None

        let declaration_callback = None

        let instrcb inst env =
            match inst with
            | CheckQRInstr(tag, count, guard, thresh, merge) -> [Ir.Builtin(QR.CheckQR(QR.create env tag count guard thresh merge))]
            | Controlled(lv) ->
            (
                let name = lazy
                    (
                        let name = Printf.sprintf "controlled_%d" !Controlled.cnt in
                        Controlled.cnt := !Controlled.cnt + 1;
                        name
                    )
                in
                match Script.eval_loc lv env with
                | Var(var) -> [Ir.Symbolize(var); Ir.Builtin(Controlled.Add(var))]
                | Restrict(var, {hi; lo}) ->
                (
                    let var_ = Dba.Var.temporary (Lazy.force name) (Size.Bit.create (hi - lo + 1)) in
                    let rval = Dba_utils.Expr.complement (Dba.Expr.v var_) ~lo ~hi var in
                    [Ir.Symbolize(var_); Ir.Builtin(Controlled.Add(var_)); Ir.Assign({var; rval})]
                )
                | Store(bsize, dir, addr, base) ->
                (
                    let var_ = Dba.Var.temporary (Lazy.force name) (Size.Bit.create (8 * bsize)) in
                    let rval = Dba.Expr.v var_ in
                    [Ir.Symbolize(var_); Ir.Builtin(Controlled.Add(var_)); Ir.Store({base; dir; addr; rval})]
                )
            )
            | _ -> []

        let instruction_callback = Some(instrcb)

        let process_callback = None

        let builtincb = function
            | QR.CheckQR(qr) -> Some(QR.do_qr qr)
            | Controlled.Add(var) -> Some(Controlled.add var)
            | _ -> None

        let builtin_callback = Some(builtincb)

        let pp_builtin fmt = function
            | QR.CheckQR(qr) -> 
            (
                QR.pp_qr fmt qr;
                true
            )
            | Controlled.Add(var) ->
            (
                Controlled.pp_add_ctrl fmt var;
                true
            )
            | _ -> false

        let builtin_printer = Some(pp_builtin)

        let at_exit_callback = None
    end

module PlugReg =
    struct
        let name = "qrse"

        let grammar_extension = gram_ext

        let instruction_printer = Some(pp_instr)

        let declaration_printer = None

        let extension :
            type a b.
                (module EXPLORATION_STATISTICS) ->
                (module Path.S with type t = a) ->
                (module STATE with type t = b) ->
                (module Exec.EXTENSION with type path = a and type state = b) option 
            =
            fun _stats path state ->
                if is_enabled () 
                then Some(module State (val path) (val state))
                else None
    end

let _ =
    Exec.register_plugin (module PlugReg : Exec.PLUGIN)
