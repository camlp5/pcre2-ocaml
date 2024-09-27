open OUnit2
open Pcre2

let test_full_split ctxt =
  assert_equal 0 0
  ; assert_equal [Text "ab"; Delim "x"; Group (1, "x"); NoGroup; Text "cd"]
      (full_split ~pat:"(x)|(u)" "abxcd")
  ; assert_equal [Text "ab"; Delim "x"; Group (1, "x"); NoGroup; Text "cd"; Delim "u";
                  NoGroup; Group (2, "u"); Text "ef"]
      (full_split ~pat:"(x)|(u)" "abxcduef")

let test_exec_all ctxt =
  let assert_matches ~pat subj expected =
    let assert_substrings_equal (subj_exp, ovec_exp) (subj_act, ovec_act) =
      assert_equal subj_exp subj_act;
      Array.iter2 assert_equal ovec_exp ovec_act
    in
    let expected_subj = Array.map (fun ovec -> subj, ovec) expected in
    let actual = exec_all ~pat subj in
    assert_equal (Array.length expected_subj) (Array.length actual);
    Array.iter2 assert_substrings_equal expected_subj actual
  in
  (* A pattern with no matches should raise Not_found. *)
  assert_raises Not_found (fun () -> exec_all ~pat:"empty" "");
  assert_raises Not_found (fun () -> exec_all ~pat:"empty" "empt");
  (* Single matches of non-zero-length patterns. *)
  assert_matches ~pat:"p" "p" [| [|0; 1; 0|] |];
  assert_matches ~pat:"pattern" "pattern" [| [|0; 7; 0|] |];
  assert_matches ~pat:"pattern" "This is a pattern." [| [|10; 17; 0|] |];
  (* Multiple matches of non-zero-length patterns. *)
  assert_matches ~pat:"a" "aaa" [| [|0; 1; 0|]; [|1; 2; 0|]; [|2; 3; 0|] |];
  assert_matches ~pat:"hello|ocaml" "hello ocaml"
    [| [|0; 5; 0|]; [|6; 11; 0|] |];
  assert_matches ~pat:"(hello|ocaml)" "hello ocaml"
    [| [|0; 5; 0; 5; 0; 0|]; [|6; 11; 6; 11; 0; 0|] |];
  assert_matches ~pat:"(hello|(ocaml))" "hello ocaml"
    [| [|0; 5; 0; 5; -1; -1; 0; 0; 0|]; [|6; 11; 6; 11; 6; 11; 0; 0; 0|] |];
  (* Matches of zero-length patterns. *)
  assert_matches ~pat:"(?=(hello|ocaml))" "hellocaml"
    [| [|0; 0; 0; 5; 0; 0|]; [|4; 4; 4; 9; 0; 0|] |];
  assert_matches ~pat:"(?=(hello|ocaml))|language" "hellocamlanguage"
    [| [|0; 0; 0; 5; 0; 0|]; [|4; 4; 4; 9; 0; 0|]; [|8; 16; -1; -1; 0; 0|] |];
  assert_matches ~pat:"(?=(hello|ocaml))|language" "hellocamllanguage"
    [| [|0; 0; 0; 5; 0; 0|]; [|4; 4; 4; 9; 0; 0|]; [|9; 17; -1; -1; 0; 0|] |]

let suite = "Test pcre2" >::: [
      "test_full_split" >:: test_full_split;
      "test_exec_all"   >:: test_exec_all;
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

