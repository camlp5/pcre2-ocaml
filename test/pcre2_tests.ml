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
    let rec assert_match_equal idx expected actual =
      match expected with
      | (exp_subj, exp_start, exp_stop) :: exp_tl ->
          assert_equal exp_subj (get_substring actual idx);
          let act_start, act_stop = get_substring_ofs actual idx in
          assert_equal exp_start act_start;
          assert_equal exp_stop act_stop;
          assert_match_equal (succ idx) exp_tl actual
      | [] -> ()
    and assert_substrings_equal expected actual =
      match expected, actual with
      | exp_hd :: exp_tl, act_hd :: act_tl ->
          assert_equal (List.length exp_hd) (num_of_subs act_hd);
          assert_match_equal 0 exp_hd act_hd;
          assert_substrings_equal exp_tl act_tl
      | [], [] -> ()
      | _, _ -> failwith "Unequal substring list lengths"
    in
    let actual = exec_all ~pat subj in
    assert_equal (List.length expected) (Array.length actual);
    assert_substrings_equal expected @@ Array.to_list actual
  in
  (* A pattern with no matches should raise Not_found. *)
  assert_raises Not_found (fun () -> exec_all ~pat:"empty" "");
  assert_raises Not_found (fun () -> exec_all ~pat:"empty" "empt");
  (* Single matches of non-zero-length patterns. *)
  assert_matches ~pat:"p" "p" [[("p", 0, 1)]];
  assert_matches ~pat:"pattern" "pattern" [[("pattern", 0, 7)]];
  (* Multiple matches of non-zero-length patterns. *)
  assert_matches ~pat:"a" "aaa" [[("a", 0, 1)]; [("a", 1, 2)]; [("a", 2, 3)]];
  assert_matches ~pat:"hello|ocaml" "hello ocaml"
    [[("hello", 0, 5)]; [("ocaml", 6, 11)]];
  assert_matches ~pat:"(hello|ocaml)" "hello ocaml"
    [[("hello", 0, 5); ("hello", 0, 5)]; [("ocaml", 6, 11); ("ocaml", 6, 11)]];
  (* Matches of zero-length patterns. *)
  assert_matches ~pat:"(?=(hello|ocaml))" "hello ocaml"
    [[("", 0, 0); ("hello", 0, 5)]; [("", 6, 6); ("ocaml", 6, 11)]];
  assert_matches ~pat:"(?=(hello|ocaml))" "hellocaml"
    [[("", 0, 0); ("hello", 0, 5)]; [("", 4, 4); ("ocaml", 4, 9)]]

let suite = "Test pcre2" >::: [
      "test_full_split" >:: test_full_split;
      "test_exec_all"   >:: test_exec_all;
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

