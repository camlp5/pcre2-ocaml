open OUnit2
(**pp -syntax camlp5o -package pa_ppx.deriving_plugins.std *)

let test_special_char_regexps ctxt =
  ();
  assert_equal "\n" ([%match {|\n$|} / s exc pcre2 strings] "\n");
  assert_equal "" ([%subst {|\n+$|} / {||} / s pcre2] "\n\n")

let test_pcre2_simple_match ctxt =
  ();
  assert_equal "abc"
    (Pcre2.get_substring ([%match "abc" / exc raw pcre2] "abc") 0);
  assert_equal (Some "abc") ([%match "abc" / pcre2] "abc");
  assert_equal (Some "abc") ([%match "abc" / strings pcre2] "abc");
  assert_equal true ([%match "abc" / pred pcre2] "abc");
  assert_equal false ([%match "abc" / pred pcre2] "abd");
  assert_equal None ([%match "abc" / pcre2] "abd");
  assert_raises Not_found (fun () -> [%match "abc" / exc pcre2] "abd");
  assert_raises Not_found (fun () -> [%match "abc" / exc strings pcre2] "abd");
  assert_equal None ([%match "abc" / strings pcre2] "abd");
  assert_equal "abc" ([%match "abc" / exc strings pcre2] "abc");
  assert_equal ("abc", Some "b") ([%match "a(b)c" / exc strings pcre2] "abc");
  assert_equal ("ac", None) ([%match "a(?:(b)?)c" / exc strings pcre2] "ac");
  assert_equal "abc"
    (Pcre2.get_substring ([%match "ABC" / exc raw i pcre2] "abc") 0);
  assert_equal
    ("abc", Some "a", Some "b", Some "c")
    ([%match "(a)(b)(c)" / exc strings pcre2] "abc")

let test_pcre2_selective_match ctxt =
  ();
  assert_equal ("abc", Some "b")
    ([%match "a(b)c" / exc strings (!0, 1) pcre2] "abc");
  assert_equal ("abc", "b")
    ([%match "a(b)c" / exc strings (!0, !1) pcre2] "abc");
  assert_equal "b" ([%match "a(b)c" / exc strings !1 pcre2] "abc");
  assert_equal
    (Some ("abc", "b"))
    ([%match "a(b)c" / strings (!0, !1) pcre2] "abc");
  assert_equal ("ac", None) ([%match "a(b)?c" / exc strings (!0, 1) pcre2] "ac");
  assert_raises Not_found (fun _ ->
      [%match "a(b)?c" / exc strings (!0, !1) pcre2] "ac");
  assert_equal None ([%match "a(b)?c" / strings (!0, !1) pcre2] "ac")

let test_pcre2_search ctxt =
  ();
  assert_equal "abc" ([%match "abc" / exc strings pcre2] "zzzabc");
  assert_equal None ([%match "^abc" / strings pcre2] "zzzabc")

let show_string_option = function
  | None -> "None"
  | Some s -> Printf.sprintf "Some %s" s

let test_pcre2_single ctxt =
  let printer = show_string_option in
  ();
  assert_equal ~printer None ([%match ".+" / pcre2] "\n\n");
  assert_equal ~printer None ([%match ".+" / m pcre2 strings] "\n\n");

  assert_equal ~printer None ([%match ".+" / pcre2 strings] "\n\n");
  assert_equal ~printer (Some "\n\n") ([%match ".+" / s pcre2 strings] "\n\n");
  assert_equal ~printer None ([%match ".+" / m pcre2 strings] "\n\n");

  let printer x = x in
  ();
  assert_equal ~printer "\n\n" ([%match ".+" / s exc pcre2 strings] "\n\n");
  assert_equal ~printer "<<abc>>\ndef"
    ([%subst ".+" / {|<<$0>>|} / pcre2] "abc\ndef");
  assert_equal ~printer "<<abc\ndef>>"
    ([%subst ".+" / {|<<$0>>|} / s pcre2] "abc\ndef");
  assert_equal ~printer "<<abc>>\ndef"
    ([%subst ".+" / {|<<$0>>|} / m pcre2] "abc\ndef");

  assert_equal ~printer "<<abc>>\ndef"
    ([%subst ".*" / {|<<$0>>|} / pcre2] "abc\ndef");
  assert_equal ~printer "<<abc>><<>>\n<<def>><<>>"
    ([%subst ".*" / {|<<$0>>|} / g pcre2] "abc\ndef");
  assert_equal ~printer "<<abc>>\n<<def>>"
    ([%subst ".+" / {|<<$0>>|} / g pcre2] "abc\ndef");
  assert_equal ~printer "<<abc>>a\nc<<aec>>"
    ([%subst "a.c" / {|<<$0>>|} / g pcre2] "abca\ncaec");
  assert_equal ~printer "<<abc>><<a\nc>><<aec>>"
    ([%subst "a.c" / {|<<$0>>|} / g s pcre2] "abca\ncaec")

let test_pcre2_multiline ctxt =
  ();
  assert_equal (Some "bar") ([%match ".+$" / strings pcre2] "foo\nbar");
  assert_equal (Some "foo") ([%match ".+$" / m strings pcre2] "foo\nbar")

let test_pcre2_simple_split ctxt =
  ();
  assert_equal [ "bb" ] ([%split "a" / pcre2] "bb")

let test_pcre2_delim_split_raw ctxt =
  let open Pcre2 in
  ();
  assert_equal
    [ Delim "a"; Text "b"; Delim "a"; Text "b" ]
    ([%split "a" / pcre2 raw] "ababa");
  assert_equal
    [ Delim "a"; Text "b"; Delim "a"; Delim "a"; Text "b" ]
    ([%split "a" / pcre2 raw] "abaaba");
  assert_equal
    [
      Delim "a";
      NoGroup;
      Text "b";
      Delim "ac";
      Group (1, "c");
      Text "b";
      Delim "a";
      NoGroup;
    ]
    ([%split "a(c)?" / pcre2 raw] "abacba");
  assert_equal
    [
      Delim "ac";
      Group (1, "c");
      Text "b";
      Delim "ac";
      Group (1, "c");
      Text "b";
      Delim "ac";
      Group (1, "c");
    ]
    ([%split "a(c)" / pcre2 raw] "acbacbac");
  assert_equal
    [
      Delim "ac";
      Group (1, "c");
      Text "b";
      Delim "ac";
      Group (1, "c");
      Text "b";
      Delim "ac";
      Group (1, "c");
    ]
    ([%split "a(c)" / pcre2 raw] "acbacbac");
  assert_equal
    [
      Delim "a";
      NoGroup;
      Text "b";
      Delim "ac";
      Group (1, "c");
      Text "b";
      Delim "a";
      NoGroup;
    ]
    ([%split "a(c)?" / pcre2 raw] "abacba");
  assert_equal
    [ Text "ab"; Delim "x"; Group (1, "x"); NoGroup; Text "cd" ]
    ([%split {|(x)|(u)|} / raw pcre2] "abxcd");
  assert_equal
    [
      Text "ab";
      Delim "x";
      Group (1, "x");
      NoGroup;
      Text "cd";
      Delim "u";
      NoGroup;
      Group (2, "u");
    ]
    ([%split {|(x)|(u)|} / raw pcre2] "abxcdu")

let test_pcre2_string_pattern ctxt =
  ();
  assert_equal "$b"
    ([%pattern {|$$$1|} / pcre2] ([%match "a(b)c" / exc pcre2 raw] "abc"));
  assert_equal "b"
    ([%pattern {|${01}|} / pcre2] ([%match "a(b)c" / exc pcre2 raw] "abc"));
  assert_equal "bx"
    (let s = "x" in
     [%pattern {|${01}${s}|} / pcre2] ([%match "a(b)c" / exc pcre2 raw] "abc"));
  assert_equal {|"bx|}
    (let s = "x" in
     [%pattern {|"${01}${s}|} / pcre2] ([%match "a(b)c" / exc pcre2 raw] "abc"));
  assert_equal {|"x|}
    (let s = "x" in
     [%pattern {|"${s}|} / pcre2])

let test_pcre2_expr_pattern ctxt =
  ();
  assert_equal "abc"
    ([%pattern "$0$" / e pcre2] ([%match "abc" / exc pcre2 raw] "abc"));
  assert_equal "abcx"
    ([%pattern {|$0$ ^ "x"|} / e pcre2] ([%match "abc" / exc pcre2 raw] "abc"));
  assert_equal "abcx"
    (let x = "x" in
     [%pattern {|$0$ ^ x|} / e pcre2] ([%match "abc" / exc pcre2 raw] "abc"));
  assert_equal "x"
    (let x = "x" in
     [%pattern {|"" ^ x|} / e pcre2])

let test_pcre2_subst ctxt =
  ();
  assert_equal "$b" ([%subst "a(b)c" / {|$$$1|} / pcre2] "abc");
  assert_equal "$b" ([%subst "A(B)C" / {|$$$1|} / i pcre2] "abc");
  assert_equal "$babc" ([%subst "A(B)C" / {|$$$1|} / i pcre2] "abcabc");
  assert_equal "$b$b" ([%subst "A(B)C" / {|$$$1|} / g i pcre2] "abcabc");
  assert_equal "$b$b" ([%subst "A(B)C" / {|"$" ^ $1$|} / e g i pcre2] "abcabc");
  assert_equal "$$" ([%subst "A(B)C" / {|"$"|} / e g i pcre2] "abcabc");
  assert_equal "$$" ([%subst "A(B)C" / {|$$|} / g i pcre2] "abcabc")

let test_pcre2_ocamlfind_bits ctxt =
  ();
  assert_equal ~printer:show_string_option (Some "-syntax camlp5o ")
    (snd
       ([%match {|^\(\*\*pp (.*?)\*\)|} / exc strings pcre2]
          {|(**pp -syntax camlp5o *)
|}))

let pcre2_envsubst envlookup s =
  let f s1 s2 =
    if s1 <> "" then envlookup s1
    else if s2 <> "" then envlookup s2
    else assert false
  in

  [%subst {|(?:\$\(([^)]+)\)|\$\{([^}]+)\})|} / {| f $1$ $2$ |} / g e pcre2] s

let test_pcre2_envsubst_via_replace ctxt =
  let f = function
    | "A" -> "res1"
    | "B" -> "res2"
    | _ -> failwith "unexpected arg in envsubst"
  in
  assert_equal "...res1...res2..." (pcre2_envsubst f {|...$(A)...${B}...|})

let suite =
  "Test pa_ppx_regexp"
  >::: [
         "pcre2 simple_match" >:: test_pcre2_simple_match;
         "pcre2 selective_match" >:: test_pcre2_selective_match;
         "pcre2 search" >:: test_pcre2_search;
         "pcre2 single" >:: test_pcre2_single;
         "pcre2 multiline" >:: test_pcre2_multiline;
         "pcre2 simple_split" >:: test_pcre2_simple_split;
         "pcre2 delim_split raw" >:: test_pcre2_delim_split_raw;
         "pcre2 string_pattern" >:: test_pcre2_string_pattern;
         "pcre2 expr_pattern" >:: test_pcre2_expr_pattern;
         "pcre2 subst" >:: test_pcre2_subst;
         "pcre2 ocamlfind bits" >:: test_pcre2_ocamlfind_bits;
         "pcre2 envsubst via replace" >:: test_pcre2_envsubst_via_replace;
         "pcre only_regexps" >:: test_special_char_regexps;
       ]

let _ = if not !Sys.interactive then run_test_tt_main suite else ()
