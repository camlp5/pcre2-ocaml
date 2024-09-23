open OUnit2
open Pcre2

let simple_test ctxt =
  assert_equal 0 0
  ; assert_equal [Text "ab"; Delim "x"; Group (1, "x"); NoGroup; Text "cd"]
      (full_split ~pat:"(x)|(u)" "abxcd")
  ; assert_equal [Text "ab"; Delim "x"; Group (1, "x"); NoGroup; Text "cd"; Delim "u";
                  NoGroup; Group (2, "u"); Text "ef"]
      (full_split ~pat:"(x)|(u)" "abxcduef")

let marshalled_string_termination ctxt =
  try
    (* At the time of writing, the longest error message that can be returned by
       PCRE2 is "\g is not followed by a braced, angle-bracketed, or quoted
       name/number or by a plain number". *)
    ignore @@ regexp "\\gg";
    assert_failure "Invalid pattern must fail to compile."
  with Error (BadPattern (msg, offset)) ->
    let is_non_printing c =
      let codepoint = Char.code c in
      codepoint < (Char.code ' ') || codepoint > (Char.code '~')
    in
    assert_equal offset 2;
    assert_bool "PCRE2 string contains non-printing character."
      (not @@ String.exists is_non_printing msg)

let suite = "Test pcre" >::: [
      "simple_test"                   >:: simple_test;
      "marshalled_string_termination" >:: marshalled_string_termination;
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
