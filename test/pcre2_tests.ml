open OUnit2
(**pp -syntax camlp5o -package pa_ppx.deriving_plugins.std *)

let test_special_char_regexps ctxt =
  ();
  assert_equal "\n"
    ((let __re__ = Pcre2.regexp ~flags:[ `DOTALL ] "\\n$" in
      fun __subj__ ->
        (fun __g__ -> Pcre2.get_substring __g__ 0)
          (Pcre2.exec ~rex:__re__ __subj__))
       "\n");
  assert_equal ""
    (Pcre2.substitute_substrings_first
       ~rex:(Pcre2.regexp ~flags:[ `DOTALL ] "\\n+$")
       ~subst:(fun __g__ -> String.concat "" [])
       "\n\n")

let test_pcre2_simple_match ctxt =
  ();
  assert_equal "abc"
    (Pcre2.get_substring
       ((let __re__ = Pcre2.regexp ~flags:[] "abc" in
         fun __subj__ -> Pcre2.exec ~rex:__re__ __subj__)
          "abc")
       0);
  assert_equal (Some "abc")
    ((let __re__ = Pcre2.regexp ~flags:[] "abc" in
      fun __subj__ ->
        match
          Option.map
            (fun __g__ -> Pcre2.get_substring __g__ 0)
            (try Some (Pcre2.exec ~rex:__re__ __subj__) with Not_found -> None)
        with
        | exception Not_found -> None
        | rv -> rv)
       "abc");
  assert_equal (Some "abc")
    ((let __re__ = Pcre2.regexp ~flags:[] "abc" in
      fun __subj__ ->
        match
          Option.map
            (fun __g__ -> Pcre2.get_substring __g__ 0)
            (try Some (Pcre2.exec ~rex:__re__ __subj__) with Not_found -> None)
        with
        | exception Not_found -> None
        | rv -> rv)
       "abc");
  assert_equal true
    ((let __re__ = Pcre2.regexp ~flags:[] "abc" in
      fun __subj__ -> Pcre2.pmatch ~rex:__re__ __subj__)
       "abc");
  assert_equal false
    ((let __re__ = Pcre2.regexp ~flags:[] "abc" in
      fun __subj__ -> Pcre2.pmatch ~rex:__re__ __subj__)
       "abd");
  assert_equal None
    ((let __re__ = Pcre2.regexp ~flags:[] "abc" in
      fun __subj__ ->
        match
          Option.map
            (fun __g__ -> Pcre2.get_substring __g__ 0)
            (try Some (Pcre2.exec ~rex:__re__ __subj__) with Not_found -> None)
        with
        | exception Not_found -> None
        | rv -> rv)
       "abd");
  assert_raises Not_found (fun () ->
      (let __re__ = Pcre2.regexp ~flags:[] "abc" in
       fun __subj__ ->
         (fun __g__ -> Pcre2.get_substring __g__ 0)
           (Pcre2.exec ~rex:__re__ __subj__))
        "abd");
  assert_raises Not_found (fun () ->
      (let __re__ = Pcre2.regexp ~flags:[] "abc" in
       fun __subj__ ->
         (fun __g__ -> Pcre2.get_substring __g__ 0)
           (Pcre2.exec ~rex:__re__ __subj__))
        "abd");
  assert_equal None
    ((let __re__ = Pcre2.regexp ~flags:[] "abc" in
      fun __subj__ ->
        match
          Option.map
            (fun __g__ -> Pcre2.get_substring __g__ 0)
            (try Some (Pcre2.exec ~rex:__re__ __subj__) with Not_found -> None)
        with
        | exception Not_found -> None
        | rv -> rv)
       "abd");
  assert_equal "abc"
    ((let __re__ = Pcre2.regexp ~flags:[] "abc" in
      fun __subj__ ->
        (fun __g__ -> Pcre2.get_substring __g__ 0)
          (Pcre2.exec ~rex:__re__ __subj__))
       "abc");
  assert_equal ("abc", Some "b")
    ((let __re__ = Pcre2.regexp ~flags:[] "a(b)c" in
      fun __subj__ ->
        (fun __g__ ->
          ( Pcre2.get_substring __g__ 0,
            try Some (Pcre2.get_substring __g__ 1) with Not_found -> None ))
          (Pcre2.exec ~rex:__re__ __subj__))
       "abc");
  assert_equal ("ac", None)
    ((let __re__ = Pcre2.regexp ~flags:[] "a(?:(b)?)c" in
      fun __subj__ ->
        (fun __g__ ->
          ( Pcre2.get_substring __g__ 0,
            try Some (Pcre2.get_substring __g__ 1) with Not_found -> None ))
          (Pcre2.exec ~rex:__re__ __subj__))
       "ac");
  assert_equal "abc"
    (Pcre2.get_substring
       ((let __re__ = Pcre2.regexp ~flags:[ `CASELESS ] "ABC" in
         fun __subj__ -> Pcre2.exec ~rex:__re__ __subj__)
          "abc")
       0);
  assert_equal
    ("abc", Some "a", Some "b", Some "c")
    ((let __re__ = Pcre2.regexp ~flags:[] "(a)(b)(c)" in
      fun __subj__ ->
        (fun __g__ ->
          ( Pcre2.get_substring __g__ 0,
            (try Some (Pcre2.get_substring __g__ 1) with Not_found -> None),
            (try Some (Pcre2.get_substring __g__ 2) with Not_found -> None),
            try Some (Pcre2.get_substring __g__ 3) with Not_found -> None ))
          (Pcre2.exec ~rex:__re__ __subj__))
       "abc")

let test_pcre2_selective_match ctxt =
  ();
  assert_equal ("abc", Some "b")
    ((let __re__ = Pcre2.regexp ~flags:[] "a(b)c" in
      fun __subj__ ->
        (fun __g__ ->
          ( Pcre2.get_substring __g__ 0,
            try Some (Pcre2.get_substring __g__ 1) with Not_found -> None ))
          (Pcre2.exec ~rex:__re__ __subj__))
       "abc");
  assert_equal ("abc", "b")
    ((let __re__ = Pcre2.regexp ~flags:[] "a(b)c" in
      fun __subj__ ->
        (fun __g__ ->
          (Pcre2.get_substring __g__ 0, Pcre2.get_substring __g__ 1))
          (Pcre2.exec ~rex:__re__ __subj__))
       "abc");
  assert_equal "b"
    ((let __re__ = Pcre2.regexp ~flags:[] "a(b)c" in
      fun __subj__ ->
        (fun __g__ -> Pcre2.get_substring __g__ 1)
          (Pcre2.exec ~rex:__re__ __subj__))
       "abc");
  assert_equal
    (Some ("abc", "b"))
    ((let __re__ = Pcre2.regexp ~flags:[] "a(b)c" in
      fun __subj__ ->
        match
          Option.map
            (fun __g__ ->
              (Pcre2.get_substring __g__ 0, Pcre2.get_substring __g__ 1))
            (try Some (Pcre2.exec ~rex:__re__ __subj__) with Not_found -> None)
        with
        | exception Not_found -> None
        | rv -> rv)
       "abc");
  assert_equal ("ac", None)
    ((let __re__ = Pcre2.regexp ~flags:[] "a(b)?c" in
      fun __subj__ ->
        (fun __g__ ->
          ( Pcre2.get_substring __g__ 0,
            try Some (Pcre2.get_substring __g__ 1) with Not_found -> None ))
          (Pcre2.exec ~rex:__re__ __subj__))
       "ac");
  assert_raises Not_found (fun _ ->
      (let __re__ = Pcre2.regexp ~flags:[] "a(b)?c" in
       fun __subj__ ->
         (fun __g__ ->
           (Pcre2.get_substring __g__ 0, Pcre2.get_substring __g__ 1))
           (Pcre2.exec ~rex:__re__ __subj__))
        "ac");
  assert_equal None
    ((let __re__ = Pcre2.regexp ~flags:[] "a(b)?c" in
      fun __subj__ ->
        match
          Option.map
            (fun __g__ ->
              (Pcre2.get_substring __g__ 0, Pcre2.get_substring __g__ 1))
            (try Some (Pcre2.exec ~rex:__re__ __subj__) with Not_found -> None)
        with
        | exception Not_found -> None
        | rv -> rv)
       "ac")

let test_pcre2_search ctxt =
  ();
  assert_equal "abc"
    ((let __re__ = Pcre2.regexp ~flags:[] "abc" in
      fun __subj__ ->
        (fun __g__ -> Pcre2.get_substring __g__ 0)
          (Pcre2.exec ~rex:__re__ __subj__))
       "zzzabc");
  assert_equal None
    ((let __re__ = Pcre2.regexp ~flags:[] "^abc" in
      fun __subj__ ->
        match
          Option.map
            (fun __g__ -> Pcre2.get_substring __g__ 0)
            (try Some (Pcre2.exec ~rex:__re__ __subj__) with Not_found -> None)
        with
        | exception Not_found -> None
        | rv -> rv)
       "zzzabc")

let show_string_option = function
  | None -> "None"
  | Some s -> Printf.sprintf "Some %s" s

let test_pcre2_single ctxt =
  let printer = show_string_option in
  ();
  assert_equal ~printer None
    ((let __re__ = Pcre2.regexp ~flags:[] ".+" in
      fun __subj__ ->
        match
          Option.map
            (fun __g__ -> Pcre2.get_substring __g__ 0)
            (try Some (Pcre2.exec ~rex:__re__ __subj__) with Not_found -> None)
        with
        | exception Not_found -> None
        | rv -> rv)
       "\n\n");
  assert_equal ~printer None
    ((let __re__ = Pcre2.regexp ~flags:[ `MULTILINE ] ".+" in
      fun __subj__ ->
        match
          Option.map
            (fun __g__ -> Pcre2.get_substring __g__ 0)
            (try Some (Pcre2.exec ~rex:__re__ __subj__) with Not_found -> None)
        with
        | exception Not_found -> None
        | rv -> rv)
       "\n\n");
  assert_equal ~printer None
    ((let __re__ = Pcre2.regexp ~flags:[] ".+" in
      fun __subj__ ->
        match
          Option.map
            (fun __g__ -> Pcre2.get_substring __g__ 0)
            (try Some (Pcre2.exec ~rex:__re__ __subj__) with Not_found -> None)
        with
        | exception Not_found -> None
        | rv -> rv)
       "\n\n");
  assert_equal ~printer (Some "\n\n")
    ((let __re__ = Pcre2.regexp ~flags:[ `DOTALL ] ".+" in
      fun __subj__ ->
        match
          Option.map
            (fun __g__ -> Pcre2.get_substring __g__ 0)
            (try Some (Pcre2.exec ~rex:__re__ __subj__) with Not_found -> None)
        with
        | exception Not_found -> None
        | rv -> rv)
       "\n\n");
  assert_equal ~printer None
    ((let __re__ = Pcre2.regexp ~flags:[ `MULTILINE ] ".+" in
      fun __subj__ ->
        match
          Option.map
            (fun __g__ -> Pcre2.get_substring __g__ 0)
            (try Some (Pcre2.exec ~rex:__re__ __subj__) with Not_found -> None)
        with
        | exception Not_found -> None
        | rv -> rv)
       "\n\n");
  let printer x = x in
  ();
  assert_equal ~printer "\n\n"
    ((let __re__ = Pcre2.regexp ~flags:[ `DOTALL ] ".+" in
      fun __subj__ ->
        (fun __g__ -> Pcre2.get_substring __g__ 0)
          (Pcre2.exec ~rex:__re__ __subj__))
       "\n\n");
  assert_equal ~printer "<<abc>>\ndef"
    (Pcre2.substitute_substrings_first
       ~rex:(Pcre2.regexp ~flags:[] ".+")
       ~subst:(fun __g__ ->
         String.concat ""
           [
             "<<";
             (match Pcre2.get_substring __g__ 0 with
             | exception Not_found -> ""
             | s -> s);
             ">>";
           ])
       "abc\ndef");
  assert_equal ~printer "<<abc\ndef>>"
    (Pcre2.substitute_substrings_first
       ~rex:(Pcre2.regexp ~flags:[ `DOTALL ] ".+")
       ~subst:(fun __g__ ->
         String.concat ""
           [
             "<<";
             (match Pcre2.get_substring __g__ 0 with
             | exception Not_found -> ""
             | s -> s);
             ">>";
           ])
       "abc\ndef");
  assert_equal ~printer "<<abc>>\ndef"
    (Pcre2.substitute_substrings_first
       ~rex:(Pcre2.regexp ~flags:[ `MULTILINE ] ".+")
       ~subst:(fun __g__ ->
         String.concat ""
           [
             "<<";
             (match Pcre2.get_substring __g__ 0 with
             | exception Not_found -> ""
             | s -> s);
             ">>";
           ])
       "abc\ndef");
  assert_equal ~printer "<<abc>>\ndef"
    (Pcre2.substitute_substrings_first
       ~rex:(Pcre2.regexp ~flags:[] ".*")
       ~subst:(fun __g__ ->
         String.concat ""
           [
             "<<";
             (match Pcre2.get_substring __g__ 0 with
             | exception Not_found -> ""
             | s -> s);
             ">>";
           ])
       "abc\ndef");
  assert_equal ~printer "<<abc>><<>>\n<<def>><<>>"
    (Pcre2.substitute_substrings
       ~rex:(Pcre2.regexp ~flags:[] ".*")
       ~subst:(fun __g__ ->
         String.concat ""
           [
             "<<";
             (match Pcre2.get_substring __g__ 0 with
             | exception Not_found -> ""
             | s -> s);
             ">>";
           ])
       "abc\ndef");
  assert_equal ~printer "<<abc>>\n<<def>>"
    (Pcre2.substitute_substrings
       ~rex:(Pcre2.regexp ~flags:[] ".+")
       ~subst:(fun __g__ ->
         String.concat ""
           [
             "<<";
             (match Pcre2.get_substring __g__ 0 with
             | exception Not_found -> ""
             | s -> s);
             ">>";
           ])
       "abc\ndef");
  assert_equal ~printer "<<abc>>a\nc<<aec>>"
    (Pcre2.substitute_substrings
       ~rex:(Pcre2.regexp ~flags:[] "a.c")
       ~subst:(fun __g__ ->
         String.concat ""
           [
             "<<";
             (match Pcre2.get_substring __g__ 0 with
             | exception Not_found -> ""
             | s -> s);
             ">>";
           ])
       "abca\ncaec");
  assert_equal ~printer "<<abc>><<a\nc>><<aec>>"
    (Pcre2.substitute_substrings
       ~rex:(Pcre2.regexp ~flags:[ `DOTALL ] "a.c")
       ~subst:(fun __g__ ->
         String.concat ""
           [
             "<<";
             (match Pcre2.get_substring __g__ 0 with
             | exception Not_found -> ""
             | s -> s);
             ">>";
           ])
       "abca\ncaec")

let test_pcre2_multiline ctxt =
  ();
  assert_equal (Some "bar")
    ((let __re__ = Pcre2.regexp ~flags:[] ".+$" in
      fun __subj__ ->
        match
          Option.map
            (fun __g__ -> Pcre2.get_substring __g__ 0)
            (try Some (Pcre2.exec ~rex:__re__ __subj__) with Not_found -> None)
        with
        | exception Not_found -> None
        | rv -> rv)
       "foo\nbar");
  assert_equal (Some "foo")
    ((let __re__ = Pcre2.regexp ~flags:[ `MULTILINE ] ".+$" in
      fun __subj__ ->
        match
          Option.map
            (fun __g__ -> Pcre2.get_substring __g__ 0)
            (try Some (Pcre2.exec ~rex:__re__ __subj__) with Not_found -> None)
        with
        | exception Not_found -> None
        | rv -> rv)
       "foo\nbar")

let test_pcre2_simple_split ctxt =
  ();
  assert_equal [ "bb" ]
    ((let __re__ = Pcre2.regexp ~flags:[] "a" in
      fun __subj__ -> Pcre2.split ~rex:__re__ __subj__)
       "bb")

let test_pcre2_delim_split_raw ctxt =
  let open Pcre2 in
  ();
  assert_equal
    [ Delim "a"; Text "b"; Delim "a"; Text "b" ]
    ((let __re__ = Pcre2.regexp ~flags:[] "a" in
      fun __subj__ -> Pcre2.full_split ~rex:__re__ __subj__)
       "ababa");
  assert_equal
    [ Delim "a"; Text "b"; Delim "a"; Delim "a"; Text "b" ]
    ((let __re__ = Pcre2.regexp ~flags:[] "a" in
      fun __subj__ -> Pcre2.full_split ~rex:__re__ __subj__)
       "abaaba");
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
    ((let __re__ = Pcre2.regexp ~flags:[] "a(c)?" in
      fun __subj__ -> Pcre2.full_split ~rex:__re__ __subj__)
       "abacba");
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
    ((let __re__ = Pcre2.regexp ~flags:[] "a(c)" in
      fun __subj__ -> Pcre2.full_split ~rex:__re__ __subj__)
       "acbacbac");
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
    ((let __re__ = Pcre2.regexp ~flags:[] "a(c)" in
      fun __subj__ -> Pcre2.full_split ~rex:__re__ __subj__)
       "acbacbac");
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
    ((let __re__ = Pcre2.regexp ~flags:[] "a(c)?" in
      fun __subj__ -> Pcre2.full_split ~rex:__re__ __subj__)
       "abacba");
  assert_equal
    [ Text "ab"; Delim "x"; Group (1, "x"); NoGroup; Text "cd" ]
    ((let __re__ = Pcre2.regexp ~flags:[] "(x)|(u)" in
      fun __subj__ -> Pcre2.full_split ~rex:__re__ __subj__)
       "abxcd");
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
    ((let __re__ = Pcre2.regexp ~flags:[] "(x)|(u)" in
      fun __subj__ -> Pcre2.full_split ~rex:__re__ __subj__)
       "abxcdu")

let test_pcre2_string_pattern ctxt =
  ();
  assert_equal "$b"
    ((fun __g__ ->
       String.concat ""
         [
           "$";
           "";
           (match Pcre2.get_substring __g__ 1 with
           | exception Not_found -> ""
           | s -> s);
         ])
       ((let __re__ = Pcre2.regexp ~flags:[] "a(b)c" in
         fun __subj__ -> Pcre2.exec ~rex:__re__ __subj__)
          "abc"));
  assert_equal "b"
    ((fun __g__ ->
       String.concat ""
         [
           (match Pcre2.get_substring __g__ 01 with
           | exception Not_found -> ""
           | s -> s);
         ])
       ((let __re__ = Pcre2.regexp ~flags:[] "a(b)c" in
         fun __subj__ -> Pcre2.exec ~rex:__re__ __subj__)
          "abc"));
  assert_equal "bx"
    (let s = "x" in
     (fun __g__ ->
       String.concat ""
         [
           (match Pcre2.get_substring __g__ 01 with
           | exception Not_found -> ""
           | s -> s);
           "";
           s;
         ])
       ((let __re__ = Pcre2.regexp ~flags:[] "a(b)c" in
         fun __subj__ -> Pcre2.exec ~rex:__re__ __subj__)
          "abc"));
  assert_equal "\"bx"
    (let s = "x" in
     (fun __g__ ->
       String.concat ""
         [
           "\"";
           (match Pcre2.get_substring __g__ 01 with
           | exception Not_found -> ""
           | s -> s);
           "";
           s;
         ])
       ((let __re__ = Pcre2.regexp ~flags:[] "a(b)c" in
         fun __subj__ -> Pcre2.exec ~rex:__re__ __subj__)
          "abc"));
  assert_equal "\"x"
    (let s = "x" in
     String.concat "" [ "\""; s ])

let test_pcre2_expr_pattern ctxt =
  ();
  assert_equal "abc"
    ((fun __g__ ->
       match Pcre2.get_substring __g__ 0 with
       | exception Not_found -> ""
       | s -> s)
       ((let __re__ = Pcre2.regexp ~flags:[] "abc" in
         fun __subj__ -> Pcre2.exec ~rex:__re__ __subj__)
          "abc"));
  assert_equal "abcx"
    ((fun __g__ ->
       (match Pcre2.get_substring __g__ 0 with
       | exception Not_found -> ""
       | s -> s)
       ^ "x")
       ((let __re__ = Pcre2.regexp ~flags:[] "abc" in
         fun __subj__ -> Pcre2.exec ~rex:__re__ __subj__)
          "abc"));
  assert_equal "abcx"
    (let x = "x" in
     (fun __g__ ->
       (match Pcre2.get_substring __g__ 0 with
       | exception Not_found -> ""
       | s -> s)
       ^ x)
       ((let __re__ = Pcre2.regexp ~flags:[] "abc" in
         fun __subj__ -> Pcre2.exec ~rex:__re__ __subj__)
          "abc"));
  assert_equal "x"
    (let x = "x" in
     "" ^ x)

let test_pcre2_subst ctxt =
  ();
  assert_equal "$b"
    (Pcre2.substitute_substrings_first
       ~rex:(Pcre2.regexp ~flags:[] "a(b)c")
       ~subst:(fun __g__ ->
         String.concat ""
           [
             "$";
             "";
             (match Pcre2.get_substring __g__ 1 with
             | exception Not_found -> ""
             | s -> s);
           ])
       "abc");
  assert_equal "$b"
    (Pcre2.substitute_substrings_first
       ~rex:(Pcre2.regexp ~flags:[ `CASELESS ] "A(B)C")
       ~subst:(fun __g__ ->
         String.concat ""
           [
             "$";
             "";
             (match Pcre2.get_substring __g__ 1 with
             | exception Not_found -> ""
             | s -> s);
           ])
       "abc");
  assert_equal "$babc"
    (Pcre2.substitute_substrings_first
       ~rex:(Pcre2.regexp ~flags:[ `CASELESS ] "A(B)C")
       ~subst:(fun __g__ ->
         String.concat ""
           [
             "$";
             "";
             (match Pcre2.get_substring __g__ 1 with
             | exception Not_found -> ""
             | s -> s);
           ])
       "abcabc");
  assert_equal "$b$b"
    (Pcre2.substitute_substrings
       ~rex:(Pcre2.regexp ~flags:[ `CASELESS ] "A(B)C")
       ~subst:(fun __g__ ->
         String.concat ""
           [
             "$";
             "";
             (match Pcre2.get_substring __g__ 1 with
             | exception Not_found -> ""
             | s -> s);
           ])
       "abcabc");
  assert_equal "$b$b"
    (Pcre2.substitute_substrings
       ~rex:(Pcre2.regexp ~flags:[ `CASELESS ] "A(B)C")
       ~subst:(fun __g__ ->
         "$"
         ^
         match Pcre2.get_substring __g__ 1 with
         | exception Not_found -> ""
         | s -> s)
       "abcabc");
  assert_equal "$$"
    (Pcre2.substitute_substrings
       ~rex:(Pcre2.regexp ~flags:[ `CASELESS ] "A(B)C")
       ~subst:(fun __g__ -> "$")
       "abcabc");
  assert_equal "$$"
    (Pcre2.substitute_substrings
       ~rex:(Pcre2.regexp ~flags:[ `CASELESS ] "A(B)C")
       ~subst:(fun __g__ -> String.concat "" [ "$" ])
       "abcabc")

let test_pcre2_ocamlfind_bits ctxt =
  ();
  assert_equal ~printer:show_string_option (Some "-syntax camlp5o ")
    (snd
       ((let __re__ = Pcre2.regexp ~flags:[] "^\\(\\*\\*pp (.*?)\\*\\)" in
         fun __subj__ ->
           (fun __g__ ->
             ( Pcre2.get_substring __g__ 0,
               try Some (Pcre2.get_substring __g__ 1) with Not_found -> None ))
             (Pcre2.exec ~rex:__re__ __subj__))
          "(**pp -syntax camlp5o *)\n"))

let pcre2_envsubst envlookup s =
  let f s1 s2 =
    if s1 <> "" then envlookup s1
    else if s2 <> "" then envlookup s2
    else assert false
  in
  Pcre2.substitute_substrings
    ~rex:(Pcre2.regexp ~flags:[] "(?:\\$\\(([^)]+)\\)|\\$\\{([^}]+)\\})")
    ~subst:(fun __g__ ->
      f
        (match Pcre2.get_substring __g__ 1 with
        | exception Not_found -> ""
        | s -> s)
        (match Pcre2.get_substring __g__ 2 with
        | exception Not_found -> ""
        | s -> s))
    s

let test_pcre2_envsubst_via_replace ctxt =
  let f = function
    | "A" -> "res1"
    | "B" -> "res2"
    | _ -> failwith "unexpected arg in envsubst"
  in
  assert_equal "...res1...res2..." (pcre2_envsubst f "...$(A)...${B}...")

let bad_pattern ctxt =
  let open Pcre2 in
  try
    ignore (regexp "?");
    assert_failure "Regex should fail to parse"
  with Error (BadPattern (s, _)) ->
    assert_bool
      "String contains a zero byte. In 8-bit mode this indicates an error in \
       the creation of the error message since strings created by PCRE2 should \
       be null terminated."
      (not @@ String.exists (fun c -> c = '\000') s)

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
         "bad_pattern" >:: bad_pattern;
       ]

let _ = if not !Sys.interactive then run_test_tt_main suite
