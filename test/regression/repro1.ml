
let () =
  let rex = Pcre2.regexp "a+" in
  let ws  = Pcre2.create_workspace ~size:20 () in
  ignore (Pcre2.pcre2_dfa_match ~rex ~workspace:ws "aaabbb");
  ignore (Hashtbl.hash ws)   (* Tag_val(ws.(3)=0) → read at -1 → SIGSEGV *)

