
let () =
  let rex = Pcre2.regexp "a+" in
  let ws  = Array.make 20 0 in
  ignore (Pcre2.pcre2_dfa_match ~rex ~workspace:ws "aaabbb");
  ignore (Hashtbl.hash ws)   (* Tag_val(ws.(3)=0) → read at -1 → SIGSEGV *)

