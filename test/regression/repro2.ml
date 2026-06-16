
let () =
  let ws  = Pcre2.create_workspace ~size:20 () in
  let rex = Pcre2.regexp ~flags:[`AUTO_CALLOUT] "a+" in
  let callout _ = ignore (Hashtbl.hash ws) in
  ignore (Pcre2.pcre2_dfa_match ~rex ~callout ~workspace:ws "aaabbb")
