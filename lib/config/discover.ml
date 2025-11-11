let () =
  let module C = Configurator.V1 in
  C.main ~name:"pcre2" (fun c ->
      let default : C.Pkg_config.package_conf =
        { libs = [ "-lpcre2-8" ]; cflags = [] }
      in
      let conf =
        match C.Pkg_config.get c with
        | None -> default
        | Some pc ->
            Option.value (C.Pkg_config.query pc ~package:"libpcre2-8") ~default
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
