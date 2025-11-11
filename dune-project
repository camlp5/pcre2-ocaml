(lang dune 2.7)

(name pcre2)

(version 8.0.4)

(generate_opam_files true)

(source
 (github camlp5/pcre2-ocaml))

(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")

(homepage "https://github.com/camlp5/pcre2-ocaml")

(maintainers "Chet Murthy <chetsky@gmail.com>")

(authors "Markus Mottl <markus.mottl@gmail.com>")

(package
 (name pcre2)
 (synopsis
   "Bindings to the Perl Compatibility Regular Expressions library (version 2)")
 (description
   "pcre2-ocaml offers library functions for string pattern matching and\nsubstitution, similar to the functionality offered by the Perl language.")
 (depends
  (ocaml
   (>= 4.08))
  dune-compiledb
  dune-configurator
  (conf-libpcre2-8 :build)
  (ounit2 :with-test)))
