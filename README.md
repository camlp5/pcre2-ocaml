# PCRE2-OCaml - Perl Compatibility Regular Expressions for OCaml

Fork of the original [pcre-ocaml project](https://github.com/mmottl/pcre-ocaml)
for PCRE2 support.

These are the bindings as needed by the
[Haxe compiler](https://github.com/HaxeFoundation/haxe). I do not plan on
maintaining this repository.

This [OCaml](http://www.ocaml.org) library interfaces with the C library
[PCRE2](http://www.pcre.org), providing Perl-compatible regular expressions for
string matching.

## Features

PCRE2-OCaml offers:

- Pattern searching
- Subpattern extraction
- String splitting by patterns
- Pattern substitution

Reasons to choose PCRE2-OCaml:

- The PCRE2 library by Philip Hazel is mature and stable, implementing nearly
  all Perl regular expression features. High-level OCaml functions (split,
  replace, etc.) are compatible with Perl functions, as much as OCaml allows.
  Some developers find Perl-style regex syntax more intuitive and powerful than
  the Emacs-style regex used in OCaml's `Str` module.

- PCRE2-OCaml is reentrant and thread-safe, unlike the `Str` module. This
  reentrancy offers convenience, eliminating concerns about library state.

- High-level replacement and substitution functions in OCaml are faster than
  those in the `Str` module. When compiled to native code, they can even
  outperform Perl's C-based functions.

- Returned data is unique, allowing safe destructive updates without side
  effects.

- The library interface uses labels and default arguments for enhanced
  programming comfort.

## Usage

Please run:

```sh
odig odoc pcre2
```

Or (maybe?):

```sh
dune build @doc
```

Functions support two flag types:

1. **Convenience flags**: Readable and concise, translated internally on each
   call. Example:

   ```ocaml
   let rex = Pcre2.regexp ~flags:[`ANCHORED; `CASELESS] "some pattern" in
   (* ... *)
   ```

   These are easy to use but may incur overhead in loops. For performance
   optimization, consider the next approach.

2. **Internal flags**: Predefined and translated from convenience flags for
   optimal loop performance. Example:

   ```ocaml
   let iflags = Pcre2.cflags [`ANCHORED; `CASELESS] in
   for i = 1 to 1000 do
     let rex = Pcre2.regexp ~iflags "some pattern constructed at runtime" in
     (* ... *)
   done
   ```

   Translating flags outside loops saves cycles. Avoid creating regex in loops:

   ```ocaml
   for i = 1 to 1000 do
     let chunks = Pcre2.split ~pat:"[ \t]+" "foo bar" in
     (* ... *)
   done
   ```

   Instead, predefine the regex:

   ```ocaml
   let rex = Pcre2.regexp "[ \t]+" in
   for i = 1 to 1000 do
     let chunks = Pcre2.split ~rex "foo bar" in
     (* ... *)
   done
   ```

Functions use optional arguments with intuitive defaults. For instance,
`Pcre2.split` defaults to whitespace as the pattern. The `examples` directory
contains applications demonstrating PCRE2-OCaml's functionality.

## Restartable (Partial) Pattern Matching

PCRE2 includes a DFA match function for restarting partial matches with new
input, exposed via `pcre2_dfa_exec`. While not suitable for extracting
submatches or splitting strings, it's useful for streaming and search tasks.

Example of a partial match restarted:

```ocaml
utop # open Pcre2;;
utop # let rex = regexp "12+3";;
val rex : regexp = <abstr>
utop # let workspace = Array.make 40 0;;
val workspace : int array =
  [| ... |]
utop # pcre2_dfa_match ~rex ~flags:[`PARTIAL_SOFT] ~workspace "12222";;
Exception: Pcre2.Error Partial.
utop # pcre2_dfa_match ~rex ~flags:[`PARTIAL_SOFT; `DFA_RESTART] ~workspace "2222222";;
Exception: Pcre2.Error Partial.
utop # pcre2_dfa_exec ~rex ~flags:[`PARTIAL_SOFT; `DFA_RESTART] ~workspace "2222222";;
Exception: Pcre2.Error Partial.
utop # pcre2_dfa_exec ~rex ~flags:[`PARTIAL_SOFT; `DFA_RESTART] ~workspace "223xxxx";;
- : int array = [|0; 3; 0|]
```

Refer to the `pcre2_dfa_exec` documentation and the `dfa_restart` example for
more information.

## Contact Information and Contributing

Submit bug reports, feature requests, and contributions via the
[GitHub issue tracker](https://github.com/camlp5/pcre2-ocaml/issues).

For the latest information, visit: <https://github.com/camlp5/pcre2-ocaml>
