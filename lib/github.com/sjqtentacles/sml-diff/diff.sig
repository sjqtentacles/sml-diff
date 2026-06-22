(* diff.sig

   Sequence diffing via the Myers O(ND) shortest-edit-script algorithm.

   The core operates on two `'a vector`s given an equality predicate, producing
   an edit script: a list describing, in order, how to turn the first sequence
   into the second. The script uses three constructors:

     Keep x      - x appears in both sequences (a common element)
     Delete x    - x is present in the first sequence but not the second
     Insert x    - x is present in the second sequence but not the first

   Reading only the `Keep`s gives the longest common subsequence; reading
   `Keep`/`Delete` reconstructs the first sequence and `Keep`/`Insert` the
   second.

   Line-oriented helpers (`diffLines`, `formatUnified`) build on the core to
   diff and render text. *)

signature DIFF =
sig
  datatype 'a edit = Keep of 'a | Insert of 'a | Delete of 'a

  (* `diff eq a b` returns the shortest edit script transforming `a` into `b`,
     using `eq` to compare elements. Runs in O(ND) time where N = |a|+|b| and
     D is the size of the edit script. *)
  val diff : ('a * 'a -> bool) -> 'a vector -> 'a vector -> 'a edit list

  (* Convenience wrappers over lists. *)
  val diffList : ('a * 'a -> bool) -> 'a list -> 'a list -> 'a edit list

  (* The longest common subsequence (the `Keep` elements, in order). *)
  val lcs : ('a * 'a -> bool) -> 'a vector -> 'a vector -> 'a list

  (* The Levenshtein-style edit distance (number of Insert + Delete). *)
  val editDistance : ('a * 'a -> bool) -> 'a vector -> 'a vector -> int

  (* Project an edit script back onto each side. *)
  val applyOld : 'a edit list -> 'a list   (* Keep + Delete *)
  val applyNew : 'a edit list -> 'a list   (* Keep + Insert *)

  (* Apply an edit script as a patch: given the original sequence and a script
     (e.g. the output of `diff`/`diffList`), reconstruct the target sequence.
     The `eq` predicate matches the threading used by `diff`; the original comes
     before the script so `applyEdits eq a (diff eq a b)` reads left to right.

     Walking the script with a cursor into the original, each `Keep x`/`Delete x`
     must align with (be `eq` to) the original element under the cursor and then
     advances it; `Insert x` emits `x` without advancing. The result is the
     reconstructed target (the `Keep`/`Insert` elements, i.e. `applyNew`), but
     only when the script is consistent with the original.

     Returns `NONE` (rather than raising) when the script is inconsistent: a
     `Keep`/`Delete` element does not match the original at that position, the
     script consumes past the end of the original, or it does not consume the
     original in full. Otherwise `SOME` the target. *)
  val applyEdits     : ('a * 'a -> bool) -> 'a vector -> 'a edit list -> 'a vector option
  val applyEditsList : ('a * 'a -> bool) -> 'a list   -> 'a edit list -> 'a list option

  (* ---- Line-oriented text helpers ------------------------------------- *)

  (* Split into lines on "\n". A trailing newline does not produce a final
     empty line, so "a\nb\n" and "a\nb" both split to ["a","b"]. *)
  val splitLines : string -> string list

  (* Diff two texts line by line. *)
  val diffLines : string -> string -> string edit list

  (* Render a line-edit script in a simple unified style: each line prefixed
     with " " (keep), "-" (delete), or "+" (insert), joined by newlines. *)
  val formatUnified : string edit list -> string
end
