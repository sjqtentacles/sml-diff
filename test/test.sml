(* Dependency-free test runner for the Diff structure.
 * Prints one line per assertion and exits non-zero if any assertion fails. *)

structure D = Diff

val passed = ref 0
val failed = ref 0

fun check (name : string) (cond : bool) : unit =
    if cond
    then (passed := !passed + 1; print ("ok   - " ^ name ^ "\n"))
    else (failed := !failed + 1; print ("FAIL - " ^ name ^ "\n"))

(* charvec from a string *)
fun cv s = Vector.fromList (explode s)

(* render an edit list of chars compactly, e.g. " a-b+c" *)
fun showCharEdits es =
    String.concat
      (map (fn D.Keep c => " " ^ str c
             | D.Delete c => "-" ^ str c
             | D.Insert c => "+" ^ str c) es)

fun charDiff a b = D.diff (op = : char * char -> bool) (cv a) (cv b)

(* The fundamental property: applyOld reconstructs a, applyNew reconstructs b. *)
fun reconstructs a b =
    let val es = charDiff a b
    in implode (D.applyOld es) = a andalso implode (D.applyNew es) = b end

fun run () =
  let
    (* Degenerate cases *)
    val () = check "empty vs empty is []" (null (charDiff "" ""))
    val () = check "empty vs abc is 3 inserts"
                   (showCharEdits (charDiff "" "abc") = "+a+b+c")
    val () = check "abc vs empty is 3 deletes"
                   (showCharEdits (charDiff "abc" "") = "-a-b-c")
    val () = check "identical is all keeps"
                   (showCharEdits (charDiff "abc" "abc") = " a b c")

    (* Simple single edits *)
    val () = check "insert in middle"
                   (showCharEdits (charDiff "ac" "abc") = " a+b c")
    val () = check "delete in middle"
                   (showCharEdits (charDiff "abc" "ac") = " a-b c")
    val () = check "replace = delete+insert"
                   (reconstructs "abc" "axc")

    (* The canonical Myers (1986) example: ABCABBA -> CBABAC.
       The shortest edit script has length 5 (edit distance 5) and the LCS has
       length 4 (one valid LCS is "BABA"; "CABA"/"BCBA" can also arise depending
       on tie-breaking, all length 4). *)
    val () = check "Myers example edit distance = 5"
                   (D.editDistance (op =) (cv "ABCABBA") (cv "CBABAC") = 5)
    val () = check "Myers example LCS length = 4"
                   (length (D.lcs (op =) (cv "ABCABBA") (cv "CBABAC")) = 4)
    val () = check "Myers example reconstructs both sides"
                   (reconstructs "ABCABBA" "CBABAC")

    (* edit distance sanity *)
    val () = check "editDistance identical = 0"
                   (D.editDistance (op =) (cv "hello") (cv "hello") = 0)
    val () = check "editDistance kitten/sitting = 5"
                   (D.editDistance (op =) (cv "kitten") (cv "sitting") = 5)
    val () = check "editDistance is |a|+|b| - 2*|lcs|"
                   (let val a = cv "ABCABBA" and b = cv "CBABAC"
                        val l = length (D.lcs (op =) a b)
                    in D.editDistance (op =) a b
                       = Vector.length a + Vector.length b - 2 * l
                    end)

    (* LCS correctness on a known pair *)
    val () = check "lcs of XMJYAUZ / MZJAWXU = MJAU"
                   (implode (D.lcs (op =) (cv "XMJYAUZ") (cv "MZJAWXU")) = "MJAU")

    (* diffList wrapper over ints *)
    val () = check "diffList ints reconstructs"
                   (let val es = D.diffList (op = : int*int->bool)
                                            [1,2,3,4] [1,3,4,5]
                    in D.applyOld es = [1,2,3,4] andalso D.applyNew es = [1,3,4,5]
                    end)

    (* Line helpers *)
    val () = check "splitLines drops trailing newline"
                   (D.splitLines "a\nb\n" = ["a", "b"])
    val () = check "splitLines no trailing newline"
                   (D.splitLines "a\nb" = ["a", "b"])
    val () = check "splitLines single line" (D.splitLines "hello" = ["hello"])

    val les = D.diffLines "line1\nline2\nline3\n" "line1\nlineX\nline3\n"
    val () = check "diffLines keeps unchanged, swaps middle"
                   (D.applyOld les = ["line1", "line2", "line3"]
                    andalso D.applyNew les = ["line1", "lineX", "line3"])
    val () = check "formatUnified renders prefixes"
                   (D.formatUnified les =
                    " line1\n-line2\n+lineX\n line3")

    (* A larger randomized-style consistency check: many pairs all reconstruct *)
    val pairs = [("", "x"), ("x", ""), ("abcdef", "abcdef"),
                 ("abcdef", "abXdef"), ("the quick brown", "the slow brown"),
                 ("aaaa", "aaa"), ("aaa", "aaaa"), ("abcabc", "bcabca"),
                 ("1234567890", "1234509876")]
    val allRecon = List.all (fn (a, b) => reconstructs a b) pairs
    val () = check "all sample pairs reconstruct both sides" allRecon

    (* applyEdits: apply a computed patch and round-trip back to the target. *)
    val di = D.diffList (op = : int * int -> bool)
    val ds = D.diffList (op = : string * string -> bool)

    (* Round-trip property: applyEditsList (a, diff a b) = SOME b. *)
    val () = check "applyEdits round-trip [1,2,3]->[1,3,4]"
                   (D.applyEditsList (op =) [1,2,3] (di [1,2,3] [1,3,4])
                    = SOME [1,3,4])
    val () = check "applyEdits round-trip []->[1,2]"
                   (D.applyEditsList (op =) [] (di [] [1,2]) = SOME [1,2])
    val () = check "applyEdits round-trip [1,2,3]->[]"
                   (D.applyEditsList (op =) [1,2,3] (di [1,2,3] []) = SOME [])
    val () = check "applyEdits round-trip identical [1,2,3]"
                   (D.applyEditsList (op =) [1,2,3] (di [1,2,3] [1,2,3])
                    = SOME [1,2,3])
    val () = check "applyEdits round-trip strings"
                   (D.applyEditsList (op =) ["x","y","z"]
                                     (ds ["x","y","z"] ["x","w","z","q"])
                    = SOME ["x","w","z","q"])

    (* The vector form agrees with diff over vectors. *)
    val () = check "applyEdits vector round-trip"
                   (case D.applyEdits (op =) (cv "abc") (charDiff "abc" "axc") of
                        SOME v => implode (Vector.foldr (op ::) [] v) = "axc"
                      | NONE => false)

    (* Inconsistency -> NONE: a Keep that doesn't match the original. *)
    val () = check "applyEdits Keep mismatch -> NONE"
                   (D.applyEditsList (op = : int*int->bool)
                                     [1,2,3] [D.Keep 9] = NONE)
    (* A Delete that doesn't match the original at that position. *)
    val () = check "applyEdits Delete mismatch -> NONE"
                   (D.applyEditsList (op = : int*int->bool)
                                     [1,2,3]
                                     [D.Delete 1, D.Delete 9, D.Keep 3] = NONE)
    (* Script keeps/deletes more elements than the original has. *)
    val () = check "applyEdits past end -> NONE"
                   (D.applyEditsList (op = : int*int->bool)
                                     [1] [D.Keep 1, D.Delete 2] = NONE)
    (* Script does not consume the original in full. *)
    val () = check "applyEdits underrun -> NONE"
                   (D.applyEditsList (op = : int*int->bool)
                                     [1,2,3] [D.Keep 1] = NONE)
  in
    print ("\n" ^ Int.toString (!passed) ^ " passed, "
           ^ Int.toString (!failed) ^ " failed\n");
    OS.Process.exit (if !failed = 0 then OS.Process.success else OS.Process.failure)
  end

val () = run ()
