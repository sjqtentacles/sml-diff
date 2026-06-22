(* diff.sml

   Implementation of the Myers O(ND) diff algorithm.

   Forward pass: explore edit paths of increasing length D. For each D we keep,
   in the array V indexed by diagonal k = x - y, the furthest-reaching x on
   that diagonal. We snapshot V after each D; once a path reaches (n, m) we
   walk the snapshots backwards to recover the actual edit script. This is the
   classic greedy-LCS-with-backtrack formulation from Myers (1986). *)

structure Diff :> DIFF =
struct
  datatype 'a edit = Keep of 'a | Insert of 'a | Delete of 'a

  fun diff (eq : 'a * 'a -> bool) (a : 'a vector) (b : 'a vector) : 'a edit list =
      let
        val n = Vector.length a
        val m = Vector.length b
      in
        if n = 0 andalso m = 0 then []
        else
        let
          val max = n + m
          val off = max                 (* k + off keeps the index in range *)
          val size = 2 * max + 1

          fun ax i = Vector.sub (a, i)
          fun bx j = Vector.sub (b, j)

          val v = Array.array (size, 0)
          fun get k = Array.sub (v, k + off)
          fun set (k, x) = Array.update (v, k + off, x)

          (* Decide, given the V in scope (current or a snapshot), whether the
             move into diagonal k at edit count d came from a downward (insert)
             step. Used identically in the forward and backward passes so they
             stay consistent. *)
          fun cameFromDown (getf, k, d) =
              k = ~d orelse (k <> d andalso getf (k - 1) < getf (k + 1))

          (* Forward search; returns (dFinal, snapshots) with snapshots most
             recent first (snapshots.[0] is V as it was *before* extending to
             dFinal). *)
          fun search () =
              let
                fun go (d, trace) =
                    if d > max then raise Fail "myers: exceeded max edit length"
                    else
                      let
                        val snap = Array.tabulate (size, fn i => Array.sub (v, i))
                        val trace = snap :: trace
                        fun loopK k =
                            if k > d then NONE
                            else
                              let
                                val down = cameFromDown (get, k, d)
                                val xStart = if down then get (k + 1)
                                             else get (k - 1) + 1
                                val yStart = xStart - k
                                fun snake (x, y) =
                                    if x < n andalso y < m andalso eq (ax x, bx y)
                                    then snake (x + 1, y + 1)
                                    else (x, y)
                                val (x, y) = snake (xStart, yStart)
                                val () = set (k, x)
                              in
                                if x >= n andalso y >= m then SOME (d, trace)
                                else loopK (k + 2)
                              end
                      in
                        case loopK (~d) of
                            SOME res => res
                          | NONE => go (d + 1, trace)
                      end
              in
                go (0, [])
              end

          val (dFinal, traceList) = search ()
          (* snaps.[d] is V as it stood before extending to edit length d
             (i.e. the furthest-reaching values produced at step d-1). *)
          val snaps = Vector.fromList (List.rev traceList)

          (* Backtrack from (n,m) to (0,0), emitting the script in order. *)
          fun backtrack () =
              let
                fun loop (d, x, y, acc) =
                    let
                      val vd = Vector.sub (snaps, d)
                      fun getp k = Array.sub (vd, k + off)
                      val k = x - y
                      val (prevK, isDown) =
                          if d = 0 then (k, false)
                          else if cameFromDown (getp, k, d) then (k + 1, true)
                          else (k - 1, false)
                      val prevX = if d = 0 then 0 else getp prevK
                      val prevY = prevX - prevK
                      (* follow the snake (diagonal Keeps) down to (prevX,prevY)
                         plus the single non-diagonal move *)
                      fun snake (x, y, acc) =
                          if x > prevX andalso y > prevY
                          then snake (x - 1, y - 1, Keep (ax (x - 1)) :: acc)
                          else (x, y, acc)
                      val (x1, y1, acc) = snake (x, y, acc)
                    in
                      if d = 0 then acc
                      else
                        let
                          val acc =
                              if isDown then Insert (bx prevY) :: acc
                              else Delete (ax prevX) :: acc
                        in
                          loop (d - 1, prevX, prevY, acc)
                        end
                    end
              in
                loop (dFinal, n, m, [])
              end
        in
          backtrack ()
        end
      end

  fun fromList xs = Vector.fromList xs

  fun diffList eq a b = diff eq (fromList a) (fromList b)

  fun lcs eq a b =
      List.mapPartial (fn Keep x => SOME x | _ => NONE) (diff eq a b)

  fun editDistance eq a b =
      List.foldl (fn (e, acc) => case e of Keep _ => acc | _ => acc + 1)
                 0 (diff eq a b)

  fun applyOld es =
      List.mapPartial (fn Keep x => SOME x | Delete x => SOME x | Insert _ => NONE) es

  fun applyNew es =
      List.mapPartial (fn Keep x => SOME x | Insert x => SOME x | Delete _ => NONE) es

  fun applyEdits (eq : 'a * 'a -> bool) (orig : 'a vector) (edits : 'a edit list)
      : 'a vector option =
      let
        val n = Vector.length orig
        (* i is the cursor into `orig`; acc accumulates the target in reverse. *)
        fun go ([], i, acc) =
              if i = n then SOME (Vector.fromList (List.rev acc)) else NONE
          | go (Keep x :: rest, i, acc) =
              if i < n andalso eq (Vector.sub (orig, i), x)
              then go (rest, i + 1, x :: acc)
              else NONE
          | go (Delete x :: rest, i, acc) =
              if i < n andalso eq (Vector.sub (orig, i), x)
              then go (rest, i + 1, acc)
              else NONE
          | go (Insert x :: rest, i, acc) =
              go (rest, i, x :: acc)
      in
        go (edits, 0, [])
      end

  fun applyEditsList eq xs edits =
      Option.map (fn v => Vector.foldr (op ::) [] v)
                 (applyEdits eq (fromList xs) edits)

  (* ---- Text helpers ---------------------------------------------------- *)

  fun splitLines s =
      let
        val parts = String.fields (fn c => c = #"\n") s
      in
        case List.rev parts of
            "" :: rest => List.rev rest
          | _ => parts
      end

  fun diffLines a b =
      diff (op =) (fromList (splitLines a)) (fromList (splitLines b))

  fun formatUnified es =
      let
        fun render (Keep s)   = " " ^ s
          | render (Delete s) = "-" ^ s
          | render (Insert s) = "+" ^ s
      in
        String.concatWith "\n" (map render es)
      end
end
