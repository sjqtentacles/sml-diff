(* sml-diff demo: diffs two versions of a small source file with
   Diff.diffLines, then renders the unified result (green insert / red delete /
   gray context) to assets/diff.png using the bitmap-font renderer. *)

fun rgb (r, g, b) = (r, g, b)

val oldSrc =
  String.concatWith "\n"
    [ "fun area r ="
    , "  let"
    , "    val w = width r"
    , "    val h = height r"
    , "  in"
    , "    w * h"
    , "  end" ]

val newSrc =
  String.concatWith "\n"
    [ "fun area (r : rect) ="
    , "  let"
    , "    val w = width r"
    , "    val h = height r"
    , "    val a = w * h"
    , "  in"
    , "    a"
    , "  end" ]

val edits = Diff.diffLines oldSrc newSrc

val scale = 2
val lineH = 11 * scale
val left = 16
val top = 58
val width = 540
val height = top + length edits * lineH + 18

val bg     = rgb (23, 26, 33)
val titleC = rgb (150, 200, 230)
val keepC  = rgb (165, 172, 184)
val insC   = rgb (120, 222, 140)
val delC   = rgb (236, 120, 132)
val insBg  = rgb (28, 52, 38)
val delBg  = rgb (56, 30, 36)

val c = Canvas.make (width, height) bg

(* title + a divider line *)
val () = ignore (Font.drawText c (left, 18) scale titleC
                   "diff: rect.sml   - before / + after")
val () = Canvas.fillRect c (0, 48, width, 2) (rgb (44, 50, 62))

val () =
  let
    fun row (i, e) =
      let
        val y = top + i * lineH
        val (prefix, fg, bgc) =
          case e of
              Diff.Keep _   => (" ", keepC, NONE)
            | Diff.Insert _ => ("+", insC, SOME insBg)
            | Diff.Delete _ => ("-", delC, SOME delBg)
        val text = case e of Diff.Keep s => s | Diff.Insert s => s | Diff.Delete s => s
        val () = case bgc of SOME col => Canvas.fillRect c (0, y - 2, width, lineH) col | NONE => ()
        val () = ignore (Font.drawText c (left, y) scale fg prefix)
        val () = ignore (Font.drawText c (left + 7 * scale, y) scale fg text)
      in
        ()
      end
    fun loop (i, es) =
      case es of [] => () | e :: rest => (row (i, e); loop (i + 1, rest))
  in
    loop (0, edits)
  end

val () =
  let
    val os = BinIO.openOut "assets/diff.png"
  in
    BinIO.output (os, Image.encodePng (Canvas.toImage c));
    BinIO.closeOut os;
    print "wrote assets/diff.png\n"
  end
