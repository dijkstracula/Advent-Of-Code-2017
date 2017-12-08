structure StringKey =
struct
  type ord_key = string
  val compare = String.compare
end
structure Set = RedBlackSetFn(StringKey)

(* pt 1 *)
fun id x = x

(* pt 2 *)
fun charsort str = let
  val chars = String.explode(str)
  val ordered = ListMergeSort.sort op< chars
in
  String.implode(ordered)
end

fun parse file f = case TextIO.inputLine file of
   NONE        => nil
 | SOME(line)  => let 
   val words = map f (String.tokens Char.isSpace line)
   val uniqs = Set.fromList words
  in
    (if (Set.numItems uniqs = length words) then 1 else 0) :: parse file f
  end

fun ex4 _ = 
let 
  val file = TextIO.openIn "day4_input.txt"
  val parsed = parse file charsort
  val _ = TextIO.closeIn file
in
  foldr (op+) 0 parsed
end
