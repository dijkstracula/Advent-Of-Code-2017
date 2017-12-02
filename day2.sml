fun maximaDiff nil = raise Empty 
  | maximaDiff (h::t) = 
    let val (max, min) = 
      foldl (fn (x, (max, min)) => (Int.max(x, max), Int.min(x, min))) (h,h) t
    in
      max - min
    end

fun divisiblePair l =
    let 
      fun xProd l = foldr (fn (a, acc) => map (fn b => (a,b)) l @ acc) [] l
      fun f (a,b) = a <> b andalso (a mod b = 0)
      val (n, d) = (hd o List.filter f o xProd) l
    in
      n div d
    end

fun parse ins = 
let
  fun loop ins = case TextIO.inputLine ins of
      NONE      => nil
    | SOME line => 
        let val strings = String.tokens Char.isSpace line in
          map (fn s => Option.valOf (Int.fromString s)) strings :: loop ins
        end
in
  loop ins
end

fun ex2 _ = 
let 
  val file = TextIO.openIn "day2_input.txt"
  val parsed = parse file
  val _ = TextIO.closeIn file
in
  foldl (op+) 0 (map divisiblePair parsed)
end
