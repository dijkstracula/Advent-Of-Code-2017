fun read ins =
case TextIO.scanStream (Int.scan StringCvt.DEC) ins of 
     NONE => nil
  |  SOME digit => digit :: (read ins)

fun tramp state =
let
  fun loop state p acc = 
    if p = Array.length state then acc else let 
      val v   = Array.sub(state, p) 
      val off = if v >= 3 then ~1 else 1
      in
        Array.update(state, p, v + off);
        loop state (p + v) (acc + 1)
      end
in
  loop state 0 0
end
   
val file = TextIO.openIn "day5_input.txt";
val mem = Array.fromList (read file);
tramp mem;
