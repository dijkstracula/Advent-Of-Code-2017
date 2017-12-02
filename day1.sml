fun strToDigits s = map (fn c => (ord c) - 48) (explode s)

fun rotate l i = rev(rev(List.take(l,i)) @ rev(List.drop(l,i)))

fun tuples l i = ListPair.zipEq(l, rotate l i)
fun adjacents l = tuples l 1
fun antipodes l = tuples l (length l div 2)

fun ex1 s = 
  foldl (fn ((a,b), acc) => if a = b then a + acc else acc) 
  0
  (antipodes (strToDigits s))
