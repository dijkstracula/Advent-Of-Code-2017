open Math

(* r is the range of values contained on a "spiral level".
 * L0: [1,1]
 * L1: [2,9]   = [1^2 + 1, 3^2]
 * L2: [10,25] = [3^2 + 1, 5^2]
 *)
fun range n = 
  if n = 0 then (1,1) else
    ( round(pow(2.0 * real(n) - 1.0, 2.0)) + 1,
      round(pow(2.0 * real(n) + 1.0, 2.0)) )

(* sideLen is the length of the spiral side that element n lies on.  If 
* n is within [(k - 2)^2 + 1, k^2 + 1], sideLen n equals k. *)
fun sideLen n = 
let val l = ceil(sqrt(real(n))) in
  (l div 2) * 2 + 1
end

(* level is the "spiral level" that element n lies on. *)
fun level n = ceil(sqrt(real(n))) div 2

fun spiral n = let 
  val (min, max) = range n 
  val flat = List.tabulate(max-min, fn x => x + min)
in
  flat
end

(* If we "rotate" whatever side `n` is on so it's on the right
* vertical side of the level, then the horizontal component of the 
* manhattan distance is just the level number and the vertical component
* is the offset from the "x-intercept". 
* *)
fun day3p1 n = let 
  val level = level n           (* what spiral level are we on *)
  val (min, max) = range level  (* extrema values for this level *)
  val m = (sideLen n) - 1       (* how long is a side of the level *)
  val zoff = level - 1          (* dist from min to the horiz axis *)
in
  level + abs(((n - min) mod m) - zoff)
end
