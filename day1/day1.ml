let file = "day1.txt"
let top3 (m1,m2,m3) n = 
  if n > m1 then n,m1,m2
  else if n > m2 then m1,n,m2
  else if n > m3 then m1,m2,n
  else m1,m2,m3
let () =
  let ic = open_in file in
  let rec get s m = try match  input_line ic with
      | "\n" |"" -> get 0 (top3 m s)
      | a ->get (s + int_of_string a) m
          with End_of_file -> let a,b,c = top3 m s in a + b+ c
  in print_int (get 0 (0,0,0))
