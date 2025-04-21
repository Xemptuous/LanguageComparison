type point = { x: int; y: int }

let greet name = Printf.printf "Hi, %s!\n" name
let square n = n * n

let () =
  let name = "Alice" in
  let x = 5 in
  let y = 3.14 in
  let active = true in

  greet name;
  Printf.printf "Square of %d is %d\n" x (square x);

  let p = { x = 3; y = 4 } in
  Printf.printf "(%d, %d)\n" p.x p.y;

  let nums = [1; 2; 3] in
  List.iter (fun n -> Printf.printf "%d " n) nums;
  print_newline ();

  let ages = [("Alice", 30); ("Bob", 25)] in
  let age = List.assoc "Bob" ages in
  Printf.printf "Bob is %d years old.\n" age;

  let rec loop count =
    if count < 3 then (
      Printf.printf "While loop: %d\n" count;
      loop (count + 1)
    )
  in
  loop 0

