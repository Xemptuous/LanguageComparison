type tape = { left : int list; cur : int; right : int list }

let zero_tape = { left = []; cur = 0; right = [] }

let move_right t =
  match t.right with
  | x :: rs -> { left = t.cur :: t.left; cur = x; right = rs }
  | [] -> { left = t.cur :: t.left; cur = 0; right = [] }

let move_left t =
  match t.left with
  | x :: ls -> { left = ls; cur = x; right = t.cur :: t.right }
  | [] -> { left = []; cur = 0; right = t.cur :: t.right }

let inc_cell t = { t with cur = (t.cur + 1) land 0xFF } (* wrap 0..255 *)
let dec_cell t = { t with cur = (t.cur - 1) land 0xFF }

type state = { ip : int; tape : tape }

let make_jump_table (prog : string) : (int, int) Hashtbl.t =
  let jump = Hashtbl.create (String.length prog) in
  let st = Stack.create () in
  String.iteri
    (fun i c ->
      match c with
      | '[' -> Stack.push i st
      | ']' -> (
          match Stack.pop_opt st with
          | Some j ->
              Hashtbl.add jump j i;
              Hashtbl.add jump i j
          | None -> failwith (Printf.sprintf "Unmatched ']' at %d" i))
      | _ -> ())
    prog;
  if not (Stack.is_empty st) then failwith "Unmatched '[' found";
  jump

let read_cell t = t.cur

let rec run (prog : string) (jump : (int, int) Hashtbl.t) (s : state) : state =
  if s.ip < 0 || s.ip >= String.length prog then s
  else
    match prog.[s.ip] with
    | '>' -> run prog jump { ip = s.ip + 1; tape = move_right s.tape }
    | '<' -> run prog jump { ip = s.ip + 1; tape = move_left s.tape }
    | '+' -> run prog jump { ip = s.ip + 1; tape = inc_cell s.tape }
    | '-' -> run prog jump { ip = s.ip + 1; tape = dec_cell s.tape }
    | '.' ->
        print_char (Char.chr (read_cell s.tape));
        flush stdout;
        run prog jump { s with ip = s.ip + 1 }
    | ',' -> run prog jump { ip = s.ip + 1; tape = { s.tape with cur = 0 } }
    | '[' ->
        if read_cell s.tape = 0 then
          let j = Hashtbl.find jump s.ip in
          run prog jump { s with ip = j + 1 }
        else run prog jump { s with ip = s.ip + 1 }
    | ']' ->
        if read_cell s.tape <> 0 then
          let j = Hashtbl.find jump s.ip in
          run prog jump { s with ip = j + 1 }
        else run prog jump { s with ip = s.ip + 1 }
    | _ -> run prog jump { s with ip = s.ip + 1 }

let () =
  let input =
    "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
  in
  let jumps = make_jump_table input in
  ignore (run input jumps { ip = 0; tape = zero_tape })
