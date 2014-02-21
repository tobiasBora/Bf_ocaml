(* This is an interpreter for Brainfuck in Ocaml *)
(* Created by Tobias Bora *)
(* Licence GPLv3 *)


(* Array Type *)
(* Fixed int = Fixed size of <int> cells *)
type c_type = Int of int
type a_type = Fixed_Int of int array
(* r_mode is used to know if we read line by line or character by character during the user input *)
type r_mode = CharByChar | LineByLine
    
(* It will encode the board array, max_pos is the max already visited cell position *)
type board = {
  a:a_type;
  mutable pos:int;
  mutable max_pos:int;
  mutable list_char:char list;
  mutable read_mode:r_mode;
  mutable debug_mode:bool
}

(* Exec functions *)

let get_cell b i = match b.a with
    Fixed_Int a -> Int a.(i)
let set_cell_int b i v = match b.a with
    Fixed_Int a -> a.(i) <- v
let is_zero b i = match get_cell b i with
    Int n -> n = 0

(* Move right/left : > and < *)
let e_mr b = b.pos <- b.pos + 1; b.max_pos <- max b.max_pos (b.pos + 1)
let e_ml b = b.pos <- b.pos - 1

let e_incr b = match b.a with
    Fixed_Int a -> a.(b.pos) <- a.(b.pos) + 1

let e_decr b = match b.a with
    Fixed_Int a -> a.(b.pos) <- a.(b.pos) - 1

let e_print_pos b i =
  let cell = get_cell b i in
  match cell with
      Int c -> Printf.printf "%c%!" (char_of_int c)

let e_print b = e_print_pos b b.pos
    	
(* Print in decimal *)
let e_print_d_pos b i =
  let cell = get_cell b i in
  match cell with
      Int c -> Printf.printf "%d%!" c

let e_print_d b = e_print_d_pos b b.pos

let e_print_d_board b =
  Printf.printf "\n";
  for i = 0 to b.max_pos do
    Printf.printf " | %!";
    if b.pos = i then Printf.printf "*";
    e_print_d_pos b i;
  done;
  Printf.printf " |\n%!"

(* Read user input : 2 modes : char by char, line by line *)
(* Maybe the char by char method can cause some
   troubles on non Unix systems... *)
let get1char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false } in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res
    
(* A list is created with all letters which hasn't been still read *)
let list_of_string str =
  let rec list_of_string_aux str i =
      if i >= String.length str then []
      else (String.get str i)::(list_of_string_aux str (i+1))
  in
  list_of_string_aux str 0

let char_list_of_input b = match b.read_mode with
    CharByChar -> begin
      let c = get1char () in
      b.list_char @ [c]
    end
  | LineByLine ->
    begin
      let input = read_line () in
      b.list_char @ (list_of_string input)
    end
	
(* Exec only one instruction, return back the new position *)
(* exec_instr : board -> str -> int -> int *)
exception EndOfCode
let exec_inscr b code i =
  if i < 0 || i > String.length code then
    raise EndOfCode
  else
    match String.get code i with
	'<' -> e_ml b; i+1
      | '>' -> e_mr b; i+1
      | '+' -> e_incr b; i+1
      | '-' -> e_decr b; i+1
      | '[' ->
	if is_zero b b.pos then begin
	  let cur_i = ref (i + 1) in
	  let d = ref 1 in
	  while !cur_i < String.length code && !d > 0 do
	    let cur_char = String.get code !cur_i in
	    if cur_char = ']' then decr d
	    else if cur_char = '[' then incr d;
	    incr cur_i
	  done;
	  if !d = 0 then !cur_i
	  else failwith (Printf.sprintf "Syntax error : no closing ']' for pos %d%!" i)
	end
	else
	  i+1
      | ']' ->
	if is_zero b b.pos then
	  i + 1
	else begin
	  let cur_i = ref (i - 1) in
	  let d = ref (-1) in
	  while !cur_i >= 0 && !d < 0 do
	    let cur_char = String.get code !cur_i in
	    if cur_char = ']' then decr d
	    else if cur_char = '[' then incr d;
	    decr cur_i
	  done;
	  if !d = 0 then !cur_i + 1
	  else failwith (Printf.sprintf "Syntax error : no opening '[' for position %d%!" i)
	end
      | '.' -> e_print b; i+1
      | ',' -> begin
 	while (b.list_char) = [] do
	  b.list_char <- char_list_of_input b
	done;
	(match b.list_char with
	    [] -> failwith "Empty list, shouldn't happened..."
	  | c::r -> begin
	    b.list_char <- r;
	    set_cell_int b b.pos (int_of_char c);
	  end);
	i+1
      end
      | '#' when b.debug_mode -> begin
	(* Debug mode *)
	let next = String.get code (i+1) in
	if next = 's' then (* #s make a little pause *)
	  Unix.sleep 1
	else if next = 'p' then (* #p print the int value *)
	  e_print_d b
	else if next = 'f' then (* #f print the full board *)
	  e_print_d_board b
	else if next = 'e' then (* #f is like hitting enter *)
	  Printf.printf "\n%!"
	else
	  ();
	i+2;
      end
      | _ -> i+1

let exec_full_code b code =
  let i = ref 0 in
  while !i < String.length code do
    i := exec_inscr b code !i;
  done

let _ =
  (* Default values *)
  let message = "Welcome in Bf-Ocaml, a great Ocaml interpreter for Brainfuck !\n\
 Usage :\n\
 bf_ocaml filename.bf\n\
 or\n\
 bf_ocaml \"source code\"\n"
  and board_type = ref "fixed_int"
  and board_size = ref 30000
  and read_mode = ref "c"
  and file = ref ""
  and debug_mode = ref false
  in

  (* We get the arguments *)
  let tmp_arg = ref [] in
  let args =
    [
      ("-t",Arg.Set_string board_type, " -t <type> : specify the type of the array : fixed_int = int array of fixed size (default)");
      ("-s",Arg.Set_int board_size, " -s <n> specify the size of the array (Default : 30000)");
      ("-r", Arg.Set_string read_mode, " read input used mode, -r c : caractere by character (default), -r l : line by line");
      ("-d", Arg.Set debug_mode, " Debug mode");
      ("-h", Arg.Unit (fun x -> Printf.printf "%s" (Arg.usage_string (Arg.align !tmp_arg) message); exit 0), " Display this list of options")
    ] in
  tmp_arg := args;
  Arg.parse (Arg.align args) (fun s -> file := s) message;

  let r_mode =
    if !read_mode = "c" then CharByChar
    else if !read_mode = "l" then LineByLine
    else failwith "Unknow read mode" in
  let array =
    if !board_type = "fixed_int" then
      Fixed_Int (Array.make !board_size 0)
    else
      failwith "Unknow array type mode"
  in
  (* Check if the file argument is a file or a code *)
  let code = ref "" in
  if !file = "" then
    Printf.printf "%s" message
  else if Sys.file_exists !file then begin
    (* Read the file*)
    let chan = open_in !file in
    (try
       while true do
	 code := !code ^ (input_line chan)
       done
     with End_of_file ->
       close_in chan);
  end
  else
    code := !file;
  (* Create the board *)
  let b = {
      a = array;
      pos = 0;
      max_pos = 0;
      list_char = [];
      read_mode = r_mode;
      debug_mode = !debug_mode
    } in
  exec_full_code b !code
