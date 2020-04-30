<<<<<<< HEAD
(*Simulacao de um processo*)
(* Modulos *)
Open Lib

(*Variáveis*)
let memory = Array.make 1000 {ins = 'N'; n = 0; nome = ""}
let next_memory_index = ref 0

(*{nome = "progenitor"; start = "0"; variavel = ; pid = ; ppid = ; prioridade = ; pc = ; estado = 0;}*)

(*Funções*)
let tirar_CR str =
  let n = String.length str in
  if n > 0 && str.[n-1] = '\r' then
    String.sub str 0 (n-1)
  else
    str

let line_to_instr line =
  let x = String.split_on_char ' ' (tirar_CR line) in
  match x with
  |[] -> {ins = char_of_int 0; n = 0; nome = ""}
  |[hd] -> {ins = hd.[0]; n = 0; nome = ""}
  |h::t -> 
    if h.[0] <> 'L'  then {ins = h.[0];n = int_of_string (List.hd t); nome = ""}
    else if h.[0] = 'L' then {ins = h.[0];n = 0;nome = (List.hd t)}
    else {ins = 'E';n = 0;nome = ""}


let abrir filename =
  let process = {nome = filename; start = !next_memory_index; variavel = 0; pid = !next_pid; ppid = 0; prioridade = 0; pc = 0; estado = 0};
  let fi = open_in (filename^".prg") in
  let flag = ref true in
  let line = ref "" in
  let instr = ref {ins = 'N'; n = 0; nome = ""} in
  while (!flag) do
      try 
      begin 
        line := input_line fi;
        instr := line_to_instr !line;
        memory.(!next_memory_index) <- !instr;
        next_memory_index := !next_memory_index + 1;
      end
      with End_of_file -> begin line := "EOF"; close_in fi; flag := false; pcb_table := !pcb_table @ [process]; next_pid := !next_pid + 1 end
  done;
  
let read_instr instr process =
  match instr.ins with
  | 'M' -> process.variavel <- instr.n
  | 'A' -> process.variavel <- (process.variavel + instr.n) 
  | 'S' -> process.variavel <- (process.variavel - instr.n) 
  | 'B' -> process.estado <- 2
  | 'T' -> process.estado <- 3
  | 'C' -> let proc = process in
          begin
            process.pc <- (process.pc + instr.n);
            proc.pid <- !next_pid;
            proc.ppid <- process.pid; 
            next_pid := !next_pid + 1;
            pcb_table := !pcb_table @ [proc]
          end
  | 'L' -> begin
            (List.nth pcb_table (List.length pcb_table - 1)).nome <- instr.nome;
            abrir ((List.nth pcb_table (List.length pcb_table - 1)).nome)
          end
  | _ -> failwith "Intrução inválida\n"
=======
(*Simulacao de um processo*)
(*pen Lib*)
type pcb = 
  {
    nome : string; 				(*nome do processo*)
    start : int;					(*endereço da primeira instrução*)
    variavel : int;				(*valor da variável*)
    pid : int;						(*PID*)
    ppid : int;						(*PPID*)
    prioridade : int;			(*Prioridade do programa*)
    pc : int;							(*Program Counter*)
    estado : int					(*Estado do program: blocked,...*)
  }

let process = [|{nome = "progenitor"; start = 0; variavel = 0; pid = 0; ppid = 0; prioridade = 0; pc = 0; estado = 0}|]
let ins line =
  let x = String.split_on_char ' ' line in
  match x with
  |[] -> {ins = char_of_int 0;n = 0;nome = ""}
  |[hd] -> {ins = hd.[0];n = 0;nome = ""}
  |h::t -> 
    if h.[0] != 'L'  then {ins = h.[0];n = int_of_string (List.hd t);nome = ""}
    else {ins = h.[0];n = 0;nome = (List.hd t)}

let i = ref 0
let func l =
  match l with
  | [hd] -> match hd with
            | 'B' -> wait
            | 'T' -> 
            | _ ->
  | hd :: tl -> match hd with
                | 'M' -> process.(!i).variavel <- tl
                | 'A' -> process.(!i).variavel <- process.(!i).variavel + int_of_string tl
                | 'S' -> process.(!i).variavel <- process.(!i).variavel - int_of_string tl
                | 'C' -> let _ = fork; i:= !i + 1; process.(!i).pid <- getpid; process.(!i).ppid <- getppid
                | 'L' -> process.(!i).nome <- tl
                | _ -> ()
  | _ -> ()
>>>>>>> 60badbeb4be6deebd5ece4ffd13dbb81e649260b
