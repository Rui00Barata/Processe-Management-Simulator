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