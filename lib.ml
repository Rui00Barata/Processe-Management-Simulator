(*Biblioteca*)
(*Modulos adicionais*)
open Queue
open String
open Printf
open List

(*Funções auxiliares*)
let remove_CR str =
  let n = String.length str in
  if n > 0 && str.[n-1] = '\r' then
    String.sub str 0 (n-1)
  else
    str

(*MEMORY MODEL*)
type instruction = 
{
	ins : char;
	n : int;
	name : string
}
let memory = Array.make 1000 {ins = 'N'; n = 0; name = ""}
let next_memory_index = ref 0

(*PROCESSS CONTROL BLOCK*)
type pcb = 
{
	mutable name : string; 				(*Nome do programa*)
	mutable start : int;					(*Endereço da primeira instrução*)
	mutable variable : int;				(*Valor da variável*)
	mutable pid : int;						(*PID*)
	mutable ppid : int;						(*PPID*)
	mutable priority : int;			  (*Prioridade do programa*)
	mutable time : int;           (*Tempo de chegada*)
	mutable pc : int;							(*Program Counter*)
	mutable status : int					(*Estado do programa: ready(0), running(1), blocked(2), terminated(3)*)
}

(*GESTOR DE PROCESSOS*)
type newP = 
{
	name : string;
	time : int;
	priority: int;
}

let time = ref 0

let time_quantum = ref 10

let cpu = ref 0

let next_pid = ref 1

let pcb_table = ref [{name = "filename"; start = !next_memory_index; variable = 0; pid = !next_pid; ppid = 0; priority = 0; time = 0; pc = 0; status = 0}]

let newQ :newP Queue.t = Queue.create ()

let readyQ :pcb  Queue.t = Queue.create ()

let blockedQ :pcb  Queue.t = Queue.create ()

type running_state = 
{
	ind : int;						(*Índice do PCBTabela*)
	pid : int;						(*PID do processo em execução*)
	pc : int							(*Program counter do processo*)
}