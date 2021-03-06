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

type running_state = 
{
	mutable ind : int;						(*Índice do PCBTabela*)
	mutable pid : int;						(*PID do processo em execução*)
	mutable pc : int							(*Program counter do processo*)
}

(*MEMORY MODEL*)
type instruction = 
{
	ins : char;
	n : int;
	name : string
}
let memory = Array.make 1000 {ins = 'N'; n = 0; name = ""}
let next_memory_index = ref 0
(* First_fit vars *)
let heap_f = ref (Array.make 128 (-1))
let time_list_f : int list ref = ref []
let success_list_f: int list ref = ref []
(* Next_fit vars *)
let heap_n = ref (Array.make 128 (-1))
let time_list_n : int list ref = ref []
let success_list_n: int list ref = ref []
(* Best_fit vars *)
let heap_b = ref (Array.make 128 (-1))
let time_list_b : int list ref = ref []
let success_list_b: int list ref = ref []
(* Worst_fit vars *)
let heap_w = ref (Array.make 128 (-1))
let time_list_w : int list ref = ref []
let success_list_w: int list ref = ref []

(*PROCESSS CONTROL BLOCK*)
type pcb = 
{
	mutable name : string; 				(*Nome do programa*)
	mutable start : int;					(*Endereço da primeira instrução*)
	mutable variable : int;				(*Valor da variável*)
	mutable pid : int;						(*PID*)
	mutable ppid : int;						(*PPID*)
	mutable priority : int;			  (*Prioridade do programa*)
	mutable arrival_time : int;   (*Tempo de chegada*)
	mutable burst_time : int;     (*Tempo necessário para executar*)
	mutable time : int;						(*Tempo de execução*)
	mutable pc : int;							(*Program Counter*)
	mutable status : int;					(*Estado do programa: ready(0), running(1), blocked(2), terminated(3)*)
	mutable finish : int					(*Tempo em que o processo termina*)
}

(*GESTOR DE PROCESSOS*)
type newP = 
{
	name : string;
	time : int;
	priority: int;
}

let clock_flag = ref true

let executing_flag = ref false

let rem_time = ref 0

let time_flag = ref false

let debug_mode = ref false

let rr_flag = ref false

let stop_time_flag = ref false 

let preempt_flag = ref false

let time = ref 0

let time_quantum = ref 10

let next_pid = ref 1

type pcb_list = pcb list ref

let pcb_table : pcb_list = ref []

let newQ :newP Queue.t = Queue.create ()

let readyQ :pcb Queue.t = Queue.create ()

let blockedQ :pcb Queue.t = Queue.create ()

let terminatedQ :pcb Queue.t = Queue.create ()

let running_proc = {ind = -1; pid = -1; pc = -1}