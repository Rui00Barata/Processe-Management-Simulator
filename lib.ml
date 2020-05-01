(*Biblioteca*)
(*Modulos adicionais*)
open Queue
open String
open Printf
open List

(*MEMORY MODEL*)
type instruction = 
{
	ins : char;
	n : int;
	nome : string
}
let memory = Array.make 1000 {ins = 'N'; n = 0; nome = ""}
let next_memory_index = ref 0

(*PROCESSS CONTROL BLOCK*)
type pcb = 
{
	mutable nome : string; 				(*nome do programa*)
	mutable start : int;					(*endereço da primeira instrução*)
	mutable variavel : int;				(*valor da variável*)
	mutable pid : int;						(*PID*)
	mutable ppid : int;						(*PPID*)
	mutable prioridade : int;			(*Prioridade do programa*)
	mutable pc : int;							(*Program Counter*)
	mutable estado : int					(*Estado do program: ready(0), running(1), blocked(2), terminated(3)*)
}

(*GESTOR DE PROCESSOS*)
let tempo = ref 0

let cpu = ref 0

let next_pid = ref 1

let pcb_table = ref [{nome = "filename"; start = !next_memory_index; variavel = 0; pid = !next_pid; ppid = 0; prioridade = 0; pc = 0; estado = 0}]

let prontos = Queue.create

let blocked = Queue.create

type running_state = 
{
	ind : int;						(*Índice do PCBTabela*)
	pid : int;						(*PID do processo em execução*)
	pc : int							(*Program counter do processo*)
}