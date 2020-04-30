<<<<<<< HEAD
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

let pcb_table = ref []

let prontos = Queue.create

let blocked = Queue.create

type running_state = 
{
	ind : int;						(*Índice do PCBTabela*)
	pid : int;						(*PID do processo em execução*)
	pc : int							(*Program counter do processo*)
}
	
=======
(**Biblioteca*)
(*Modulos adicionais*)
open Queue
open String

(*MEMORY MODEL*)
type instruction = 
{
	ins : char;
	n : int;
	nome : string
}

(*PROCESSS CONTROL BLOCK*)
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

(*GESTOR DE PROCESSOS*)
let tempo = ref 0

let cpu = ref 0

let pcbtabela = Queue.create

let prontos = Queue.create

let blocked = Queue.create

type running_state = 
{
	ind : int;						(*indice do PCBTabela*)
	pid : int;						(*PID do processo em execução*)
	pc : int							(*Program counter do processo*)
}
>>>>>>> 60badbeb4be6deebd5ece4ffd13dbb81e649260b
