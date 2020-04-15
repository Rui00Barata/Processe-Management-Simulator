(*Biblioteca*)
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
	nome : string; 				(*nome do programa*)
	start : int;					(*endereço da primeira instrução*)
	variavel : int;				(*valor da variável*)
	pid : int;						(*PID*)
	ppid : int;						(*PPID*)
	prioridade : int;			(*Prioridade do programa*)
	pc : int;							(*Program Counter*)
	estado : int					(*Estado do program: ready(0), running(1), blocked(2), terminated(3)*)
}

(*GESTOR DE PROCESSOS*)
let tempo = ref 0

let cpu = ref 0

let next_pid = ref 1

let process_list = ref []

let prontos = Queue.create

let blocked = Queue.create

type running_state = 
{
	ind : int;						(*Índice do PCBTabela*)
	pid : int;						(*PID do processo em execução*)
	pc : int							(*Program counter do processo*)
}
	
