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
	nome : string; 				(*nome da primeira instrução*)
	start : int;					(*endereço da primeira instrução*)
	variavel : int;				(*valor da variável*)
	pid : int;						(*PID*)
	ppid : int;						(*PPID*)
	prioridade : int;			(*Prioridade do programa*)
	pc : int;							(*Program Counter*)
	estado : int					(*Estaado do program: blocked,...*)
}

(*GESTO DE PROCESSOS*)
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
	
