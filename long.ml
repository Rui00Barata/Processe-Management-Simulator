(*Função de Escalonamento de Longo Prazo*)
open Lib

let unblock v n =
  for i = 0 to (n - 1) do 
    if v.(i) then let p = (Queue.pop blockedQ) in (Queue.push p readyQ; p.status <- 0)
    else Queue.push (Queue.pop blockedQ) blockedQ
  done

let long_sched n =
  if (Queue.is_empty blockedQ) then ()
  else
  let flag = ref true in
  while !flag do
    let x = Array.make n false in
    begin
      for i = 0 to (n - 1) do
        if ((Random.float 1.) > (1. -. (1. /. float_of_int n))) then  (* Chance de 1/n para desbloquear*)
          begin
            x.(i) <- true;                                        (*Processo pode-se desbloquear*)
            flag := false                                         (*É necessário desbloquear sempre no minimo 1 processo*)
          end
      done;
      unblock x n
    end
  done