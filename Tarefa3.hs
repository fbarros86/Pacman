{-| 
Module : Tarefa3
Description : Tarefa3 - Trabalho de LI1 2020/2021

= Introdução

Nesta tarefa criámos um mecanismo que, dado um labirinto válido,
converte-o numa sequência de instruções (cada uma representando um corredor).
Estas, quando executadas, produzem o mesmo labirinto,
de modo a recriá-lo num formato mais compacto para leitura.

> compactMaze :: Maze -> Instructions

= Objetivo

Estas instruções permitem processar dois tipos de padrões: padrões verticais e padrões horizontais.

Para os padrões horizontais são utilizadas listas de pares cujo primeiro valor é um inteiro e o segundo uma peça.
O inteiro vai representar o número de vezes que a peça se repete consecutivamente naquele corredor.

Para os padrões verticais, é utilizada a instrução Repeat x, tal que x representa o
indice do elemento das instruções que se deve repetir nesse lugar.

Decidimos começar por encontrar os padrões horizontais do labirinto, juntando todos as peças
iguais que aparecem seguidas. A partir do resultado desta compressão, procuramos os padrões
verticais, comparando todos os corredores.

= Discussão e conclusão

Testamos esta função para vários labirintos e verificámos que a sequência de instruções obtida
permitia sempre produzir o labirinto original, além disso tinha em atenção os dois tipos de padrões referidos.

-}
module Tarefa3 where

import Types


compactMaze :: Maze -> Instructions
compactMaze m = compararCorredores (map compactCorridor m) 0


-- | Transforma um corredor numa instrução, tendo em conta a existência de padrões
-- horizontais, que foram explicados anteriormente
--
compactCorridor :: Corridor -> Instruction
compactCorridor [] = Instruct []
compactCorridor [a] = Instruct [(1,a)]
compactCorridor (x:y:z) | (x==y) = Instruct ((1+a, x) : t)
                        | otherwise = Instruct ((1, x) : (a,b) : t)
           where Instruct ((a,b) : t) = compactCorridor (y:z)

-- | Procura padrões verticais, numa lista de instruções e, caso as encontre, substitui
-- por Repeat x, como foi explicado anteriormente
--

compararCorredores :: Instructions -> Int -> Instructions
compararCorredores [] x = []
compararCorredores (h:t) i = h : compararCorredores y (i+1)
                         where y = compararCorredor h t i

-- | Verifica se existem algumas instruções iguais à h numa lista de instruções. Caso tal se verifique, substitui
-- as instruções repetidas por Repeat i
--

compararCorredor :: Instruction -> Instructions -> Int -> Instructions
compararCorredor h [] i = []
compararCorredor h (x:xs) i = if eIgual h x then Repeat i: compararCorredor h xs i
                           else x: compararCorredor h xs i

-- | Compara instruções 
--
eIgual :: Instruction -> Instruction -> Bool  
eIgual (Instruct i1) (Instruct i2) = i1==i2
eIgual _ _ = False                                      