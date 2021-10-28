{-|
Module : Testes
Description : Testing - Trabalho de LI1 2020/2021

O objetivo deste módulo é testar o funcionamento das funções definidas nas Tarefas 1, 2 e 3, através da escolha
de inputs especificos para testar os diferentes tipos de situações possíveis
-}

module Testes where

import Types
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Tarefa5
import Tarefa6
import FileUtilis
import Data.List


-- *Tarefa 1

-- |Foram escolhidos 4 inputs diferentes que representam labirintos com altura par ou impar e largura par ou impar
--
testesT1:: [(Int, Int,Int)]
testesT1 = [(15,10,1238), (16,10,1237),(16,11,1235),(15,11,123)]


-- | Retorna um lista de labirintos gerados através dos inputs dados
--
testesResT1 :: [(Int,Int,Int)] -> [Maze]
testesResT1 = map (\(x,y,z) -> generateMaze x y z) 

{- | Verifica se todos os labirintos dados cumprem os requisitos necessários para poderem ser considerados válidos, ou seja,
verifa se, para cada labirinto:

- este é rodeado por peças Wall (exceto túnel)

- a faixa mais central forma um túnel

- todos os corredores têm o mesmo comprimento

- existe uma estrutura denominada “casa dos fantasmas”, localizada no centro do labirinto, a qual
respeita uma determinada estrutura

-}
verificaLabirintos :: [Maze] -> Bool
verificaLabirintos = all verificaLabirinto

-- | Verifica se um labirinto cumprem os requisitos referidos anteriormente
--
verificaLabirinto :: Maze -> Bool
verificaLabirinto m@(c:cs) = (temParedeseTunel m (linhas m) 1) && (corredoresIguais (length c) cs )

-- | Verifica se um labirinto é rodeado por paredes (exceto túnel) e verifica se a faixa central forma um túnel
--
temParedeseTunel :: Maze -> Int -> Int -> Bool
temParedeseTunel (c:cs) l ac | ac==l || ac==1 = foldr (\x r -> (x==Wall) && r) True c
                             | ac== (div (l+1) 2) =  ((head c) == Empty)&& ((last c)==Empty) && (temParedeseTunel cs l (ac+1))
                             | otherwise = ((head c) == Wall)&& ((last c)== Wall) && (temParedeseTunel cs l (ac+1))

-- | Verifica se todos corredores têm o mesmo comprimento
--
corredoresIguais :: Int -> Maze  -> Bool
corredoresIguais i = foldr (\x r ->(length x ==i)&&r) True 

-- | Verifica se existe a "casa dos fantasmas" e que esta cumpre a estrutura correta, ou seja:
--
-- - A casa dos fantasmas tem uma altura  fixa igual a 3
--
-- - Caso o número de colunas do labirinto seja ímpar, o comprimento da casa é 9
--
-- - Caso o número de colunas do labirinto seja par, o comprimento da casa é 8
--
-- - A casa está centralizada, execto nos casos em que não é possível (quando o número de linhas é par).
-- Nestes casos a casa é colocada a X peças de distância da parede inferior e a X-1 peças de distância
-- da parede superior
--
-- - As peças que rodeiam a casa de fantasmas são Empty
--
temCasaFantasmas :: Maze -> Int -> Int -> Bool
temCasaFantasmas (x:xs) l c | l>1 = temCasaFantasmas xs (l-1) c
                            | (mod c 2) == 0 = temCasaFPares (x:xs) c 1 0
                            | otherwise = temCasaFImpares (x:xs) c 1 0

-- | Verifica se existe a "casa dos fantasmas" e que esta cumpre a estrutura descrita anteriormente quando o número colunas
-- do labirinto é par, sabendo que o labirinto dado já se encontra na linha anterior àquela que deve conter o início
-- da casa
--
temCasaFPares :: Maze -> Int -> Int-> Int -> Bool
temCasaFPares ((x:xs):t) c ac1 ac2 | ac2==5 = True
                                   | ac1<c = temCasaFPares (xs:t) c (ac1+1) ac2
                                   | ac2==0 = isPrefixOf empty xs && temCasaFPares t c 1 1
                                   | ac2==1 = isPrefixOf entr xs && temCasaFPares t c 1 2
                                   | ac2==2 = isPrefixOf parede xs && temCasaFPares t c 1 3
                                   | ac2==3 = isPrefixOf paredes xs && temCasaFPares t c 1 4
                                   | ac2==4 = isPrefixOf empty xs && temCasaFPares t c ac1 5
                                   where empty = replicate 10 Empty
                                         entr = [Empty, Wall, Wall, Wall, Empty, Empty, Wall, Wall, Wall, Empty]
                                         parede = [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty]
                                         paredes = [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Empty]

-- | Verifica se existe a "casa dos fantasmas" e que esta cumpre a estrutura descrita anteriormente quando o número colunas
-- do labirinto é ímpar, sabendo que o labirinto dado já se encontra na linha anterior àquela que deve conter o início
-- da casa
--
temCasaFImpares :: Maze -> Int -> Int-> Int -> Bool
temCasaFImpares ((x:xs):t) c ac1 ac2 | ac2==5 = True
                                     | ac1<c = temCasaFImpares (xs:t) c (ac1+1) ac2
                                     | ac2==0 = isPrefixOf empty xs && temCasaFImpares t c 1 1
                                     | ac2==1 = isPrefixOf entr xs && temCasaFImpares t c 1 2
                                     | ac2==2 = isPrefixOf parede xs && temCasaFImpares t c 1 3
                                     | ac2==3 = isPrefixOf paredes xs && temCasaFImpares t c 1 4
                                     | ac2==4 = isPrefixOf empty xs && temCasaFImpares t c ac1 5
                                     where empty = replicate 11 Empty
                                           entr = [Empty, Wall, Wall, Wall, Empty, Empty, Empty, Wall, Wall, Wall, Empty]
                                           parede = [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty]
                                           paredes = [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Empty]

-- *Tarefa 2

{- | 
São testadas as seguintes situações:

- o movimento para todas as direções

- a mudança de direção

- o resultado quando a peça para onde o Pacman se move é Empty, Wall, Food Little ou Food Big

- o encontro do Pacman com um fantasma Dead ou Alive (tendo ou não vidas)

-}
testesT2 :: [(Play, State)]
testesT2 = [((Move 0 L), (State mz1 [pac1] 2)),((Move 0 R), (State mz1 [pac2] 2)),((Move 0 U),
           (State mz1 [pac3] 2)),((Move 0 D), (State mz1 [pac4] 2)), ((Move 0 L), (State mz1 [pac4] 2)),
           ((Move 0 R), (State mz1 [pac2,gh1] 2)),((Move 0 R), (State mz1 [pac2,gh2] 2)),
           ((Move 0 L), (State mz2 [pac5,gh3] 2)), ((Move 0 R), (State mz1 [pac6,gh1] 2))]

mz1 = generateMaze 15 10 1234
mz2 = generateMaze 16 11 1235

-- | Para averiguar se o resultado destes testes é o esperado, apenas se aplicou esta função aos testes definidos anteriormente
-- e verificou-se que o resultado foi o esperado (no Terminal)
--
testesResT2 :: [(Play,State)] -> [State]
testesResT2 = map (\(x,y) -> play x y)  


-- *Tarefa 3

-- | São testados vários labirintos em que se repetem peças consecutivamente e corredores
--
testesT3 :: [Maze]
testesT3 = [mz3, mz4, mz5,mz6]

-- | Retorna um lista de instruções correspondentes à compressão dos labirintos dados
--
testesResT3 :: [Maze] -> [Instructions]
testesResT3 = map (compactMaze)

-- | Verifica se cada instrução é aquivalente ao labirinto na mesma posição
--
verificaInstructions :: [Instructions] -> [Maze] -> Bool
verificaInstructions i m = and (zipWith (instrIgualMaz) i m)

-- | Verifica se um instrução é equivalente a um dado labirinto
--
instrIgualMaz :: Instructions -> Maze -> Bool
instrIgualMaz i m = labIguais m $ constrMaze $ (tiraRepeat i )

-- | Verifica se dois labirintos são iguais
--
labIguais :: Maze -> Maze -> Bool
labIguais [] [] = True
labIguais ((x:xs):t) ((y:ys):z) = x==y && labIguais (xs:t) (ys:z)
labIguais ([]:t) ([]:z) = labIguais t z
labIguais _ _ = False

-- | Transforma instruções num labirinto
--
-- __Obs:__ Considerando que nenhuma instrução é do tipo Repeat x
--
constrMaze :: Instructions -> Maze
constrMaze = map (\(Instruct x) -> constrCorridor x) 

-- | Constroi um corredor apartir de um lista de (Int,Piece),
-- sabendo que o inteiro do par representa o número de vezes que a peça se repete consecutivamente
--
constrCorridor :: [(Int, Piece)] -> Corridor
constrCorridor [] = []
constrCorridor ((x,p):t) | x==1 = p:constrCorridor t
                         | x>1 = p: constrCorridor ((x-1,p):t)

-- | Retira das instruções as linhas em que aparece Repeat x, substituindo pela linha x
--
tiraRepeat :: Instructions -> Instructions
tiraRepeat i = tiraRepetem i (sort (quaisRepetem i [])) 0

-- | Verifica quais os índices das linhas que se repetem
--
quaisRepetem :: Instructions -> [Int] -> [Int]
quaisRepetem [] l = l
quaisRepetem (Repeat x :is) l = if (elem x l) then quaisRepetem is l
                                else quaisRepetem is (l ++ [x])
quaisRepetem (i:is) l = quaisRepetem is l

-- | Retira das instruções as linhas em que aparece Repeat x, substituindo pela linha x, sabendo que os índices
-- das linhas que se repetem
--
tiraRepetem :: Instructions -> [Int] -> Int -> Instructions
tiraRepetem i [] _ = i
tiraRepetem (x:xs) (h:t) i | i<h = x: tiraRepetem xs (h:t) (i+1)
                           | i==h = tiraRepetem (x:tiraRepetem' x xs i) t i
                           where tiraRepetem' :: Instruction -> Instructions -> Int -> Instructions
                                 tiraRepetem' i [] n = []
                                 tiraRepetem' i (Repeat x:t) n = if (x==n) then i:tiraRepetem' i t n
                                                                 else Repeat x: tiraRepetem' i t n
                                 tiraRepetem' i (h:t) n = h: tiraRepetem' i t n
