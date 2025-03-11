{- |
= Introdução

Nesta tarefa, tinhamos como objetivo implementar um mecanismo de geração de labirintos.

= Metas

A principal meta desta tarefa era conseguir gerar um labirinto, onde os inputs seriam: a sua largura, a sua altura e uma seed (um número inteiro), respetivamente.
Dito isto, neste labirinto tinhamos que incluir um túnel, que variava de tamanho face ao número par (túnel formado por 2 corredores) ou ímpar (túnel formado por 1 corredor) de altura.
Para além do túnel, tinhamos que também desenvolver a casa dos fantasmas cuja altura fixa era 3 e variava de largura (largura 9 se a largura do labirinto fosse ímpar ou 8 se fosse par).
Isto, para além de todas as outras funções que tivemos de desenvolver pedidas no trabalho e/ou auxiliadoras no processo de geração do labirinto.

= Conclusão

Em suma, penso que apesar de algumas dificuldades iniciais, fizemos um bom trabalho face aos nossos conhecimentos atuais. 
Penso que conseguimos atingir o nosso principal objetivo desta tarefa que era passar em todos os testes no sistema de avaliação dos professores, mesmo sabendo que podíamos melhorar em alguns aspetos, tal como, a qualidade do código.
-}
module Tarefa1 where

import System.Random
import Types

-- | Dada uma seed devolve uma lista de n inteiros gerados aleatóriamente.
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = let gen = mkStdGen seed
                        in take n $ randomRs (0,99) gen

-- | Dada uma seed devolve um inteiro gerado aleatóriamente.
nrAleatorio :: Int -> Int
nrAleatorio seed = head $ geraAleatorios 1 seed

-- | Converte uma lista em uma lista de listas de tamanho n.
subLista :: Int -> [a] -> [[a]]
subLista _ [] = []
subLista n l = take n l: subLista n (drop n l)

-- | Converte número numa peça.
convertPiece :: Int -> Piece
convertPiece n
    | n == 3            = Food Big    --- ^ n = 3 corresponde a comida grande.
    | n >= 0 && n <= 70 = Food Little --- ^ n <= 0 && n <= 70 corresponde a comida pequena.
    | otherwise         = Wall        --- ^ restantes casos correspondem a parede.

-- | Converte o corredor numa string.
printCorridor :: Corridor -> String
printCorridor [] = ""
printCorridor (h:t) = show h ++ printCorridor t

-- | Converte o labirinto numa string.
printMaze :: Maze -> String
printMaze [] = ""
printMaze (h:t) = printCorridor h ++ "\n" ++ printMaze t

-- | Converte uma lista de números interios em corredor chamando a função 'convertePiece'.
convertCorridor :: [Int] -> Corridor
convertCorridor [] = []
convertCorridor (h:t) = convertPiece h : convertCorridor t

-- | Converte uma lista de listas de números inteiros em labirinto chamando as funções 'replaceWithWall' e 'convertCorridor'.
convertMaze :: [[Int]] -> Maze
convertMaze [] = []
convertMaze (h:t) = replaceWithWall(convertCorridor h) : convertMaze t

-- | Gera corredores do tipo Parede (sempre horizontal).
genWall :: Int -> Corridor
genWall 0 = []
genWall x = Wall : genWall (x-1)

-- | Transforma o topo e o fundo do labirinto em corredores do tipo Parede.
putWall :: Int -> Maze -> Maze
putWall _ [] = []
putWall x maze = [genWall x] ++ init(tail maze) ++ [genWall x]

-- | Substitui primeira e última peça de cada corredor com 'Wall'.
replaceWithWall :: Corridor -> Corridor
replaceWithWall [] = []
replaceWithWall corridor = [Wall] ++ init(tail corridor) ++ [Wall]

-- | Calcula metade da altura do labirinto.
middleHeight :: Maze -> Int
middleHeight maze = div (length maze) 2

-- | Calcula metade da largura do labirinto.
middleWidth :: Maze -> Int
middleWidth (h:_) = div (length h) 2

-- | Obtém os corredores do meio do labirinto.
middleCorridors :: Maze -> [Corridor]
middleCorridors maze = if even (length maze) then take 2 (drop ((middleHeight maze)-1) maze)
                       else take 1 (drop (middleHeight maze) maze)

-- | Acrescenta peça 'Empty' para criar entrada no(s) corredor(es) do túnel.
breakWall :: [Corridor] -> [Corridor]
breakWall [] = []
breakWall (x:xs) = ([Empty] ++ (init(tail x)) ++ [Empty]) : breakWall xs

-- | Constrói o túnel chamando as funções 'middleHeight', 'breakWall' e 'middleCorridors'.
openTunnel :: Maze -> Maze
openTunnel maze = if even (length maze) then take (midH-1) maze ++ tunnel ++ drop (midH+1) maze 
                  else take midH maze ++ tunnel ++ drop (midH+1) maze 
                  where midH = middleHeight maze 
                        tunnel = breakWall (middleCorridors maze)

-- | Constrói casa dos fantasmas chamando as funções 'middleHeight' e 'middleCorridorsh'.
buildHouse :: Maze -> Maze
buildHouse maze = if even (length maze) then take (midH-3) maze ++ ghosthouse ++ drop (midH+2) maze 
                  else take (midH-2) maze ++ ghosthouse ++ drop (midH+3) maze 
                  where midH = middleHeight maze 
                        ghosthouse = house (middleCorridorsh maze)

-- | Formato da casa dos fantasmas.
house :: [Corridor] -> [Corridor]
house (a:x:y:z:b:rs) = if even (length x) then (take (midW-5) a ++ [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty] ++ drop (midW+5) a) : (take (midW-5) x ++ [Empty,Wall,Wall,Wall,Empty,Empty,Wall,Wall,Wall,Empty] ++ drop (midW+5) x) : (take (midW-5) y ++ [Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty] ++ drop (midW+5) y) : (take (midW-5) z ++ [Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty] ++ drop (midW+5) z) : (take (midW-5) b ++ [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty] ++ drop (midW+5) b) : rs
                       else (take (midW-5) a ++ [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty] ++ drop (midW+6) a) : (take (midW-5) x ++ [Empty,Wall,Wall,Wall,Empty,Empty,Empty,Wall,Wall,Wall,Empty] ++ drop (midW+6) x) : (take (midW-5) y ++ [Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty] ++ drop (midW+6) y) : (take (midW-5) z ++ [Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty] ++ drop (midW+6) z) : (take (midW-5) b ++ [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty] ++ drop (midW+6) b) : rs
                       where maze = a:x:y:z:b:rs
                             midW = middleWidth maze

-- | Obtém os corredores do meio (função auxiliar para criação da casa dos fantasmas).
middleCorridorsh :: Maze -> [Corridor]
middleCorridorsh maze = if even (length maze) then take 5 (drop ((middleHeight maze)-3) maze)
                        else take 5 (drop ((middleHeight maze)-2) maze)

-- | Gera o labirinto.
generateMaze :: Int -> Int -> Int -> Maze
generateMaze x y seed = openTunnel $ buildHouse $ putWall x (convertMaze (subLista x (geraAleatorios (x*y) seed)))