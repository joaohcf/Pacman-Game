module Tarefa3 where

import Types

-- | Compacta o labirinto, chama a função 'compactCorridors'.
compactMaze :: Maze -> Instructions
compactMaze maze = compactCorridors maze

-- | Compacta corredores do labirinto, chama as funções 'compactPieces', 'corridorIndex' e 'compactCorridor'.
compactCorridors :: Maze -> Instructions
compactCorridors [] = []
compactCorridors [x] = [Instruct (compactPieces(compactCorridor x))]
compactCorridors (h:t) = if last t `elem` (h:init t) then compactCorridors (h:init t) ++ [Repeat (corridorIndex (h:init t) (last t) 0)] else compactCorridors (h:init t) ++ [Instruct (compactPieces(compactCorridor(last t)))]

-- | Indica o Index de um determinado corredor.
corridorIndex :: [Corridor] -> Corridor -> Int -> Int
corridorIndex (h:t) x z = if x == h then z else corridorIndex t x z+1

-- | Compacta um corredor, chama a função 'compactPieces'.
compactCorridor :: Corridor -> [(Int,Piece)]
compactCorridor [] = []
compactCorridor (h:t) = compactPiece h : compactCorridor t

-- | Compacta um conjunto de peças consecutivas.
compactPieces :: [(Int,Piece)] -> [(Int,Piece)]
compactPieces [] = []
compactPieces [x] = [x]
compactPieces (h:t) = if snd h == snd (head t) then compactPieces (addPiece h:tail t) else h : compactPieces t

-- | Transforma uma peça em uma instrução.
compactPiece :: Piece -> (Int,Piece)
compactPiece piece = case piece of Wall -> (1, Wall)
                                   Empty -> (1, Empty)
                                   Food Big -> (1, Food Big)
                                   Food Little -> (1, Food Little)

-- | Contabiliza quantas peças consecutivas são iguais.
addPiece :: (Int, Piece) -> (Int, Piece)
addPiece (x,piece) = (x+1,piece)