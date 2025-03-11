module Tarefa5 where

import Types
import Tarefa2

{- |
= Introdução

Nesta tarefa, tinhamos como objetivo implementar um comportamento para os fantasmas.

= Metas

Tal como mencionado na introdução, a meta desta tarefa era implementar um comportamento para os fantasmas. Para chegar a esta meta, inicialmente, tivemos que complementar novamente, as tarefas
da Tarefa2, já que necessitávamos de considerar que a função play poderia receber jogadas de fantasmas. Para além disso, desenvolvemos funções que complementavam a movimentação do fantasma.
Estas funções são diferenciadas pelo estado do fantasma, isto é, quando o fantasma está Alive, ele procura encontrar o Pacman ( de formas variadas como movimentação na horizontal 
e na vertical ou aleatória), por outro lado, quando está Dead, ele procura fugir do Pacman. Deste modo conseguimos implementar um comportamento para os fantasmas.
 
= Conclusão

Concluindo este resumo da Tarefa5, para obter um resultado satisfatório, tivemos que complementar algumas funções de tarefas anteriores (nomeadamente Tarefa2 e Tarefa4), de forma a conseguir
desenvolver um conjunto de jogadas diferenciadas para os fantasmas. Em suma, penso que o trabalho desenvolvido pelo grupo nesta tarefa foi bastante razoável.
-}

-- = Função principal
-- | Devolve um conjunto de jogadas feitas pelos bots.
--
ghostPlay :: State -> [Play]
ghostPlay state = leaveHouse state (playersState state)

-- | Tira o jogador da casa dos fantasmas.
--
leaveHouse :: State -> [Player] -> [Play]
leaveHouse state [] = []
leaveHouse state ((Pacman x):t) = leaveHouse state t
leaveHouse state ((Ghost (GhoState (a,(x,y),c,d,e,f) g)):t) =   if x >= middleHeight mz-2 && x <= middleHeight mz && y >= middleWidth mz-4 && y <= middleWidth mz+4 then
                                                                    if y == middleWidth mz then
                                                                            Move a U : leaveHouse state t
                                                                        else
                                                                            if y < middleWidth mz then
                                                                                Move a R : leaveHouse state t
                                                                            else
                                                                                Move a L : leaveHouse state t
                                                                else
                                                                    if g == Alive then chaseMode state a : leaveHouse state t else scatterMode state a : leaveHouse state t
                                                                where mz = maze state

-- | Movimenta os fantasmas para caçarem o Pacman.
--
chaseMode :: State -> Int -> Play
chaseMode state 1 = ghostCollideWall1 state (horizontalMove 1 (playersState state))
chaseMode state 2 = ghostCollideWall2 state (verticalMove 2 (playersState state))
chaseMode state 3 = ghostCollideWall3 state (Move 3 (getPieceOrientation (PacPlayer(getPlayerByID 3 (playersState state)))))
chaseMode state 4 = ghostCollideWall4 state (Move 4 (getPieceOrientation (PacPlayer(getPlayerByID 4 (playersState state)))))

-- | Movimenta os fantasmas em modo Dead para fugirem do Pacman.
--
scatterMode :: State -> Int -> Play
scatterMode state id = ghostScatterCollideWall state (Move id (getPieceOrientation (PacPlayer(getPlayerByID id (playersState state)))))

-- | Movimenta o fantasma com prioridade na horizontal.
--
horizontalMove :: Int -> [Player] -> Play
horizontalMove id players = Move id (ghostHorizontal (getPlayerCoords(getPlayerByID id players)) (getPlayerCoords(getPlayerByID 0 players)))

-- | Movimenta o fantasma com prioridade na vertical.
--
verticalMove :: Int -> [Player] -> Play
verticalMove id players = Move id (ghostVertical (getPlayerCoords(getPlayerByID id players)) (getPlayerCoords(getPlayerByID 0 players)))

-- | Altera a orientação do fantasma com prioridade horizontal.
--
ghostHorizontal :: Coords -> Coords -> Orientation
ghostHorizontal (x,y) (m,n) | x < m = D
                            | x > m = U
                            | x == m && y < n = R
                            | x == m && y > n = L

-- | Altera a orientação do fantasma com prioridade vertical.
--
ghostVertical :: Coords -> Coords -> Orientation
ghostVertical (x,y) (m,n) | y < n = R
                          | y > n = L
                          | y == n && x < m = D
                          | y == n && x > m = U

-- | Compara o estado do fantasma em dois estados de jogo diferentes.
--
comparePlayerState :: Player -> Player -> Bool
comparePlayerState (Ghost(GhoState(_,(x,y),_,o1,_,_)_)) (Ghost(GhoState(_,(a,b),_,o2,_,_)_)) = (x,y) == (a,b) && o1 == o2

-- | Altera a orientação do fantasma no sentido dos ponteiros do relógio.
--
goRight :: Orientation -> Orientation
goRight U = R
goRight R = D
goRight D = L
goRight L = U
goRight Null = Null 

-- | Altera a orientação do fantasma no sentido contrário aos ponteiros do relógio.
--
goLeft :: Orientation -> Orientation
goLeft U = L
goLeft L = D
goLeft D = R
goLeft R = U
goLeft Null = Null

-- | Verifica se o fantasma (Alive) colidiu em uma parede e altera a orientação.
--
ghostCollideWall1 :: State -> Play -> Play
ghostCollideWall1 state (Move id orientation) =  if comparePlayerState (getPlayerByID id (playersState state)) (getPlayerByID id (playersState (play (Move id orientation) state))) then
                                                    Move id (goRight(getPieceOrientation (PacPlayer (getPlayerByID id (playersState state)))))
                                                else
                                                    Move id orientation

ghostCollideWall2 :: State -> Play -> Play
ghostCollideWall2 state (Move id orientation) =  if comparePlayerState (getPlayerByID id (playersState state)) (getPlayerByID id (playersState (play (Move id orientation) state))) then
                                                    Move id (goLeft(getPieceOrientation (PacPlayer (getPlayerByID id (playersState state)))))
                                                else
                                                    Move id orientation

ghostCollideWall3 :: State -> Play -> Play
ghostCollideWall3 state (Move id orientation) =  if comparePlayerState (getPlayerByID id (playersState state)) (getPlayerByID id (playersState (play (Move id orientation) state))) then
                                                    Move id (goRight(getPieceOrientation (PacPlayer (getPlayerByID id (playersState state)))))
                                                else
                                                    Move id orientation

ghostCollideWall4 :: State -> Play -> Play
ghostCollideWall4 state (Move id orientation) =  if comparePlayerState (getPlayerByID id (playersState state)) (getPlayerByID id (playersState (play (Move id orientation) state))) then
                                                    Move id (goLeft(getPieceOrientation (PacPlayer (getPlayerByID id (playersState state)))))
                                                else
                                                    Move id orientation

-- | Verifica se o fantasma (Dead) colidiu em uma parede e altera a orientação.
--
ghostScatterCollideWall :: State -> Play -> Play
ghostScatterCollideWall state (Move id orientation) =   if comparePlayerState (getPlayerByID id (playersState state)) (getPlayerByID id (playersState (play (Move id orientation) state))) then
                                                            Move id (goRight(getPieceOrientation (PacPlayer (getPlayerByID id (playersState state)))))
                                                        else
                                                            Move id orientation