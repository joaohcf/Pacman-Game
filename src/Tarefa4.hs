module Tarefa4 where

import Types
import Tarefa2
import Tarefa5
import Tarefa6

{- |
= Introdução

Nesta tarefa, tinhamos como objetivo calcular o efeito da passagem de um instante de tempo num estado do jogo.

= Metas

Tal como mencionado na introdução, a meta desta tarefa era calcular o efeito da passagem de um instante de tempo num estado de jogo. Dito isto, a tarefa estava dividida em 2 componentes, a primeira
onde integraríamos os tipos e funções que advêm da 1ª fase numa biblioteca denominada Ncurses, a segunda que usa o estado e informação vinda do primeiro componente para fazer avançar o estado em uma 
iteração, fazendo “movimentar” todos os jogadores no mapa. De certa forma, as principais metas desta tarefa consistiam na integração da biblioteca Ncurses, na construção de algumas funções  
que alteram o estado dos jogadores e alteração de outras (de tarefas anteriores), bem como funções para execução de certas jogadas e, finalmente, funções que trabalhavam a velocidade dos jogadores
em função da paridade da iteração (step). Deste modo, conseguimos chegar à principal meta/função que tem como objetivo a execução de jogadas consoante o tempo de jogo.

= Conclusão

Concluindo este resumo da Tarefa4, para obter um resultado satisfatório nesta tarefa, necessitámos de reformular a maioria do código das funções da Tarefa2, de maneira a 
melhorar a performance do jogo, bem como coexistir com todas as novas funções adicionadas na Tarefa4, de forma a que fosse possível calcular o efeito da passagem de tempo no estado de jogo. Em suma,
julgo que conseguimos obter resultados produtivos, apesar de poder melhor em alguns aspetos, tal como qualidade de código, entre outros.
-}

-- = Função Principal
-- | Executa jogadas consoante o tempo de jogo.
--
passTime :: Int -> State -> State
passTime step state@(State maze playersState level) = executeGhostPlay (ghostPlay newState) newState
                                                    where newState = editPlayers (movePlayers step state playersState)

-- | Muda o estado da boca do Pacman (Aberta/Fechada) a cada jogada.
--
pacmanMouth :: Player -> Player
pacmanMouth (Pacman (PacState a b Open d))   = Pacman (PacState a b Closed d)
pacmanMouth (Pacman (PacState a b Closed d)) = Pacman (PacState a b Open d)

-- | Altera o tempo restante em modo Mega do Pacman.
--
pacmanMegaTime :: Player -> Player
pacmanMegaTime (Pacman (PacState a b c Normal)) = Pacman (PacState a b c Normal)
pacmanMegaTime (Pacman (PacState a b c Mega))   = if b <= 0 then Pacman (PacState a 0 c Normal) else Pacman (PacState a (b-1) c Mega)
pacmanMegaTime (Pacman (PacState a b c Dying))  = Pacman (PacState a b c Dying)

-- | Movimenta todos os jogadores no labirinto.
--
movePlayers :: Int -> State -> [Player] -> State
movePlayers step state [] = state
movePlayers step state (h:t) = movePlayers step (movePlayer (playerMoves step (getPlayerVelocity h)) (Move (getPlayerID h) (getPlayerOrientation h)) state) t

-- | Move o jogador consoante a sua velocidade.
--
movePlayer :: Int -> Play -> State -> State
movePlayer 0 move state = state
movePlayer x move state = movePlayer (x-1) move (play move state)

-- | Devolve a velocidade do jogador.
--
getPlayerVelocity :: Player -> Double
getPlayerVelocity (Pacman(PacState (a,b,c,d,e,f) g h i)) = c
getPlayerVelocity (Ghost(GhoState (a,b,c,d,e,f) g)) = c

-- | Devolve quantas vezes o jogador vai jogar numa determinada iteração.
--
playerMoves :: Int -> Double -> Int
playerMoves step velocity = if fromIntegral (floor velocity*2) == velocity*2 then
                                round velocity
                            else
                                if mod step 2 == 0 then
                                    round (velocity*2)
                                else
                                    0

-- | Altera estado de alguns jogadores.
--
editPlayers :: State -> State
editPlayers (State maze playersState level) = State maze (editPlayersAux playersState) level

-- | Altera estado de alguns jogadores.
--
editPlayersAux :: [Player] -> [Player]
editPlayersAux [] = []
editPlayersAux (pac@(Pacman x):t)  = pacmanMegaTime (pacmanMouth pac) : editPlayersAux t
editPlayersAux (ghost@(Ghost x):t) = ghost : editPlayersAux t

-- | Executa as jogadas dos fantasmas (bots).
--
executeGhostPlay :: [Play] -> State -> State
executeGhostPlay  [] state = state
executeGhostPlay  ((Move id orientation):t) state = executeGhostPlay  t (beforePlay id orientation state)

-- | Executa uma jogada apenas se o jogador alterar a orientação.
--
beforePlay :: Int -> Orientation -> State -> State
beforePlay id orientation state@(State maze playersState level) = if orientation == getPieceOrientation (PacPlayer(getPlayerByID id playersState)) then state
                                                                  else play (Move id orientation) state