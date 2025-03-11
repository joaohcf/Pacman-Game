{- |
= Introdução

Nesta tarefa, tinhamos como objetivo implementar um robô que jogue Pacman normalmente.

= Metas

Tal como mencionado na introdução, a meta desta tarefa era implementar um robô que jogue Pacman normalmente. Para chegar a esta meta, necessitámos de criar funções para movimentação do bot.
Para além da movimentação do robô tentámos fazer com que este verificasse se na sua proximidade havia a potencial existência de um fantasma, tentando desviar-se deste e podendo, movimentar-se
noutra direção onde existissem comidas.
 
= Conclusão

Concluindo este resumo da Tarefa6, na nossa sincera opinião, foi a tarefa onde encontramos mais dificuldades, mas julgamos que obtivemos um resultado mais ao menos satisfátorio, apesar de poder 
melhorar em alguns aspetos, tal como a criação de ainda mais "niveis" e/ou prioridades para o bot, de maneira a que este ficasse ainda mais "desenvolvido". 
-}

module Tarefa6 where
import Tarefa2
import Types

-- = Função Principal
-- | Devolve uma possível jogada do bot.
--
bot :: Int -> State -> Maybe Play
bot x state = if checkDirection (getPieceOrientation (PacPlayer (getPlayerByID x (playersState state)))) (getPlayerCoordsByID x (playersState state)) (playersState state) then
                  Just (Move x (changeBotOrientation (getPieceOrientation (PacPlayer (getPlayerByID x (playersState state))))))
              else
                  if checkFood (getCorridor (fst $ getPlayerCoordsByID x (playersState state)) (maze state))then
                     if (fst $ getPlayerCoordsByID x (playersState state)) < middleWidth (maze state) then
                        Just (botCollideWall state (Move x R))
                     else
                        Just (botCollideWall state (Move x L))
                  else
                     Just (botCollideWall state (Move x D))

-- | Verifica se na direção atual irá encontrar algum fantasma num espaço de 3 casas.
--
checkDirection :: Orientation -> Coords -> [Player] -> Bool
checkDirection o (x,y) []                                   = False
checkDirection o (x,y) (ghost@(Pacman(PacState a b c d)):t) = checkDirection o (x,y) t
checkDirection o (x,y) (ghost@(Ghost(GhoState a b)):t)      = checkGhostsCoords 3 o (x,y) ghost

-- | Verifica se um fantasma está numa determinada posição.
--
checkGhostsCoords :: Int -> Orientation -> Coords -> Player -> Bool
checkGhostsCoords 0 o (x,y) player = False
checkGhostsCoords a o (x,y) player = getPlayerCoords player == (x,y) || checkGhostsCoords (a-1) o (getNewCoords (x,y) o) player

-- | Devolve o corredor em que o jogador está.
--
getCorridor :: Int -> Maze -> Corridor
getCorridor x maze = maze !! x

-- | Verifica se exite alguma comida num dado corredor.
--
checkFood :: Corridor -> Bool
checkFood [] = False
checkFood (h:t) = (h == Food Little || h == Food Big) || checkFood t

-- | Muda a orientação do bot.
--
changeBotOrientation :: Orientation -> Orientation
changeBotOrientation R = D
changeBotOrientation D = L
changeBotOrientation L = U
changeBotOrientation U = R

-- | Compara o estado do fantasma em dois estados de jogo diferentes.
--
comparePacState :: Player -> Player -> Bool
comparePacState (Pacman(PacState(_,(x,y),_,o1,_,_)_ _ _)) (Pacman(PacState(_,(a,b),_,o2,_,_)_ _ _)) = (x,y) == (a,b) && o1 == o2

-- | Verifica se bot colidiu em uma parede e altera a orientação.
--
botCollideWall :: State -> Play -> Play
botCollideWall state (Move id orientation) = if comparePacState (getPlayerByID id (playersState state)) (getPlayerByID id (playersState (play (Move id orientation) state))) then
                                                Move id (changeBotOrientation(getPieceOrientation (PacPlayer (getPlayerByID id (playersState state)))))
                                             else
                                                Move id orientation
