
import System.Random
import itens.hs

data Player = Player {position::Point, hp::Int, energy::Int, attack::Int}
 deriving(Show)

random:: ()->Int
random ()= RandomR(1,20::Int)			--Devolve um número random entre 1 e 20(simulando a rolagem de um dado)


ataque::Player-> Player-> Player 		--Recebem um jogador e um monstro e devolve um jogador
ataque jogador(_ hp _ _) monstro(_ _ _ attack)=	if (random () <= 10) then () else (hp=hp-attack)		--Se o jogador tirar 10 ou menos não acontece nada, se tirar mais que 10, o ataque acontece

