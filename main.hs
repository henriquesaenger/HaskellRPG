import person.hs

data Point= Point{x::Int, y::Int}		--cria um tipo ponto para setar a posição de um personagem no mapa
 deriving(Show)

data Map= [Point]						--cria o tipo mapa para poder montar os mapas referentes ao jogo
 deriving(Show)
