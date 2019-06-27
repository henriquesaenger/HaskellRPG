

data Player = Player {Position::Point, HP::Int, Energy::Int}
 deriving(Show)

data Monster= Monster {Position::Point, HP::Int, Energy::Int, Attack::Int}
 deriving(Show)