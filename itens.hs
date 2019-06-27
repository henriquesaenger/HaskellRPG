

data Weapon= Weapon{nome::[Char],dano :: int}

data Cura= Cura{nome::[Char], heal::Int}

data Item= Weapon | Cura

data Mochila = [Item]

