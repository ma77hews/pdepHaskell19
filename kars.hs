-- modelado de autos

data Auto = Auto {  nombre :: String,
                    nivelDeNafta :: Float,
                    velocidad :: Int,
                    nombreDelEnamorado :: String
                    --truco :: Truco
                    } deriving Show

rochaMcQueen = Auto { nombre = "rochaMcQueen",
                      nivelDeNafta = 300,
                      velocidad = 0,
                      nombreDelEnamorado = "ronco"
                      --truco = deReversaRocha
                    }

biankerr = Auto { nombre ="biankerr",
                  nivelDeNafta = 500,
                  velocidad = 20,
                  nombreDelEnamorado = "tinch"
                  --truco = impresionar
                  }
gushtav = Auto { nombre = "gushtav",
                 nivelDeNafta = 200,
                 velocidad = 130,
                 nombreDelEnamorado = "petiLaLinta"
                 --truco = nitro
                }
rodra = Auto {  nombre = "rodra",
                nivelDeNafta = 0,
                velocidad = 50,
                nombreDelEnamorado = "taisa"
                --truco = fingirAmor(petra)
                }


-- modelado de trucos de los autos

deReversaRocha auto = auto {nivelDeNafta = ((+(0.20 * 1000)).nivelDeNafta)auto }
impresionar auto = auto{velocidad = ((*2).velocidad)auto }
nitro auto = auto{velocidad = ((+15).velocidad)auto}
fingirAmor auto nombre = auto{nombreDelEnamorado = nombre} 
