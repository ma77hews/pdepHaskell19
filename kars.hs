type Truco = Auto
data Auto = Auto {  nombre :: String,
                    nivelDeNafta :: Int,
                    velocidad :: Int,
                    nombreDelEnamorado :: String
                    --truco :: Truco
                    } deriving Show

rochaMcQueen = Auto { nombre = "rochaMcQueen",
                      nivelDeNafta = 300,
                      velocidad = 0,
                      nombreDelEnamorado = "ronco"
                      --truco = deReversa
                    }

biankerr = Auto { nombre ="biankerr",
                  nivelDeNafta = 500,
                  velocidad = 0,
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
