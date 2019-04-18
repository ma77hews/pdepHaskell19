type Truco = Auto
data Auto = Auto {  nombre :: String,
                    nivelDeNafta :: Int,
                    velocidad :: Int,
                    nombreDelEnamorado :: String,
                    truco :: Truco
                    }deriving Show
