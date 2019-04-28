-- punto 1 parte a
-- modelado de autos

import Text.Show.Functions

type Truco = Auto -> Auto

data Auto = Auto {  nombre :: String,
                    nivelDeNafta :: Float,
                    velocidad :: Float,
                    nombreDelEnamorado :: String,
                    truco :: Truco
                    } deriving Show

rochaMcQueen = Auto { nombre = "rochaMcQueen",
                      nivelDeNafta = 300,
                      velocidad = 0,
                      nombreDelEnamorado = "ronco",
                      truco = deReversaRocha
                    }

biankerr = Auto { nombre ="biankerr",
                  nivelDeNafta = 500,
                  velocidad = 20,
                  nombreDelEnamorado = "tinch",
                  truco = impresionar
                  }

gushtav = Auto { nombre = "gushtav",
                 nivelDeNafta = 200,
                 velocidad = 130,
                 nombreDelEnamorado = "petiLaLinta",
                 truco = nitro
                }

rodra = Auto {  nombre = "rodra",
                nivelDeNafta = 0,
                velocidad = 50,
                nombreDelEnamorado = "taisa",
                truco = fingirAmor("petra")
                }

-- punto 1 parte b
-- modelado de trucos de los autos

deReversaRocha :: Auto -> Auto
deReversaRocha auto  = auto {nivelDeNafta = ((+(0.20 * 1000)).nivelDeNafta)auto }

modificarVelocidad calculo auto = auto{velocidad = (calculo.velocidad)auto}

impresionar :: Auto -> Auto
impresionar = modificarVelocidad (*2)
--impresionar auto = auto{velocidad = ((*2).velocidad)auto }

nitro :: Auto -> Auto
nitro = modificarVelocidad (+15)
--nitro auto = auto{velocidad = ((+15).velocidad)auto}

fingirAmor :: String -> Auto -> Auto
fingirAmor nombre auto = auto{nombreDelEnamorado = nombre}


--punto 2
-- incrementar velocidad
between cotaInferior cotaSuperior numero = (numero >= cotaInferior)&& (numero <= cotaSuperior)

contarVocales auto = (length.concat) (map ($ (nombreDelEnamorado auto)) [esVocalA,esVocalE,esVocalI,esVocalO,esVocalU])
esVocalA = filter(== 'a')
esVocalE = filter(== 'e')
esVocalI = filter(== 'i')
esVocalO = filter(== 'o')
esVocalU = filter(== 'u')

incrementarVelocidad auto
  | ((between 1 2).contarVocales) auto = auto{velocidad = ((+15).velocidad)auto}
  | ((between 3 4).contarVocales) auto = auto{velocidad = ((+20).velocidad)auto}
  | ((4<).contarVocales) auto = auto{velocidad = ((+30).velocidad)auto}


-- punto 3
-- puede realizar truco
tieneNaftaEnElTanque  unAuto = ((0<).nivelDeNafta) unAuto
velocidadMenorA100 unAuto = ((100>).velocidad) unAuto
puedeRealizarTruco unAuto = (tieneNaftaEnElTanque unAuto) && (velocidadMenorA100 unAuto)

-- punto 4
-- nuevos trucos

comboLoco = (deReversaRocha.nitro)
cambiarDeEnamorado unAuto enamorado = unAuto{nombreDelEnamorado = enamorado }
queTrucazo unAuto =  incrementarVelocidad.(cambiarDeEnamorado unAuto)
turbo unAuto = unAuto {velocidad = ((velocidad unAuto ) + (((10*).nivelDeNafta)) unAuto), nivelDeNafta = 0}

{-

tests punto 3.1

(nivelDeNafta.deReversaRocha) rochaMcQueen
(velocidad.impresionar) bienkerr
(velocidad.nitro) gushtav
(nombreDelEnamorado.(flip fingirAmor "petra")) rodra

tests punto 3.2

(velocidad.incrementarVelocidad) rochaMcQueen
(velocidad.incrementarVelocidad) biankerr
(velocidad.incrementarVelocidad) gushtav
(velocidad.incrementarVelocidad) rodra


tests punto 3.3

puedeRealizarTruco rochaMcQueen
puedeRealizarTruco gushtav
puedeRealizarTruco rodra

tests punto 3.4

(nivelDeNafta.comboLoco) rochaMcQueen
(velocidad.comboLoco) rochaMcQueen
(velocidad.(flip queTrucazo "murcielago")) rodra
(velocidad.turbo) gushtav
(nivelDeNafta.turbo) gushtav
(velocidad.turbo) rodra
(nivelDeNafta.turbo) rodra
-}
