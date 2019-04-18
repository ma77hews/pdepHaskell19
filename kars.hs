-- punto 1 parte a
-- modelado de autos

data Auto = Auto {  nombre :: String,
                    nivelDeNafta :: Float,
                    velocidad :: Float,
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

-- punto 1 parte b
-- modelado de trucos de los autos

deReversaRocha auto = auto {nivelDeNafta = ((+(0.20 * 1000)).nivelDeNafta)auto }
impresionar auto = auto{velocidad = ((*2).velocidad)auto }
nitro auto = auto{velocidad = ((+15).velocidad)auto}
fingirAmor auto nombre = auto{nombreDelEnamorado = nombre}


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
