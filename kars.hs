-- punto 1 parte a
-- modelado de autos

import Text.Show.Functions

-- tipos
type Truco = Auto -> Auto
type Trampa = Carrera -> Carrera



-- datas
data Auto = Auto {  nombre :: String,
                    nivelDeNafta :: Float,
                    velocidad :: Float,
                    nombreDelEnamorado :: String,
                    truco :: Truco
                    } deriving Show

data Carrera = Carrera { cantidadDeVueltas :: Int,
                         longitudDeLaPista :: Float,
                         nombresDelPublico :: [String],
                         trampa :: Trampa,
                         participantes :: [Auto]
                         } deriving Show
--  competidores
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


-- carreras
potreroFunes = Carrera { cantidadDeVueltas = 3,
                         longitudDeLaPista = 0.5,
                         nombresDelPublico = ["ronco", "tinch", "dodain"],
                         trampa  = sacarAlPistero,
                         participantes = [rochaMcQueen, biankerr, gushtav, rodra]
                         }


-- funciones auxiliares de modificaciones
modificarVelocidad calculo auto = auto{velocidad = (calculo.velocidad)auto}
modificarNafta calculo auto = auto{nivelDeNafta = (calculo.nivelDeNafta)auto}

-- trucos
porcentajeDeLaNafta unPorcentaje auto = ((unPorcentaje*).velocidad) auto

deReversaRocha :: Truco
deReversaRocha auto = auto {nivelDeNafta =  (nivelDeNafta auto + (porcentajeDeLaNafta 0.2 auto)) }

impresionar :: Truco
impresionar = modificarVelocidad (*2)

nitro :: Truco
nitro = modificarVelocidad (+15)

fingirAmor :: String -> Truco
fingirAmor nombre auto = auto{nombreDelEnamorado = nombre}

comboLoco :: Truco
comboLoco = (deReversaRocha.nitro)

cambiarDeEnamorado :: String -> Truco
cambiarDeEnamorado enamorado unAuto  = unAuto{nombreDelEnamorado = enamorado }

queTrucazo :: String -> Truco
queTrucazo unNombre  = incrementarVelocidad.(cambiarDeEnamorado unNombre)

turbo :: Truco
turbo unAuto = unAuto {velocidad = ((velocidad unAuto ) + (((10*).nivelDeNafta)) unAuto), nivelDeNafta = 0}

--punto 2
-- incrementar velocidad

contarVocales = length.filter (\letra->elem letra "aeiouAEIOU")

-- between nos dice si un numero esta entre otros dos
between cotaInferior cotaSuperior numero = (numero >= cotaInferior)&& (numero <= cotaSuperior)
cantidadDeVocalesEnamoradoEntre cotaInferior cotaSuperior = (between cotaInferior cotaSuperior).contarVocales.nombreDelEnamorado

incrementarVelocidad auto
  | cantidadDeVocalesEnamoradoEntre 1 2 auto = modificarVelocidad (+15) auto
  | cantidadDeVocalesEnamoradoEntre 3 4 auto = modificarVelocidad (+20) auto
  | ((4<).contarVocales.nombreDelEnamorado) auto = modificarVelocidad (+30) auto


-- punto 3
-- puede realizar truco
tieneNaftaEnElTanque  unAuto = ((0<).nivelDeNafta) unAuto
velocidadMenorA100 unAuto = ((100>).velocidad) unAuto
puedeRealizarTruco unAuto = (tieneNaftaEnElTanque unAuto) && (velocidadMenorA100 unAuto)


-- funciones varias
modificarParticipantes funcion carrera = carrera {participantes = funcion (participantes carrera)}

nivelCriticoDeNafta = (<30).nivelDeNafta
-- trampas

sacarAlPistero :: Trampa
sacarAlPistero carrera = carrera {participantes = (tail.participantes) carrera}

lluvia :: Trampa
lluvia  = modificarParticipantes (map (modificarVelocidad (+ (-15))))

inutilidad :: Truco
inutilidad = id

noHacerNada :: Truco
noHacerNada unAuto = unAuto {truco = inutilidad}

neutralizarTrucos :: Trampa
neutralizarTrucos = modificarParticipantes (map noHacerNada)

pocaReserva :: Trampa
pocaReserva  = modificarParticipantes (filter (not.nivelCriticoDeNafta))

podio :: Trampa
podio = modificarParticipantes (take 3)


-- a correr
