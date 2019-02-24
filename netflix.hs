data Serie = UnaSerie { 
    nombre :: String, 
    genero :: String,
    duracion :: Int,
    cantTemporadas :: Int,
    calificaciones :: [Int],
    esOriginalDeNetflis :: Bool
} deriving (Eq, Show)

tioGolpetazo = UnaSerie {
    nombre = "One punch man",
    genero = "Monito chino",
    duracion = 24,
    cantTemporadas = 1,
    calificaciones = [5],
    esOriginalDeNetflis = False
}

cosasExtranias = UnaSerie {
    nombre = "Strangers things",
    genero = "Misterio",
    duracion = 50,
    cantTemporadas = 2,
    calificaciones = [3,3],
    esOriginalDeNetflis = True
}

dbs = UnaSerie {
    nombre = "Dragon ball supah",
    genero = "Monito chino",
    duracion = 150,
    cantTemporadas = 5,
    calificaciones = [],
    esOriginalDeNetflis = False
}

espejoNegro = UnaSerie {
    nombre = "Black mirrow",
    genero = "Suspenso",
    duracion = 123,
    cantTemporadas = 4,
    calificaciones = [2,2,4],
    esOriginalDeNetflis = True
}

rompiendoMalo = UnaSerie {
    nombre = "Breaking Bad",
    genero = "Drama",
    duracion = 200,
    cantTemporadas = 5,
    calificaciones = [5,1,2,4,5],
    esOriginalDeNetflis = False
}

treceRazonesPorque = UnaSerie {
    nombre = "13 reasons why",
    genero = "Drama",
    duracion = 50,
    cantTemporadas = 1,
    calificaciones = [3,3,3],
    esOriginalDeNetflis = True
}

-- PARTE 1

-- 1 Crear maraton

maratonAnime = [tioGolpetazo, dbs]

maratonPrecoz = [treceRazonesPorque, tioGolpetazo]

maratonDramatico = [treceRazonesPorque, rompiendoMalo]

maratonOriginalDeNetflis = [treceRazonesPorque, espejoNegro]

-- 2 Saber la cantidad de series del maraton

cuantasSeriesMaraton maraton = length maraton

-- 3 Saber si una serie es popular

esPopular serie = length (calificaciones serie) == 3 || length (calificaciones serie) > 3

-- 4 Saber si la serie vale la pena

serieValeLaPena serie = (cantTemporadas serie) > 1 && esPopular serie

-- 5 Saber si un maraton vale la pena

maratonValeLaPena maraton = (serieValeLaPena (head maraton) && serieValeLaPena (last maraton)) || elem (rompiendoMalo) maraton

-- 6 Saber si un maraton repunta al final

maratonRepunta maraton = not (maratonValeLaPena (primerMitad maraton) ) && maratonValeLaPena (segundaMitad maraton)

primerMitad maraton = take (length maraton `div` 2) maraton

segundaMitad maraton = drop (length maraton `div` 2) maraton

-- 7 calcular calificaciones

promediarCalificacion serie | length (calificaciones serie) ==0 = 0
                |otherwise = (sum (calificaciones serie)) `div` (length (calificaciones serie))

-- 8 obtener dispersion

dispersion serie | (promediarCalificacion serie) == 0 = 0
                 |otherwise = (maximum (calificaciones serie)) - (minimum (calificaciones serie))


-- 9 calificar serie

calificar serie calificacion = (calificaciones serie) ++ [calificacion]


-- 10 hypear serie

medioCalificaciones puntuaciones = (init.tail) puntuaciones

hypear serie | elem 1 (calificaciones serie) = (calificaciones serie)
             |otherwise = [(head (calificaciones serie)) +2] ++ medioCalificaciones(calificaciones serie) ++ [(last(calificaciones serie) +2)]

-- DUDA: ¿como hago para que no se pase de 5?


-------------------------------------------------------------------------------


-- PARTE 2

-- 1 series de monitoschinos

esMonitoChino maraton = filter ((== "Monito chino").genero) maraton

-- 2 originales de netflix y que valen la pena

dameLasQueSonOriginales maraton = filter (esOriginalDeNetflis) maraton

dameOriginalesYValenLaPena maraton = filter (serieValeLaPena) (dameLasQueSonOriginales maraton)

-- 3 series que tengan n cantidad de temporadas

cantidadDeTemporadas n maraton = filter ((==n).cantTemporadas) maraton

-- 4 saber si la maraton es flojita

maratonFlojita maraton = all ((==1).cantTemporadas) maraton

-- 5 cuanto tiempo se tarda en ver un maraton

cuantoTarda maraton = sum (map duracion maraton)

-- 6 nueva forma de saber si un maraton vale la pena

maratonValeLaPena2 maraton = any (serieValeLaPena) maraton || elem rompiendoMalo maraton

-- 7  calificacion mas alta de una serie original de netflix en un maraton

calificacionMasAltaOriginalDeNetflis maraton = maximum (maximum (map calificaciones maraton))


-- 8 hypear serie si corresponde


esDrama serie = (genero serie) == "Drama"

esSuspenso serie = (genero serie) == "Suspenso"

seHypea serie = esDrama serie || esSuspenso serie

hypearSiCorresponde maraton = map hypear (filter seHypea maraton)


-------------------------------------------------------------------------


-- Parte 3

-- 1

-- 1) a) promedio duracion de las series de un maraton ( duracion x cantidad de temporada)

promedioSerieMaraton maraton = (sum (map duracionTotal maraton)) `div` (length maraton)

duracionTotal serie = (duracion serie) * (cantTemporadas serie)

-- 1) b)

calificacionMaraton maraton = (sum (map promediarCalificacion maraton)) `div` (length maraton)

-- 1) c)

maratonesVerano = [maratonAnime, maratonDramatico, maratonPrecoz]

calificacionMaratones maratones = (sum (map calificacionMaraton maratones)) `div` (length maratones)