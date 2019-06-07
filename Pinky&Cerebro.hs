{-1-}

data Animal = Animal { iq::Int , especie::String, habilidades::[String]} deriving Show

pinky::Animal
pinky = Animal {
    iq = 5,
    especie = "raton",
    habilidades = ["hacer nerd","hacer sup","hacer qwas"]
}

cerebro::Animal
cerebro = Animal {
    iq = 9001,
    especie = "raton",
    habilidades = ["dominar al mundo"]
}

dumbo::Animal
dumbo = Animal {
    iq = 15,
    especie = "elefante",
    habilidades = []
}

{-2-}

type Transformacion = Animal->Animal
inteligenciaSuperior::Int->Transformacion
inteligenciaSuperior n animal = animal { iq = iq animal + n } 

pinkificar::Transformacion
pinkificar animal = animal { habilidades = [] }

superpoderes::Transformacion
superpoderes animal 
    | especie animal == "elefante" = animal { habilidades = habilidades animal ++ ["no tenerle miedo a los ratones"] }
    | especie animal == "raton" && iq animal >= 100 = animal { habilidades = habilidades animal ++ ["hablar"] }
    | otherwise = animal

{-3-}

type CriterioExito = Animal->Bool
antropomorfico::CriterioExito
antropomorfico animal = elem "hablar" (habilidades animal) && (iq animal) > 60

noTanCuerdo::CriterioExito
noTanCuerdo animal = habilidadesPinkiescas animal > 2

habilidadesPinkiescas::Animal->Int
habilidadesPinkiescas animal = (length.filter habilidadPinkiesca) (habilidades animal)

habilidadPinkiesca::String->Bool
habilidadPinkiesca habilidad = ((=="hacer").take 5) habilidad && (palabraPinkiesca.drop 6) habilidad

palabraPinkiesca::String->Bool
palabraPinkiesca palabra = length palabra <= 4 && ((length.filter esVocal) palabra) >= 1

esVocal::Char->Bool
esVocal letra = elem letra "aeiouAEIOU"  