{- 1er Parcial PDEP -> Paradigma Funcional -}

{- 1 -}
--a)
import Text.Show.Functions --Post Parcial

data Paciente = Paciente{
    nombre::String,
    edad::Int,
    vitalidad::Float,
    temperatura::Float,
    enfermedades::[Paciente->Paciente]
} deriving Show

--b)

sonia = Paciente "Sonia" 20 200 38 [estres 10, neumonia]
pedro = Paciente "Pedro" 35 250 36.5 [gripe 'A']

{- 2 -}
--a)

type Enfermedad = Paciente->Paciente

gripe::Char->Enfermedad
gripe tipo paciente 
    | tipo == 'A' = gripeA paciente
    | tipo == 'B' = gripeB paciente
    | otherwise   = gripeC paciente

gripeA::Enfermedad
gripeA paciente = paciente { vitalidad = (vitalidad paciente)/2 }

gripeB::Enfermedad
gripeB paciente = paciente { temperatura = (temperatura paciente) +10 }

gripeC::Enfermedad
gripeC paciente 
    | edad paciente < 10 = paciente { vitalidad = (vitalidad paciente)*0.8 }
    | otherwise = paciente

--Mal, puse min
estres::Int->Enfermedad
estres nivel paciente = paciente { vitalidad = max 0 ((vitalidad paciente) - fromIntegral(20*nivel) ) }

--Mal, puse max
neumonia::Enfermedad
neumonia paciente = paciente { temperatura = min 40 ((temperatura paciente) * 1.3 ) }

--b)
afectar::Paciente->[Enfermedad]->Paciente
-- afectar paciente listEnfermedades = foldr ($) paciente listEnfermedades
afectar = foldr ($)

--c)
--Mal, composicion
seContagioDe::Paciente->Paciente->Paciente
seContagioDe paciente1 paciente2 = ((agregarEnfermedades (enfermedades paciente2)).(afectar paciente1).enfermedades) paciente2

agregarEnfermedades::[Enfermedad]->Paciente->Paciente
agregarEnfermedades listEnfermedades paciente = paciente { enfermedades = (enfermedades paciente) ++ listEnfermedades }

--d)

seContagioMal::Paciente->[Paciente]->Paciente
--Mal, no use lambda
seContagioMal paciente pacientes = foldr1 (\ p1 ps -> seContagioDe p1 ps) pacientes

{- 3 -}
--a)
type Sustancia = Paciente->Paciente

paracetamol::Sustancia
paracetamol paciente = paciente { temperatura = min 37 (temperatura paciente) }

gingseng::Float->Sustancia
gingseng gramos paciente = paciente { vitalidad = (vitalidad paciente) + 0.1*gramos }

magnesio::Sustancia
magnesio paciente = paciente { vitalidad = (vitalidad paciente) *1.3 }

--b)
--Mal, type sustancias y falto deriving Show
data Receta = Receta { nombreReceta::String, sustancias::[Sustancia]} deriving Show

--c)
energizante = Receta "Energizante" [gingseng 50, magnesio]

--d)
pacientesFuertes::Int->Receta->[Paciente]->[Paciente]
--Mal, faltaba fromIntegral
pacientesFuertes nivel receta pacientes = filter ((>fromIntegral(nivel)).vitalidad) (pacientesAfectados pacientes receta)

pacientesAfectados::[Paciente]->Receta->[Paciente]
pacientesAfectados pacientes receta = map ((consumir receta).gripeA) pacientes

consumir::Receta->Paciente->Paciente
consumir receta paciente = foldr ($) paciente (sustancias receta)

--e)
todosCurados::Receta->[Paciente]->Bool
--Mal, composicion
todosCurados receta pacientes = all (estaCurado.(consumir receta)) pacientes

estaCurado::Paciente->Bool
estaCurado paciente = vitalidad paciente > 70 && temperaturaNormal paciente

temperaturaNormal::Paciente->Bool
temperaturaNormal paciente = temperatura paciente < 37 && temperatura paciente > 35

{- 4 -}
--a)
--Mal, puse Paciente->Paciente pero asi no es ordenable
type Criterio = Paciente->Float

ordenadoSegun::Criterio->Receta->[Paciente]->Bool
ordenadoSegun _ _ [_] = True
ordenadoSegun criterio receta (p1:p2:ps)
    | ((criterio).(consumir receta)) p1 > ((criterio).(consumir receta)) p2 = ordenadoSegun criterio receta (p2:ps)
    | otherwise = False

criterio4a::Criterio
--Mal, faltaba fromIntegral
criterio4a paciente = max ((temperatura.gripeA) paciente) ((fromIntegral.edad.gripeA) paciente)

--b)
criterio4b::Criterio
--Mal, faltaba fromIntegral, toInteger y round
criterio4b paciente =  fromIntegral(mod ((toInteger.length.enfermedades) paciente) ((round.vitalidad) paciente))

{- 5 -}
--a)
{- 

*Main> ordenadoSegun criterio4a energizante [pedro,sonia]
False
*Main>

*Main> ordenadoSegun criterio4a energizante [sonia,pedro]
True
*Main>

-}

--b)
{- 

*Main> ordenadoSegun criterio4b energizante [pedro,sonia]
False
*Main>

*Main> ordenadoSegun criterio4b energizante [sonia,pedro]
True
*Main>

-}

{- 6 -}
{- No es posible consultar la igualdad entre dos funciones. Por eso es imposible -}

{- 7 -}
{- Por el concepto de lazy evaluation, el interprete quedará evaluando si estan ordenados 
hasta que encuentre un elemento que no cumpla con el criterio de orden. Es decir que podrá 
dar falso si encuentra algun elemento desordenado o quedara evaluando infinitamente.
No puede ser verdadero, ya que una lista infinita nunca "matcheará" con la condicion que 
devuelve verdadero -}