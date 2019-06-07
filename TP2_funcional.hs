import Text.Show.Functions

data Gimnasta = Gimnasta{nombre::String, nivelDeEnergia::Float, nivelDeEquilibrio::Int, flexibilidad::Float, nivelDeFuerzaFisica::Float , habilidades::[Ejercicio]} deriving Show

medialuna::Ejercicio
medialuna gimnasta = gimnasta {nivelDeEquilibrio = practicar (nivelDeEquilibrio gimnasta) 5 }

rolAdelante::Float->Ejercicio
rolAdelante velocidad gimnasta  = gimnasta {nivelDeEnergia = practicar (nivelDeEnergia gimnasta) (velocidad/2) }

vertical::Ejercicio
vertical gimnasta = gimnasta {nivelDeFuerzaFisica = practicar (nivelDeFuerzaFisica gimnasta) 7 }

saltoConSoga::Float->Ejercicio
saltoConSoga saltos gimnasta | saltos >= 4 = saltosSignificativos gimnasta saltos
                             | otherwise   = gimnasta

saltosSignificativos::Gimnasta->Float->Gimnasta
saltosSignificativos gimnasta saltos = gimnasta{nivelDeFuerzaFisica = practicar (nivelDeFuerzaFisica gimnasta) saltos , nivelDeEnergia = max 0 (practicar (nivelDeEnergia gimnasta) (-saltos/2)) }

saltoMortal::Float->Float->Ejercicio
saltoMortal altura impulso gimnasta = gimnasta{nivelDeFuerzaFisica = practicar (nivelDeFuerzaFisica gimnasta) altura, flexibilidad = practicar (flexibilidad gimnasta) (impulso/2)}


practicar:: Num a => a -> a -> a
practicar habilidad incremento = habilidad + incremento

--Punto 1)--
sonia = Gimnasta{ nombre="Sonia", nivelDeEnergia=3, nivelDeEquilibrio=6, flexibilidad=10, nivelDeFuerzaFisica=5, habilidades = [medialuna, rolAdelante 30, vertical] }

pedro = Gimnasta{ nombre="Pedro", nivelDeEnergia=7, nivelDeEquilibrio=5, flexibilidad=5, nivelDeFuerzaFisica=6 , habilidades = [saltoConSoga 50, saltoMortal 2 8] }

--Punto 2)--
--a)
ejercitar::Int->Ejercicio->Ejercicio
ejercitar t ejercicio gimnasta  |  t >= 2 = ejercitar (t-2) ejercicio (ejercicio gimnasta)
                                |  otherwise = gimnasta { habilidades = ( (habilidades gimnasta) ++ [ejercicio] ) }

--b)
data Rutina = Rutina{repeticiones::Integer, ejercicios::[Ejercicio]} deriving Show

entradaEnCalor = Rutina{repeticiones=2, ejercicios=[rolAdelante 10, rolAdelante 10, medialuna, saltoConSoga 50, saltoMortal 20 15]}
rutinaDiaria = Rutina{repeticiones=3, ejercicios=[rolAdelante 20, saltoConSoga 30, vertical, medialuna, saltoConSoga 10]}

--c)
type Ejercicio = Gimnasta->Gimnasta


entrenar :: Rutina -> Gimnasta -> Gimnasta
entrenar rutina gimnasta | repeticiones rutina > 0 = entrenar (rutina { repeticiones = repeticiones rutina - 1 }) (realizarEjercicios (ejercicios rutina) gimnasta)
                         | otherwise = gimnasta

realizarEjercicios :: [Ejercicio] -> Gimnasta -> Gimnasta
realizarEjercicios (ejercicio:ejercicios) gimnasta = foldl1 ((.)) (ejercicios) gimnasta

--d)
type Nivel = Float

tienenPotencial :: [Gimnasta] -> Nivel -> [Gimnasta]
tienenPotencial gimnastas nivel = filter (tienePotencial nivel) gimnastas

tienePotencial ::  Nivel -> Gimnasta -> Bool
tienePotencial nivel gimnasta = nivel < (nivelFortaleza.fortalecer) gimnasta

nivelFortaleza :: Gimnasta -> Nivel
nivelFortaleza gimnasta = nivelDeFuerzaFisica gimnasta + nivelDeEnergia gimnasta

--fortalecer devuelve el gimnasta luego de haber realizado la rutina diaria y sus habilidades
fortalecer::Ejercicio
fortalecer gimnasta = (entrenar Rutina{repeticiones=1, ejercicios=(habilidades gimnasta)} . entrenar rutinaDiaria) gimnasta


--Punto 3)
--a)
maximoSegun::(Ord a)=>(Gimnasta->a)->[Gimnasta]->String
maximoSegun condicion listaGimnastas = (nombre.maximoGimnastaSegun condicion) (map (entrenar rutinaDiaria) listaGimnastas)


maximoGimnastaSegun::(Ord a)=>(Gimnasta->a)->[Gimnasta]->Gimnasta
maximoGimnastaSegun _ [gimnasta] = gimnasta
maximoGimnastaSegun condicion (cabeza:cola) | condicion cabeza >= condicion (maximoGimnastaSegun condicion cola) = cabeza
                                            | otherwise = maximoGimnastaSegun condicion cola

--b)

minimoEntre::(Ord a)=> (Gimnasta->a) -> (Gimnasta->a) -> Gimnasta -> a
minimoEntre campo1 campo2 gimnasta = min (campo1 gimnasta) (campo2 gimnasta)

minimoEntreFlexYFuerza gimnasta = minimoEntre flexibilidad nivelDeFuerzaFisica gimnasta


--c)
contarHabilidades::Gimnasta->Int
contarHabilidades gimnasta = (length.habilidades) gimnasta

cantHabilidadesPostEjercitar::Ejercicio->Gimnasta->Int
cantHabilidadesPostEjercitar ejer gimnasta  = contarHabilidades (ejercitar 10 ejer gimnasta)

maximoXCantHabilidades::[Gimnasta]->Ejercicio->String
maximoXCantHabilidades listaGimnastas ejer = maximoSegun (cantHabilidadesPostEjercitar ejer) (map (entrenar rutinaDiaria) listaGimnastas)

--Punto 4) 
h::(Eq b)=> b->(a->b)->[a]->Bool
h e g = any((\x->x).(==e)).map g

--Punto 5)
--a) No se puede verificar si ya lo aprendió, ya que los ejercicios son funciones y no podemos preguntar igualdad de funciones.

--b) Es posible, gracias al concepto Lazy Evaluation. Esto nos permite evaluar los argumentos a medida que se van necesitando.

--listaInfinita genera una lista infinita de socios a partir del nro ingresado como parametro n
listaInfinita n = (("socio"++).show) n : listaInfinita (n+1)

-- *Main> h "socio4" (++"") (listaInfinita 1)
-- True
-- *Main> h "socio4" (++"") (listaInfinita 5)
-- Interrupted.

