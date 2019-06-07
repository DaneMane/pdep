{- 1) Obtener la energía que produce un grito, que se calcula como el nivel de terror por la intensidad al cuadrado,
 en caso de que moje la cama, si no, sólo es el triple del nivel de terror más la intensidad. El nivel de terror es equivalente
 al largo de la onomatopeya. 

De los gritos sabemos que están representados por una tupla (Onomatopeya, Intensidad, mojóLaCama).

Ejemplo:
energiaDeGrito ("AAAAAHG", 10, True)
700
energiaDeGrito ("uf", 2, False)
8 -}


type Grito = (String,Int,Bool)

onomatopeya (o,_,_) = o
intensidad (_,i,_) = i
mojoLaCama (_,_,m) = m

energiaDeGrito :: Grito -> Int
energiaDeGrito grito
        | mojoLaCama grito = nivelDeTerror grito * (intensidad grito) ^ 2
        | otherwise = 3 * nivelDeTerror grito + intensidad grito

nivelDeTerror::Grito->Int
nivelDeTerror = length.onomatopeya


{- 2) Obtener el grito que cada monstruo (un empleado de la empresa) le arranca a un niño (su víctima). 
Hay muchos monstruos y cada uno tiene su forma de trabajar, que consiste precisamente en recibir a un niño y devolver su grito.
 De los niños se sabe el nombre, la edad y la altura y se los representa con una tupla con dichas componentes.

Monstruos en la empresa hay muchos. En particular, se desea implementar los siguientes, pero podría haber más:
Sullivan (el protagonista) produce un grito "AAAGH" con tantas A como letras tenga el nombre del niño y una intensidad como como 20 / edad; 
hace mojar la cama si el niño tiene menos de 3 años.
Randall Boggs (la lagartija) produce un grito de “¡Mamadera!”, con tanta intensidad como vocales en el nombre de la persona; 
hace mojar la cama en los niños que miden entre 0,8 y 1,2 de altura.
Chuck Norris produce siempre un grito que dice todo el abecedario, con 1000 de nivel de intensidad y siempre hace que mojen la cama.
Un Osito Cariñoso produce un grito “uf”, con un nivel de intensidad igual a la edad del niño y nunca moja la cama.
 -}


type Victima = (String, Int, Float) -- nombre, edad, altura

nombre (n,_,_) = n
edad (_,e,_) = e
altura (_,_,a) = a

type Monsturo = Victima->Grito

sullivan::Monsturo
sullivan victima = (gritoGenerado ,intensidadGenerada, haceMojarLaCama)
    where gritoGenerado      = take ((length.nombre) victima) (repeat 'A') ++"GH"
          intensidadGenerada = div 20 (edad victima)
          haceMojarLaCama    = (edad victima) < 3


randall::Monsturo
randall victima = ("Mamadera!" ,intensidadGenerada, haceMojarLaCama)
    where intensidadGenerada = (length . filter esVocal . nombre) victima
          haceMojarLaCama    = altura victima >= 0.8 && altura victima <= 1.2

esVocal letra = elem letra "aeiouAEIOU"

chuckNorris::Monsturo
chuckNorris victima = (['A'..'Z'] ,1000, True)


ositoCarinioso::Monsturo
ositoCarinioso victima = ("uf" ,edad victima, False)



{- 3) Hacer una función que reciba una lista de funciones y un elemento, y que devuelva la lista que resulta de aplicar cada función 
sobre el elemento. Definir dominio e imagen.
Por ejemplo:
pam [doble, triple, raizCuadrada] 9
[18, 27, 3]
 -}

pam:: [a->b] -> a -> [b] 
pam funciones elemento = (map ($ elemento) funciones)
-- pam funciones valor = map (\ f -> f valor) funciones



{- 4) Los monstruos a veces trabajan en equipo, por lo que van varios a la casa de un niño y todos lo asustan. Obtener el conjunto de gritos que logra el equipo.
Por ejemplo:
gritos [sullivan, osito, chuck] (“kevin”, 2, 1.1) 
[("AAAAAHG", 10, True), (“uf”,2,False), (“abcdefghijklmnopqrstuvwxyz”,1000,True)]
 -}

gritos::[Monsturo]->Victima->[Grito]
gritos = pam 



{- 5) Saber la producción energética de un equipo de monstruos que va a un campamento y asusta a todos los niños que están durmiendo en las carpas. Asumir que los campamentos son listas de niños y ya están definidos como  constantes.
Por ejemplo:  
produccionEnergeticaGritos [sullivan, osito, chuck] campamentoDeExploradores
999999
 -}

produccionEnergeticaGritos::[Monsturo]->[Victima]->Int
produccionEnergeticaGritos monstruos victimas = (sum . (map energiaDeGrito) . aplanar . map (gritos monstruos) ) victimas

aplanar::[[a]]->[a]
aplanar = foldl (++) []

{- 6) Ante la declinación en la producción energética de Monsters inc debido a las nuevas generaciones que ya no se asustan de monstruos obsoletos, la empresa ha decidido poner en marcha un proyecto de transformación de risas en energía,
 para lo que contrató a comediantes.
Las risas se representan por tuplas en las que se indica la duración y la intensidad. La energía que producen es la duración elevada a la intensidad. Cada comediante recibe un niño y devuelve una risa.
Por ejemplo:
Capusotto produce una risa de duración igual al doble de la edad del niño y la misma intensidad.

Hacer la función produccionEnergeticaRisas que devuelve el total de energía producido por enviar a un equipo de comediantes a un campamento.  Si es necesario, hacer funciones auxiliares pero no modificar las ya hechas.
 -}

type Risa = (Int,Int)

duracionRisa (d,_) = d
intensidadRisa (_,i) = i 

type Comediante = Victima -> Risa

capussotto::Comediante
capussotto victima = (2 * edad victima , intensidad victima ) 

energiaDeRisa::Risa->Int
energiaDeRisa risa = duracionRisa risa ^ intensidadRisa risa

risas::[Comediante]->Victima->[Risa]
risas = pam 


produccionEnergeticaRisas::[Comediante]->[Victima]->Int
produccionEnergeticaRisas comediantes victimas = (sum . (map energiaDeRisa) . aplanar . map (risas comediantes) ) victimas

{- 7) Hacer una única función produccionEnergetica que reemplace a produccionEnergeticaRisas y a produccionEnergeticaGritos y que aplicada con los argumentos adecuados permita obtener lo mismo que ellas, 
es decir, que reciba un conjunto de monstruos o un conjunto de comendiantes y devuelva la energía producida.
      
 -}


--produccionEnergetica empleados victimas = (sum . (map energia) . aplanar . map (extracto empleados) ) victimas

--Pattern Matching?