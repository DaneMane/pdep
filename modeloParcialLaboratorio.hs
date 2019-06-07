data Raton = CRaton {nombre::String, edad::Float, peso::Float, enfermedades::[String]} deriving Show

cerebro = CRaton "Cerebro" 9.0 0.2 ["brucelosis","sarampion","tuberculosis"]

sufijosInfecciosas = ["sis", "itis", "emia", "cocos"]

{-1-}
modificarNombre::(String->String)->Raton->Raton
modificarNombre funcion raton = raton { nombre = (funcion.nombre) raton } 

modificarEdad::(Float->Float)->Raton->Raton
modificarEdad funcion raton = raton { edad = (funcion.edad) raton } 

modificarPeso::(Float->Float)->Raton->Raton
modificarPeso funcion raton = raton { peso = (funcion.peso) raton } 

modificarEnfermedades::([String]->[String])->Raton->Raton
modificarEnfermedades funcion raton = raton { enfermedades = (funcion.enfermedades) raton } 


{-2-}
type Hierba = Raton->Raton

hierbaBuena::Hierba
hierbaBuena = modificarEdad sqrt

hierbaVerde::String->Hierba
hierbaVerde tipo raton = modificarEnfermedades (removerEnfermedades tipo) raton

removerEnfermedades::String->[String]->[String]
removerEnfermedades tipo enfermedades = filter (tipoDistinto tipo) enfermedades

tipoDistinto::String->String->Bool
tipoDistinto terminacion enfermedad = ((/=terminacion).drop ((length enfermedad)-(length terminacion)) ) enfermedad

alcachofa::Hierba
alcachofa raton
    | peso raton > 2    = modificarPeso (*0.9) raton
    | otherwise         = modificarPeso (*0.95) raton

hierbaZort::Hierba
hierbaZort raton = raton {nombre = "pinky", edad = 0, enfermedades = []}

{-3-}
type Medicamento = [Hierba]->Raton->Raton
medicamento::Medicamento
medicamento hierbas raton = foldr ($) raton hierbas 

-- medicamento hierbas raton = foldr (\ -> ) raton hierbas 

pondsAntiAge::Raton->Raton
pondsAntiAge raton = medicamento (((++[alcachofa]).(take 3).repeat) hierbaBuena) raton
