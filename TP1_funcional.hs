data Gimnasta = Gimnasta{nombre::String, nivelDeEnergia::Float, nivelDeEquilibrio::Int, flexibilidad::Float, nivelDeFuerzaFisica::Float } deriving Show

medialuna::Gimnasta->Gimnasta
medialuna gimnasta = gimnasta {nivelDeEquilibrio = practicar (nivelDeEquilibrio gimnasta) 5 }

rolAdelante::Float->Gimnasta->Gimnasta
rolAdelante velocidad gimnasta  = gimnasta {nivelDeEnergia = practicar (nivelDeEnergia gimnasta) (velocidad/2) }

vertical::Gimnasta->Gimnasta
vertical gimnasta = gimnasta {nivelDeFuerzaFisica = practicar (nivelDeFuerzaFisica gimnasta) 7 }

saltoConSoga::Float->Gimnasta->Gimnasta
saltoConSoga saltos gimnasta | saltos >= 4 = saltosSignificativos gimnasta saltos
                             | otherwise   = gimnasta

saltosSignificativos::Gimnasta->Float->Gimnasta
saltosSignificativos gimnasta saltos = gimnasta{nivelDeFuerzaFisica = practicar (nivelDeFuerzaFisica gimnasta) saltos , nivelDeEnergia = max 0 (practicar (nivelDeEnergia gimnasta) (-saltos/2)) }

saltoMortal::Float->Float->Gimnasta->Gimnasta
saltoMortal altura impulso gimnasta = gimnasta{nivelDeFuerzaFisica = practicar (nivelDeFuerzaFisica gimnasta) altura, flexibilidad = practicar (flexibilidad gimnasta) (impulso/2)}


practicar:: Num a => a -> a -> a
practicar habilidad incremento = habilidad + incremento


sonia = Gimnasta{ nombre="Sonia", nivelDeEnergia=3, nivelDeEquilibrio=6, flexibilidad=10, nivelDeFuerzaFisica=5 }

pedro = Gimnasta{ nombre="Pedro",nivelDeEnergia=7, nivelDeEquilibrio=5, flexibilidad=5, nivelDeFuerzaFisica=6 }

--4a1 > ((medialuna).((saltoMortal 20) 15).(rolAdelante 10).(rolAdelante 10).(saltoConSoga 10)) sonia
--4a2 > ((saltoConSoga 4).(rolAdelante 15).vertical) pedro

--4b > El concepto que interviene en el punto 4a es composición de funciones.