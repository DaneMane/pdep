/* Busqueda Laboral */
/* Base de conocimiento */

contador(roque).
joven(roque).
trabajoEn(roque,acme).
habla(roque,frances).
honesto(roque).
ingeniero(ana).
habla(ana,ingles).
habla(ana,frances).
trabajoEn(ana,omni).
habla(lucia,ingles).
habla(lucia,frances).
trabajoEn(lucia,omni).
abogado(cecilia).
ambicioso(cecilia).
habla(cecilia,frances).

ambicioso(Persona):- contador(Persona), joven(Persona).

tieneExperiencia(Persona):- trabajoEn(Persona,_).

profesional(Persona):- contador(Persona).
profesional(Persona):- abogado(Persona).
profesional(Persona):- ingeniero(Persona).

/* 
    1)
    puedeAndar(sector,Persona)
*/
puedeAndar(contaduria, Persona):- contador(Persona), honesto(Persona).
puedeAndar(comercioExterior, Persona):- ambicioso(Persona).
puedeAndar(ventas, Persona):- ambicioso(Persona), tieneExperiencia(Persona).

puedeAndar(ventas,lucia).

puedeAndar(sector,roque).

/* 
    2)
*/

puedeAndar(proyectos, Persona):- ingeniero(Persona), tieneExperiencia(Persona).
puedeAndar(proyectos, Persona):- abogado(Persona), joven(Persona).

puedeAndar(logistica,Persona):- profesional(Persona),cumpleCondicion(Persona).
cumpleCondicion(Persona):- joven(Persona).
cumpleCondicion(Persona):- trabajoEn(Persona, omni).

/*
    3)
*/

ingeniero(carlos).
trabajoEn(carlos,fiat).
ambicioso(carlos).

/* Relaciones Familiares */
/* Base de conocimiento */
madre(mona, homero).
madre(jaqueline, marge).
madre(marge, maggie).
madre(marge, bart).
madre(marge, lisa).
padre(abraham, herbert).
padre(abraham, homero).
padre(clancy, jaqueline).
padre(homero, maggie).
padre(homero, bart).
padre(homero, lisa).

/* 
    1)
*/

hermano(Persona, otraPersona):- mismoPadre(Persona, otraPersona), mismaMadre(Persona, otraPersona).

mismoPadre(Persona, otraPersona):- padre(Padre, Persona), padre(Padre, otraPersona), Persona \= otraPersona.

mismaMadre(Persona, otraPersona):- madre(Madre, Persona), madre(Madre,otraPersona), Persona \= otraPersona.



