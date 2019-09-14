object camion {
	const cosas = []
	const pesoMax = 2500
	var tara = 1000
	
	method cargar(unaCosa){
		cosas.add(unaCosa)
	}
	
	method descargar(unaCosa){
		cosas.remove(unaCosa)
	}
	
	method pesoTotal(){
		return tara + self.pesoTotalCarga()
	}
	
	method pesoTotalCarga(){
		return cosas.sum({ unaCosa => unaCosa.peso() })
	}
	
	method exedidoDePeso(){
		return self.pesoTotal() > pesoMax
	}
	
	method objetosPeligrosos(nivel){
		return cosas.filter({ coso => coso.nivelPeligrosidad() > nivel})
	}
	
	method objetosMasPeligrososQue(cosa){
		return self.objetosPeligrosos(cosa.nivelPeligrosidad())
	}
	
	method puedeCircularEnRuta(nivelMaxPeligrosidad){
		return cosas.all({ cosa => cosa.nivelPeligrosidad() < nivelMaxPeligrosidad})
	}
	
	method tieneAlgoQuePesaEntre(min,max){
		return cosas.any({ cosa => cosa.peso().between(min,max) })
	}
	
}

object knightRider{
	var property peso = 500
	var property nivelPeligrosidad = 10
}

object bumblebee{
	var property peso = 800
	var property forma = auto
	
	method nivelPeligrosidad(){
		return forma.nivelPeligrosidad()
	} 
}

object auto{
	var property nivelPeligrosidad = 15
}

object robot{
	var property nivelPeligrosidad = 30
}

object paqueteLadrillos{
	var property cantLadrillos
	var property nivelPeligrosidad = 2
	
	method peso() = 2 * cantLadrillos
}

object bateriaAntiaerea{
	var property estaConMisiles
	
	method peso(){
		if(estaConMisiles){
			return 300
		}
		return 200
	}
	
	method nivelPeligrosidad(){
		if(estaConMisiles){
			return 100
		}
		return 0
	}
}

object contenedorPortuario{
	const property cosas = []
	
	method peso(){
		return 100 + self.pesoTotalCosas()
	}
	
	method pesoTotalCosas(){
		return cosas.sum({ cosa => cosa.peso() })
	}
	
	method nivelPeligrosidad(){
		if(cosas.isEmpty()){
			return 0
		}
		return self.nivelCosaMasPeligrosa()
	}
	
	method nivelCosaMasPeligrosa(){
		return cosas.max({ cosa => cosa.nivelPeligrosidad() }).nivelPeligrosidad()
	}
	
}

object embalajeDeSeguridad{
	var property contenido = contenedorPortuario
	
	method peso() = contenido.peso()
	
	method nivelPeligrosidad() = contenido.nivelPeligrosidad()/2
	
}



