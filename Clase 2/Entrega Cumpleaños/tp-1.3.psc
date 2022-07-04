Algoritmo sin_titulo
	Definir importeIngresado Como Real
	Definir importeRecaudado Como Real
	Definir contador Como Entero
	Definir respuestaAlumno Como Caracter
	importeIngresado <- 0
	importeRecaudado <- 0
	contador <- 0
	respuestaAlumno <- ''
	Repetir
		Escribir 'Ingrese el importe con el cual desea colaborar:'
		Leer importeIngresado
		Si importeIngresado>0 Entonces
			importeRecaudado <- importeIngresado+importeRecaudado
			Repetir
				Escribir 'Desea seguir colaborando: SI/NO'
				Leer respuestaAlumno
				Si respuestaAlumno<>'si' Y respuestaAlumno<>'no' Entonces
					Escribir 'su respuesta tiene que ser SI o NO'
				FinSi
			Hasta Que respuestaAlumno=='si' O respuestaAlumno=='no'
			contador <- 1
		SiNo
			Escribir 'no puede ingresar un importe negativo'
		FinSi
		Si contador<1 Entonces
			Escribir 'almenos un integrante debe colaborar:'
		FinSi
	Hasta Que contador>0 Y respuestaAlumno='no'
	Escribir importeRecaudado
FinAlgoritmo
