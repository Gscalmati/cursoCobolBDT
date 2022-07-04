Algoritmo sin_titulo
	Definir importeIngresado Como Real
	Definir importeRecaudado Como Real
	definir i Como Entero
	
	importeIngresado = 0
	importeRecaudado = 0
	i = 0
	
	Para i<-0 Hasta 28 Con Paso 1 Hacer
		Repetir
			Escribir "Ingrese el importe que desea colaborar:"
			Leer importeIngresado
			Si importeIngresado < 0 Entonces
				Escribir "no puede ingresar un valor negativo"
			SiNo
				importeRecaudado = importeIngresado + importeRecaudado
			Fin Si
		Hasta Que importeIngresado >= 0
	Fin Para
	
	Escribir importeRecaudado
	fin Algoritmo
