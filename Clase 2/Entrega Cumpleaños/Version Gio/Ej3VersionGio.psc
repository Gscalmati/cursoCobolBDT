Algoritmo Ej3VersionGio
	Definir cantRecaudada Como Entero
	Definir cantTotal Como Entero
	Definir string Como cadena
	cantTotal <- 0
	Escribir 'Cu�nto quer�s ingresar?'
	Leer cantRecaudada
	Repetir
		cantTotal <- cantTotal+cantRecaudada
		Escribir "Hay alguien m�s que quiera aportar? Si/No"
		Leer string
		Si (string <> "Si" y string <> "No")
			Escribir 'Inserte Si o No'
			Leer string
		SiNo
		Si (string = "Si")
			Escribir 'Cu�nto quer�s ingresar?'
			Leer cantRecaudada
		FinSi
	FinSi
	Hasta Que string = "No"
	Escribir 'La cantidad recaudada es ',cantTotal
FinAlgoritmo
