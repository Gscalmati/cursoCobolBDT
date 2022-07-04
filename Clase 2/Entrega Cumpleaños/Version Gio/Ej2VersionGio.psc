Algoritmo Ej2VersionGio
	definir cantRecaudada como Entero
	definir cantTotal como Entero
	iniciar(cantTotal)
	ingresar(cantRecaudada)
	Mientras (cantRecaudada <> 0)
	sumar(cantRecaudada, cantTotal)
	ingresar(cantRecaudada)
	FinMientras
	show(cantTotal)
FinAlgoritmo

Funcion iniciar(var)
		var = 0
FinFuncion

Funcion ingresar(var)
	Escribir "Cuánto querés ingresar?"
	Leer var
FinFuncion

Funcion sumar(parcial, total Por Referencia)
	total = total + parcial
FinFuncion

Funcion show(total)
	Escribir "La cantidad recaudada es ", total
FinFuncion
	