Algoritmo EncontrarMaximosConNombre
	
	Definir cantNumeros, maximo, minimo, i como Numero
	Definir nombreMaximo, nombreMinimo como Cadena
	
	// Inicio Maximos y Minimos
	maximo = -9999;
	minimo = 9999;
	Escribir "Ingrese cuántos numeros se van a comparar"
	Leer cantNumeros
	// Proceso Maximo y Minimos
	Para i <- 1 hasta cantNumeros Con Paso 1
		ingresarDatos(nombreMinimo, nombreMaximo, minimo, maximo)
	FinPara
	//Final Maximo y Minimos
	mostrarMaximos(nombreMinimo, nombreMaximo, minimo, maximo, cantNumeros)
FinAlgoritmo

funcion ingresarDatos(nombreMin Por Referencia, nombreMax Por Referencia, min Por Referencia, max Por Referencia)
	Definir numCiclo como numero
	Definir nombreCiclo como cadena
	Escribir "Ingrese nombre del Alumno"
	Leer  nombreCiclo
	Escribir "Ingrese número a comparar"
	Leer numCiclo
	Si (numCiclo <= min)
		min:= numCiclo
		nombreMin= nombreCiclo
	FinSi
	Si (numCiclo >= max)
		max:= numCiclo
		nombreMax = nombreCiclo
	FinSi
FinFuncion

funcion mostrarMaximos(nombreMinimo, nombreMaximo, minimo, maximo, cantNumeros)
	Escribir "El alumno que ingresó el MÍNIMO total entre todos los ", cantNumeros, " números, es ", nombreMinimo," que eligió: ", minimo
	Escribir "El alumno que ingresó el MÁXIMO total entre todos los ", cantNumeros, " números, es ", nombreMaximo," que eligió: ", maximo
FinFuncion
	