Algoritmo EncontrarMaximosConNombre
	
	Definir cantNumeros, maximo, minimo, i como Numero
	Definir nombreMaximo, nombreMinimo como Cadena
	
	// Inicio Maximos y Minimos
	maximo = -9999;
	minimo = 9999;
	Escribir "Ingrese cu�ntos numeros se van a comparar"
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
	Escribir "Ingrese n�mero a comparar"
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
	Escribir "El alumno que ingres� el M�NIMO total entre todos los ", cantNumeros, " n�meros, es ", nombreMinimo," que eligi�: ", minimo
	Escribir "El alumno que ingres� el M�XIMO total entre todos los ", cantNumeros, " n�meros, es ", nombreMaximo," que eligi�: ", maximo
FinFuncion
	