Algoritmo Validacion2Variables
	//Inicio Validacion
	Definir ingreso Como Caracter
	Definir opc1, opc2 Como Caracter
	Definir validacion Como Logico
	//Inicio Validacion
	// Asignar a opc1 y opc2 los valores que correspondan a validar
	opc1 := "M"
	opc2 := "F"
	// Proceso Validacion
	Escribir "Ingrese un caracter a validar entre estas opiones: ", opc1, " y ", opc2
	Leer ingreso
	validarIngreso(ingreso,opc1,opc2)
FinAlgoritmo

funcion validarIngreso(ingreso, parametro1, parametro2)
	Definir ingresoCorrecto Como Logico
	Mientras (ingreso <> parametro1 y ingreso <> parametro2)
		Escribir "ERROR. Ingrese un caracter a validar ENTRE las siguientes opciones: ", parametro1, " y ", parametro2
		Leer ingreso
	FinMientras
	Escribir "El caracter ingresado es ", ingreso
	FinFuncion
	