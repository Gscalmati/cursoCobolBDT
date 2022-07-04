Algoritmo sin_titulo
	Definir i , codVendIng Como Entero
	Definir fechIng Como Cadena
	Definir montoIng Como Real
	//variables general
	Definir genFilMaxVentas, genMaxVentas Como Entero
	//variables filial
	Definir filFechMaxVentas,filContVentas Como Entero
	Definir filFechMaximo Como Cadena
	//variables fecha
	Definir fechCodVendMaxVentas, fechContVentas, fechCodVendedorMaxMonto, fechMaxVentas Como Entero
	Definir fechMaxMonto Como Real
	//variables vendedor
	Definir venCodAnterior,venContVentas Como Entero
	Definir venContMonto Como Real
	
	//inicioGeneral
	inicioGeneral(genFilMaxVentas, genMaxVentas)
	Para i <- 1 Hasta 3 Hacer
		//inicioFilial
		inicioFilial(i,filFechMaxVentas,filContVentas,filFechMaximo)
		Repetir
			//inicioFecha
			inicioFecha(fechIng,fechMaxVentas,fechMaxMonto,fechContVentas,fechCodVendMaxVentas, fechCodVendedorMaxMonto)
			//ingresoCodigoVendedor
			ingresoCodigoVendedor(codVendIng)
			Mientras codVendIng <> 0 Hacer
				//inicioVendedor
				inicioVendedor(codVendIng,venCodAnterior,venContVentas,venContMonto)
				Mientras  codVendIng = venCodAnterior Hacer
					//ingresoDatosRestantes
					ingresoDatosRestantes(montoIng)
					//procesoUnVendedor
					procesoUnVendedor(montoIng,venContVentas,venContMonto)
					//ingresoCodigoVendedor
					ingresoCodigoVendedor(codVendIng)
				FinMientras
				//finVendedor
				finVendedor(venCodAnterior,venContMonto,venContVentas,fechCodVendMaxVentas, fechCodVendedorMaxMonto, fechMaxVentas, fechMaxMonto,fechContVentas,filContVentas)
			FinMientras
			finFecha(fechIng,fechCodVendMaxVentas, fechCodVendedorMaxMonto, fechMaxVentas,fechMaxMonto,fechContVentas,filFechMaxVentas,filFechMaximo)
		Hasta Que preguntarHayFecha() = 'N' 
		//finFilial
		finFilial(i,filFechMaxVentas,filFechMaximo,filContVentas,genFilMaxVentas, genMaxVentas)
	FinPara
	//finGeneral
	finGeneral(genFilMaxVentas, genMaxVentas)
FinAlgoritmo

// *************************************************************************
// FUNCION : exito <- esNumero(caract)
// PARAMETROS : caract
// ACCION : determina si el caracter es un número
// DEVUELVE : exito --> booleano
// *************************************************************************
Funcion exito <- esNumero(caract)
	Definir exito Como Logico
	exito <- (caract >='0' & caract <='9')
FinFuncion
// *************************************************************************
// FUNCION : aumentoVariable(cant, variable Por Referencia)
// PARAMETROS : cant, variable Por Referencia
// ACCION : sumo lo que tengo en la variable "variable" lo que aloja cant
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion aumentoVariable(cant, variable Por Referencia)
	variable <- variable + cant
FinFuncion
// *************************************************************************
// FUNCION : buscoMaxYAsigno(venCodAnterior,venCant,venCodCantVentas Por Referencia,venCantVent Por Referencia)
// PARAMETROS : venCodAnterior,venCant,venCodCantVentas Por Referencia,venCantVent Por Referencia
// ACCION : busco el máximo valor y asigno el responsable del máximo
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion buscoMaxYAsigno(venCodAnterior,venCant,venCodCantVentas Por Referencia,venCantVent Por Referencia)
	Si venCant > venCantVent Entonces
		venCantVent <- venCant
		venCodCantVentas <- venCodAnterior
	FinSi
FinFuncion
// *************************************************************************
// FUNCION : inicializoVariablesVendedor(venContVentas Por Referencia,venContMonto Por Referencia)
// PARAMETROS : venContVentas Por Referencia,venContMonto Por Referencia
// ACCION : inicializa las variables venContVentas, venContMonto en 0
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion inicializoVariablesVendedor(venContVentas Por Referencia,venContMonto Por Referencia)
	venContVentas <- 0
	venContMonto <- 0
FinFuncion
// *************************************************************************
// FUNCION : inicializoVariablesFilial(filFechMaxVentas Por Referencia,filContVentas Por Referencia)
// PARAMETROS : filFechMaxVentas Por Referencia,filContVentas Por Referencia
// ACCION : inicializo las variables filFechMaxVentas, filContVentas en 0
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion inicializoVariablesFilial(filFechMaxVentas Por Referencia,filContVentas Por Referencia,filFechMaximo Por Referencia)
	filFechMaxVentas <- 0
	filContVentas <- 0
	filFechMaximo <- "0"
FinFuncion
// *************************************************************************
// FUNCION : inicializoVariablesFecha(fechMaxVentas Por Referencia,fechMaxMonto Por Referencia,fechContVentas Por Referencia)
// PARAMETROS : fechMaxVentas Por Referencia,fechMaxMonto Por Referencia,fechContVentas Por Referencia
// ACCION : inicializa las variables fechMaxVentas, fechMaxMonto, fechContVentas en 0
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion inicializoVariablesFecha(fechMaxVentas Por Referencia,fechMaxMonto Por Referencia,fechContVentas Por Referencia,fechCodVendMaxVentas Por Referencia, fechCodVendedorMaxMonto Por Referencia)
	fechMaxVentas <- 0
	fechMaxMonto <- 0
	fechContVentas <- 0
	fechCodVendMaxVentas <- 0
	fechCodVendedorMaxMonto <- 0
FinFuncion
// *************************************************************************
// FUNCION : iniciailzoVariablesGenerales(genFilMaxVentas Por Referencia, genMaxVentas Por Referencia)
// PARAMETROS : genFilMaxVentas Por Referencia, genMaxVentas Por Referencia
// ACCION : inicializa las variables genFilMaxVentas,genMaxVentas en 0 
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion inicializoVariablesGeneral(genFilMaxVentas Por Referencia, genMaxVentas Por Referencia)
	genFilMaxVentas <- 0
	genMaxVentas <- 0
FinFuncion
// *************************************************************************
// FUNCION : exito <-esAnio(cad)
// PARAMETROS : cad
// ACCION : determino si el año ingresado es válido(no determina el año biciesto)
// DEVUELVE : exito --> booleano
// *************************************************************************
Funcion exito <-esAnio(cad)
	Definir exito Como Logico
	Definir indice, longCad Como Entero
	exito <- esNumero(Subcadena(cad,0,0))
	longCad <- Longitud(cad)
	indice <- 1
	Mientras exito & indice < longCad Hacer
		exito <- esNumero(Subcadena(cad,indice,indice))
		indice <- indice + 1
	FinMientras
FinFuncion
// *************************************************************************
// FUNCION : exito <- estaEnRango(num,menor,mayor)
// PARAMETROS : num,menor,mayor
// ACCION : determino si num está en el medio entre menor y mayor
// DEVUELVE : exito --> booleano
// *************************************************************************
Funcion exito <- estaEnRango(num,menor,mayor)
	Definir exito Como Logico
	exito <-(num >= menor & num <= mayor)
FinFuncion
// *************************************************************************
// FUNCION : exito <- esDiaMes(cad,bandera)
// PARAMETROS : cad,bandera
// ACCION : valido si el día o el mes esta en rango 
// DEVUELVE : exito --> booleano
// *************************************************************************
Funcion exito <- esDiaMes(cad,bandera)
	Definir exito Como Logico
	Definir num Como Entero
	num<-ConvertirANumero(cad)
	Si bandera = 0 Entonces
		exito <- estaEnRango(num,1,30)
	SiNo 
		exito <- estaEnRango(num,1,12)
	FinSi
FinFuncion
// *************************************************************************
// FUNCION : exito <- esFormatoFecha(cad)
// PARAMETROS : cad
// ACCION : valido si la fehca fue ingresada con el formato correspondiente
// DEVUELVE : exito --> booleano
// *************************************************************************
Funcion exito <- esFormatoFecha(cad)
	Definir exito Como Logico
	Definir indice, longCad Como Entero
	exito <- esNumero(Subcadena(cad,0,0))
	longCad <- Longitud(cad)
	indice <- 1
	Mientras exito & indice < LongCad Hacer
		Si (indice = 2 o indice = 5)
			exito <- Subcadena(cad,indice,indice) = '/'
		SiNo
			exito <- esNumero(Subcadena(cad,indice,indice))
		FinSi
		indice <- indice + 1
	FinMientras
FinFuncion
// *************************************************************************
// FUNCION : exito <- validarFecha(cad)
// PARAMETROS : cad
// ACCION : valido la cadena pasada por parámentro
// DEVUELVE : exito --> booleano
// *************************************************************************
Funcion exito <- validarFecha(cad)
	Definir exito Como Logico
	Definir longCad Como Entero
	longCad <- Longitud(cad)
	//validoFormato			valido dia						valido mes						valido anio
	exito <- (esFormatoFecha(cad) & esDiaMes(Subcadena(cad,0,1),0) & esDiaMes(Subcadena(cad,3,4),1) & esAnio(Subcadena(cad,6,longCad)))
FinFuncion
// *************************************************************************
// FUNCION : ingresoFecha(fechaIng Por Referencia)
// PARAMETROS : fechaIng Por Referencia
// ACCION : le solicito al usuario una fecha y lo valido
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion  ingresoFecha(fechaIng Por Referencia)
	Escribir "Ingrese una fecha (dd/mm/yyyy): "
	Leer fechaIng
	Mientras !validarFecha(fechaIng) Hacer
		Escribir "Error !!, Ingrese una fecha (dd/mm/yyyy): "
		Leer fechaIng
	FinMientras
FinFuncion
// *************************************************************************
// FUNCION : inicioGeneral(genFilMaxVentas Por Referencia, genMaxVentas Por Referencia)
// PARAMETROS : genFilMaxVentas Por Referencia, genMaxVentas Por Referencia
// ACCION : llamo a la función iniciailzoVariablesGeneral
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion inicioGeneral(genFilMaxVentas Por Referencia, genMaxVentas Por Referencia) 
	inicializoVariablesGeneral(genFilMaxVentas, genMaxVentas)	
FinFuncion
// *************************************************************************
// FUNCION : inicioFilial(i,filFechMaxVentas Por Referencia,filContVentas Por Referencia)
// PARAMETROS : i,filFechMaxVentas Por Referencia,filContVentas Por Referencia
// ACCION : mustro por pantalla la filial correspondiente y llamo a la función inicializoVariablesFilial
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion inicioFilial(i,filFechMaxVentas Por Referencia,filContVentas Por Referencia,filFechMaximo Por Referencia)
	Escribir "Filial ",i
	inicializoVariablesFilial(filFechMaxVentas,filContVentas,filFechMaximo)
FinFuncion
// *************************************************************************
// FUNCION : inicioFecha(fechIng Por Referencia,fechMaxVentas Por Referencia,fechMaxMonto Por Referencia,fechContVentas Por Referencia)
// PARAMETROS : fechIng Por Referencia,fechMaxVentas Por Referencia,fechMaxMonto Por Referencia,fechContVentas Por Referencia
// ACCION : llamo a las funciones inicializoVariablesFecha y ingresoFecha
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion inicioFecha(fechIng Por Referencia,fechMaxVentas Por Referencia,fechMaxMonto Por Referencia,fechContVentas Por Referencia,fechCodVendMaxVentas Por Referencia, fechCodVendedorMaxMonto Por Referencia)
	inicializoVariablesFecha(fechMaxVentas,fechMaxMonto,fechContVentas,fechCodVendMaxVentas, fechCodVendedorMaxMonto)
	ingresoFecha(fechIng)
FinFuncion
// *************************************************************************
// FUNCION : inicioVendedor(codVendIng,venCodAnterior Por Referencia,venContVentas Por Referencia,venContMonto Por Referencia)
// PARAMETROS : codVendIng,venCodAnterior Por Referencia,venContVentas Por Referencia,venContMonto Por Referencia
// ACCION : llamo a la función inicializoVariablesVendedor y le asigno a la variable venCodAnterior el código del vendedor
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion inicioVendedor(codVendIng,venCodAnterior Por Referencia,venContVentas Por Referencia,venContMonto Por Referencia)
	inicializoVariablesVendedor(venContVentas,venContMonto)
	venCodAnterior <- codVendIng
FinFuncion
// *************************************************************************
// FUNCION : exito <- validarCodigo(cad)
// PARAMETROS : cad
// ACCION : valido la cadena pasada por parámetro
// DEVUELVE : exito --> booleano
// *************************************************************************
Funcion exito <- validarCodigo(cad)
	Definir  exito Como Logico
	Definir indice, longCad Como Entero 
	exito <- esNumero(Subcadena(cad,0,0))
	indice <- 1
	longCad <- Longitud(cad)
	Mientras  exito = Verdadero & indice < longCad Hacer
		exito <- esNumero(Subcadena(cad,indice,indice))
		indice <- indice + 1
	FinMientras
FinFuncion
// *************************************************************************
// FUNCION : ingresoCodigoVendedor(codVendIng Por Referencia)
// PARAMETROS : codVendIng Por Referencia
// ACCION : le pido al usuario el código del vendedor y lo valido
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion ingresoCodigoVendedor(codVendIng Por Referencia)
	Definir dato Como Cadena
	Escribir "Ingrese el codigo del vendedor: "
	Leer dato
	Mientras !validarCodigo(dato) Hacer
		Escribir "Error!!, ingrese el código del vendedor: "
		Leer dato
	FinMientras
	codVendIng <- ConvertirANumero(dato)	
FinFuncion
// *************************************************************************
// FUNCION : ingresoMonto(monto Por Referencia)
// PARAMETROS : monto Por Referencia
// ACCION : le solicita al usuario que ingrese un monto
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion ingresoMonto(monto Por Referencia)
	Escribir "Ingrese monto: "
	Leer monto
	Mientras monto < 0 Hacer
		Escribir "El monto ingresado no debe ser un número negativo, Ingrese monto: "
		Leer monto
	FinMientras
FinFuncion
// *************************************************************************
// FUNCION : ingresoDatosRestantes(montoIng Por Referencia)
// PARAMETROS : montoIng Por Referencia
// ACCION : llama a la función ingresoMonto
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion ingresoDatosRestantes(montoIng Por Referencia)
	ingresoMonto(montoIng)
FinFuncion
// *************************************************************************
// FUNCION : procesoUnVendedor(montoIng,venContVentas Por Referencia,venContMonto Por Referencia)
// PARAMETROS : montoIng,venContVentas Por Referencia,venContMonto Por Referencia
// ACCION : llama a la funcion aumentoVariable
// DEVUELVE : exito --> booleano
// *************************************************************************
Funcion procesoUnVendedor(montoIng,venContVentas Por Referencia,venContMonto Por Referencia)
	aumentoVariable(1,venContVentas)
	aumentoVariable(montoIng,venContMonto)	
FinFuncion
// *************************************************************************
// FUNCION : finVendedor(venCodAnterior,venContMonto,venContVentas,fechCodVendMaxVentas Por Referencia, fechCodVendedorMaxMonto Por Referencia, fechMaxVentas Por Referencia, fechMaxMonto Por Referencia,fechContVentas Por Referencia, filContVentas Por Referencia)
// PARAMETROS : venCodAnterior,venContMonto,venContVentas,fechCodVendMaxVentas Por Referencia, fechCodVendedorMaxMonto Por Referencia, fechMaxVentas Por Referencia, fechMaxMonto Por Referencia,fechContVentas Por Referencia, filContVentas Por Referencia
// ACCION : llama a las funciones buscoMaxYAsigno y aumentoVariable
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion finVendedor(venCodAnterior,venContMonto,venContVentas,fechCodVendMaxVentas Por Referencia, fechCodVendedorMaxMonto Por Referencia, fechMaxVentas Por Referencia, fechMaxMonto Por Referencia,fechContVentas Por Referencia, filContVentas Por Referencia)
	buscoMaxYAsigno(venCodAnterior,venContVentas,fechCodVendMaxVentas,fechMaxVentas)
	buscoMaxYAsigno(venCodAnterior,venContMonto,fechCodVendedorMaxMonto,fechMaxMonto)
	aumentoVariable(venContVentas,filContVentas)
	aumentoVariable(venContVentas,fechContVentas)	
FinFuncion
// *************************************************************************
// FUNCION : resp <- preguntarHayFecha()
// PARAMETROS : no tiene
// ACCION : pregunta al usuario si desea ingresar otra fecha
// DEVUELVE : resp --> caracter(s/n)
// *************************************************************************
Funcion resp <- preguntarHayFecha()
	Definir resp Como Cadena
	Escribir "Desea ingresar otra fecha (s/n):"
	Leer resp
	Mientras resp <> 's' & resp <> 'n' & resp <> 'S' & resp <> 'N' Hacer
		Escribir "Error !!, Desea ingresar otra fecha (s/n):"
		Leer resp
	FinMientras
	resp <- Mayusculas(resp)
FinFuncion
// *************************************************************************
// FUNCION : mostrarDatosVendedor(fechCodVendMaxVentas, fechCodVendedorMaxMonto, fechMaxVentas, fechMaxMonto)
// PARAMETROS : fechCodVendMaxVentas, fechCodVendedorMaxMonto, fechMaxVentas, fechMaxMonto
// ACCION : muestra por pantalla los datos del vendedor solicitado
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion mostrarDatosVendedor(fechCodVendMaxVentas, fechCodVendedorMaxMonto, fechMaxVentas, fechMaxMonto)
	Escribir "El vendedor que más ingresos generó es ", fechCodVendedorMaxMonto, " con $", fechMaxMonto
	Escribir "El vendedor que más ventas realizó es ", fechCodVendMaxVentas, " con ", fechMaxVentas
FinFuncion
// *************************************************************************
// FUNCION : finFecha(fechIng,fechCodVendMaxVentas, fechCodVendedorMaxMonto, fechMaxVentas, fechMaxMonto, fechContVentas, filFechMaxVentas Por Referencia, filFechMaximo Por Referencia)
// PARAMETROS : fechIng,fechCodVendMaxVentas, fechCodVendedorMaxMonto, fechMaxVentas, fechMaxMonto, fechContVentas, filFechMaxVentas Por Referencia, filFechMaximo Por Referencia
// ACCION : llama a las funciones buscoMaxYAsigno y mostrarDatosVendedor. Además escribe la fecha correspondiente
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion finFecha(fechIng,fechCodVendMaxVentas, fechCodVendedorMaxMonto, fechMaxVentas, fechMaxMonto, fechContVentas, filFechMaxVentas Por Referencia, filFechMaximo Por Referencia)
	buscoMaxYAsigno(fechIng,fechContVentas,filFechMaximo,filFechMaxVentas)
	Escribir "Fecha: ", fechIng
	mostrarDatosVendedor(fechCodVendMaxVentas, fechCodVendedorMaxMonto, fechMaxVentas, fechMaxMonto)
FinFuncion
// *************************************************************************
// FUNCION : mostrarDatosFecha(filFechMaxVentas,filFechMaximo)
// PARAMETROS : filFechMaxVentas,filFechMaximo
// ACCION : muestra por pantalla los datos de fecha correspondiente
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion mostrarDatosFecha(filFechMaxVentas,filFechMaximo)
	Escribir "La fecha que tuvo mayor cantidad de ventas es ", filFechMaximo, " con ", filFechMaxVentas," ventas"
FinFuncion
// *************************************************************************
// FUNCION : finFilial(i,filFechMaxVentas,filFechMaximo,filContVentas,genFilMaxVentas Por Referencia, genMaxVentas Por Referencia)
// PARAMETROS : i,filFechMaxVentas,filFechMaximo,filContVentas,genFilMaxVentas Por Referencia, genMaxVentas Por Referencia
// ACCION : llama a las funciones buscoMaxYAsigno y mostrarDatosFecha. Además muestra por pantalla la filial correspondiente
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion finFilial(i,filFechMaxVentas,filFechMaximo,filContVentas,genFilMaxVentas Por Referencia, genMaxVentas Por Referencia)
	buscoMaxYAsigno(i,filContVentas,genFilMaxVentas,genMaxVentas)
	Escribir "Filial: ", i
	mostrarDatosFecha(filFechMaxVentas,filFechMaximo)
FinFuncion
// *************************************************************************
// FUNCION : mostrarDatosGeneral(filFechMaxVentas,filFechMaximo)
// PARAMETROS : filFechMaxVentas,filFechMaximo
// ACCION : muestra por pantalla los datos solicitados
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion mostrarDatosGeneral(filFechMaxVentas,filFechMaximo)
	Escribir "La filial que tuvo mayor cantidad de ventas es ", filFechMaxVentas, " con ", filFechMaximo," ventas"
FinFuncion
// *************************************************************************
// FUNCION : finGeneral(genFilMaxVentas, genMaxVentas)
// PARAMETROS : genFilMaxVentas, genMaxVentas
// ACCION : convoca a la función mostrarDatosGeneral
// DEVUELVE : no devuelve nada
// *************************************************************************
Funcion finGeneral(genFilMaxVentas, genMaxVentas)
	mostrarDatosGeneral(genFilMaxVentas, genMaxVentas)
FinFuncion 
	