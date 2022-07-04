Algoritmo JuanMarceloRuizAlvarez
	//Variables Generales
	Definir cantSociosProSoloTransf, socioMasDineroTransf Como Entero
	Definir totalGralTransferido, mayorTransferencia Como Real
	
	//Variables Particulares
	Definir importe, totalDebitoSoloTransferencia Como Real
	Definir nroSocioActual, nroSocioAnterior Como Entero
	Definir modalidad Como Caracter
	inicioGeneral(cantSociosProSoloTransf,socioMasDineroTransf,mayorTransferencia,socioMasDineroTransf,totalGralTransferido)
	ingresoGeneral(nroSocioActual)
	Mientras nroSocioActual <> 0 Hacer
		inicioSocio(totalDebitoSoloTransferencia,nroSocioActual,nroSocioAnterior)
		Mientras nroSocioActual = nroSocioAnterior Hacer
			ingresoRestoDatosSocios(importe, modalidad)
			procesoDatos(importe, modalidad,totalDebitoSoloTransferencia)
			ingresoGeneral(nroSocioActual)
		FinMientras
		finSocio(nroSocioAnterior, totalDebitoSoloTransferencia,cantSociosProSoloTransf,totalGralTransferido,mayorTransferencia,socioMasDineroTransf)
	FinMientras
	finGeneral(cantSociosProSoloTransf,totalGralTransferido,socioMasDineroTransf,mayorTransferencia)
FinAlgoritmo


//Funciones

Funcion finGeneral(cantSociosProSoloTransf,totalGralTransferido,socioMasDineroTransf,mayorTransferencia)
	Escribir 'ESTADISTICA GENERAL:'
	Escribir 'En total se procesaron a, ',cantSociosProSoloTransf,' Socios'
	Escribir 'El importe general transferido fue de: ',totalGralTransferido
	Escribir 'El Socio que mas dinero transfirió fue el ',socioMasDineroTransf,' con ',mayorTransferencia
FinFuncion

Funcion inicioGeneral(cantSociosProSoloTransf Por Referencia,socioMasDineroTransf Por Referencia,mayorTransferencia Por Referencia, socioMasDineroTransf Por Referencia, totalGralTransferido Por Referencia)
	cantSociosProSoloTransf=0
	socioMasDineroTransf=0
	mayorTransferencia=0
	socioMasDineroTransf=0
	totalGralTransferido=0
FinFuncion

Funcion inicioSocio(totalDebitosSoloTransf Por Referencia, nroSocioActual, nroSocioAnterior Por Referencia)
	totalDebitosSoloTransf=0
	nroSocioAnterior=nroSocioActual
FinFuncion

Funcion ingresoGeneral(importe Por Referencia)	
	Definir mensajeDeIngreso,mensajeDeError Como Caracter
	Definir opc Como Entero
	mensajeDeIngreso <- 'Ingrese un Nro. de Socio ( >=0 ): '
	mensajeDeError <- 'Error! El Nro. de Socio  debe ser mayor o igual a cero. Ingrese el Nro. de Socio nuevamente: '
	opc <- 0
	importe <- ingresarEnteroValidado(mensajeDeIngreso,mensajeDeError,opc)
FinFuncion

Funcion respuesta <- ingresarEnteroValidado(mensajeDeIngreso,mensajeDeError,piso)
	// mensajeDeIngreso: el mensaje que se mostrará al usuario para pedir la respuesta.
	// mensajeDeError: el mensaje que se mostrará al usuario en caso de que la respuesta sea inválida.
	// piso: valor a partir del cual la respuesta será válida (el piso no es válido).
	// repsuesta: respuesta del usuario que debe ser validada.
	Definir respuesta Como Entero
	Escribir mensajeDeIngreso
	Leer respuesta
	Mientras (!validarIngresoEntero(respuesta,piso)) Hacer
		Escribir mensajeDeError
		Leer respuesta
	FinMientras
FinFuncion

Funcion esValido <- validarIngresoEntero(respuesta,piso)
	// respuesta: la respuesta del usuario que se debe validar.
	// piso: valor a partir del cual la respuesta será válida (el piso no es válido).
	// esValido: valor booleano que indica si la respuesta ingresada es correcta o no.
	Definir esValido Como Logico
	esValido <- respuesta>=piso
FinFuncion

Funcion ingresoRestoDatosSocios(importe Por Referencia, modalidad Por Referencia)
	ingresoImporteSocio(importe)
	ingresoModalidadSocio(modalidad)
FinFuncion

Funcion ingresoImporteSocio(importe Por Referencia)	
	Definir mensajeDeIngreso,mensajeDeError Como Caracter
	Definir opc Como Entero
	mensajeDeIngreso <- 'Ingrese un importe ( >0 ): '
	mensajeDeError <- 'Error! El importe debe ser mayor a cero. Ingrese el importe nuevamente: '
	opc <- 0
	importe <- ingresarRealValidado(mensajeDeIngreso,mensajeDeError,opc)
FinFuncion

Funcion respuesta <- ingresarRealValidado(mensajeDeIngreso,mensajeDeError,piso)
	// mensajeDeIngreso: el mensaje que se mostrará al usuario para pedir la respuesta.
	// mensajeDeError: el mensaje que se mostrará al usuario en caso de que la respuesta sea inválida.
	// piso: valor a partir del cual la respuesta será válida (el piso no es válido).
	// repsuesta: respuesta del usuario que debe ser validada.
	Definir respuesta Como Real
	Escribir mensajeDeIngreso
	Leer respuesta
	Mientras (!validarIngresoReal(respuesta,piso)) Hacer
		Escribir mensajeDeError
		Leer respuesta
	FinMientras
FinFuncion

Funcion esValido <- validarIngresoReal(respuesta,piso)
	// respuesta: la respuesta del usuario que se debe validar.
	// piso: valor a partir del cual la respuesta será válida (el piso no es válido).
	// esValido: valor booleano que indica si la respuesta ingresada es correcta o no.
	Definir esValido Como Logico
	esValido <- respuesta>piso
FinFuncion

Funcion ingresoModalidadSocio(modalidad Por Referencia)	
	Definir mensajeDeIngreso,mensajeDeError Como Caracter
	Definir opc1, opc2, opc3 Como Caracter
	mensajeDeIngreso <- 'Ingrese una modalidad (D: tarjeta de Débito, E: Efectivo, T: Transferencia): '
	mensajeDeError <- 'Error! La modalidad debe contener un caracter (D, E o T). Ingrese nuevamente: '
	opc1 <- 'D'
	opc2 <- 'E'
	opc3 <- 'T'
	modalidad <- ingresarCaracterValidado(mensajeDeIngreso,mensajeDeError,opc1,opc2,opc3)
FinFuncion

Funcion respuesta <- ingresarCaracterValidado(mensajeDeIngreso,mensajeDeError,opc1,opc2,opc3)
	Definir respuesta Como Caracter
	Escribir mensajeDeIngreso
	Leer respuesta
	Mientras (!validarIngresoCaracter(respuesta,opc1,opc2,opc3)) Hacer
		Escribir mensajeDeError
		Leer respuesta
	FinMientras
FinFuncion

Funcion esValido <- validarIngresoCaracter(respuesta,opc1,opc2,opc3)
	Definir esValido Como Logico
	esValido <- respuesta=opc1 o respuesta=opc2 o respuesta=opc3
FinFuncion

Funcion procesoDatos(importe, modalidad, totalDebitosSoloTransf Por Referencia)
	Si modalidad = 'T' Entonces
		totalDebitosSoloTransf=totalDebitosSoloTransf + importe
	FinSi
FinFuncion

Funcion finSocio(nroSocioAnterior, totalDebitosSoloTransf Por Referencia, cantSociosProSoloTransf Por Referencia, totalGralTransferido Por Referencia,mayorTransferencia Por Referencia,socioMasDineroTransf Por Referencia)
	Limpiar Pantalla
	Escribir 'BANCO: EL CORRALITO'
	Escribir 'LISTADO DE TRANSFERENCIAS BANCARIAS DE SOCIOS'
	Escribir 'SOCIO   ', 'IMPORTE'
	Escribir nroSocioAnterior,'   ',totalDebitosSoloTransf
	Esperar Tecla
	si totalDebitosSoloTransf <> 0 Entonces
		cantSociosProSoloTransf = cantSociosProSoloTransf + 1
	FinSi
	totalGralTransferido=totalGralTransferido+totalDebitosSoloTransf
	setMax(mayorTransferencia,socioMasDineroTransf,totalDebitosSoloTransf,nroSocioAnterior)
	totalDebitosSoloTransf=0
FinFuncion

Funcion setMax(valorMaximo Por Referencia,datoMaximo Por Referencia,valorNuevo,datoNuevo)
	Si valorNuevo>valorMaximo Entonces
		valorMaximo <- valorNuevo
		datoMaximo <- datoNuevo
	FinSi
FinFuncion