Algoritmo debitosElCorralito
	//VARIABLES
	//Generales
	definir cantSociosProcesadosConTransf, numSocioMasTransf, cantSocioMasTransf Como Entero
	definir totalGeneral Como Real
	//Socio 
	definir montoOpTransf como Real
	definir nroSocioAct, cantTransferencias Como Entero
	//Ingresos
	definir nroSocioIng Como Entero
	definir importeSocio Como Real
	definir modalidadOp Como Caracter
	
	//INICIO PROGRAMA
	inicioGeneral(cantSociosProcesadosConTransf, totalGeneral, numSocioMasTransf, cantSocioMasTransf)
	ingresoNroSocio(nroSocioIng)
	Mientras nroSocioIng<>0 Hacer
		inicioSocio(montoOpTransf, cantTransferencias, nroSocioIng, nroSocioAct)
		//nroSocioAct <- nroSocioIng
		Mientras nroSocioAct = nroSocioIng Hacer
			ingresoRestoDatos(importeSocio, modalidadOp)
			procesarSocio(importeSocio, modalidadOp, montoOpTransf, cantTransferencias)
			ingresoNroSocio(nroSocioIng)
		FinMientras
		finSocio(nroSocioAct, montoOpTransf, cantTransferencias, cantSociosProcesadosConTransf, numSocioMasTransf, cantSocioMasTransf, totalGeneral)
	FinMientras
	finGeneral(cantSociosProcesadosConTransf, numSocioMasTransf, cantSocioMasTransf, totalGeneral)
FinAlgoritmo

//Inicios

Funcion inicioGeneral(cantSociosProcesados Por Referencia, totalGeneral Por Referencia, numSocioMaximo Por Referencia, cantSocioMaximo Por Referencia)
	inicializarVariable(cantSociosProcesados)
	inicializarVariable(totalGeneral)
	inicializarVariable(numSocioMaximo)
	inicializarVariable(cantSocioMaximo)
	Escribir '                BANCO EL CORRALITO                '
	Escribir '       Listado de Transferencias de Socios       ' 
FinFuncion

Funcion inicioSocio(montoOpTransf Por Referencia, cantTransf Por Referencia, nroIng Por Referencia, nroActual Por Referencia)
	inicializarVariable(montoOpTransf)
	inicializarVariable(cantTransf)
	nroActual = nroIng
FinFuncion

// Inicializadores
Funcion inicializarVariable (var1 Por Referencia)
	var1 <- 0
FinFuncion

// Ingresos

Funcion ingresoNroSocio(nroSoc Por Referencia)
	Escribir 'Ingrese Numero de Socio'
	Leer nroSoc
	validarNro(nroSoc)
FinFuncion

Funcion ingresoImporte(impIng Por Referencia)
	Escribir 'Ingrese importe de operacion'
	Leer impIng
	validarImporte(impIng)
FinFuncion

Funcion ingresoModalidad(modalidad Por Referencia)
	Escribir 'Ingrese tipo de operacion: D - Debito / E - Efectivo / T - Transferencia'
	Leer modalidad
	validarModalidad(modalidad)
FinFuncion

Funcion ingresoRestoDatos( importe Por Referencia, modalidad Por Referencia)
	ingresoImporte(importe)
	ingresoModalidad(modalidad)
FinFuncion

// Validadores
Funcion validarImporte(importe Por Referencia)
	Mientras importe<=0 Hacer
		Escribir 'Error. Ingrese un importe mayor a 0'
		Leer importe
	FinMientras
FinFuncion

Funcion validarNro(numSocio Por Referencia)
	Mientras numSocio<0 Hacer
		Escribir 'Error. Ingrese un numero de socio válido, debe ser a partir del nro. 1'
		Leer numSocio
	FinMientras
FinFuncion

Funcion validarModalidad(modalidad Por Referencia)
	Mientras modalidad<>'D' Y modalidad<>'T' Y modalidad<>'E' Hacer
		Escribir 'Error. Ingrese un tipo de operacion valida: D - Debito / E - Efectivo / T - Transferencia'
		Leer modalidad
	FinMientras
FinFuncion

//Procesos
Funcion procesarSocio (importeSocio Por Valor, modalidadOp Por Valor, montoOpTransf Por Referencia, cantTransferencias Por Referencia)
	Si modalidadOp = 'T'
		montoOpTransf = montoOpTransf + importeSocio
		cantTransferencias = cantTransferencias +1
	FinSi
FinFuncion

//Fines
Funcion finSocio(nroSoc Por Valor, montoOpTransf Por Valor, cantTransferencias Por Valor, cantSociosConTransf Por Referencia, numSocioMasTransf Por Referencia, cantSocioMasTransf Por Referencia, totalGeneral Por Referencia)
	Escribir '       SOCIO     IMPORTE    TRANSFERENCIAS       '
	Escribir '        ', nroSoc,'         ', montoOpTransf, '            ', cantTransferencias, '             '
	Si cantTransferencias > 0 Entonces
		cantSociosConTransf = cantSociosConTransf +1
		Si cantTransferencias > cantSocioMasTransf Entonces
			cantSocioMasTransf = cantTransferencias
			numSocioMasTransf = nroSoc
		FinSi
	FinSi
	totalGeneral = totalGeneral + montoOpTransf
FinFuncion


Funcion finGeneral(cantSociosProcesados, numSocioMasTransf, cantSocioMasTransf, totalGeneral)
	Escribir 'ESTADÍSTICA FINAL'
	Escribir 'En total se procesaron a', cantSociosProcesados, ' socios'
	Escribir  'El socio que más transferencias realizó fue el ', numSocioMasTransf, ' con ' , cantSocioMasTransf, ' transferencias'
	Escribir 'El total general transferido fue de ', totalGeneral
	
FinFuncion
