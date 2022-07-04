Algoritmo filiales3
	// Variables a Ingresar
	definir diaIng como cadena
	definir legIng como entero
	definir impIng como real
	
	// Varialbles AUXILIARES
	definir legAct Como Entero
	definir i Como Entero
	definir otroDia como Cadena
	
	
	// Variables VENDEDOR
	// Maximos
	definir vendMasVentas Como Entero
	definir cantVendMasVentas como Entero
	definir vendMayorMonto Como Entero
	definir cantVendMayorMonto Como Real
	//Acumuladores
	definir cantVentasAct Como Entero
	definir impVentasAct  Como Real
	
	// Variables FILIALES
	// Maximos
	definir cantFilMasVentas como entero
	definir diaFilMasVentas Como Cadena
	//Acumuladores
	definir cantVentasFilAct Como Entero
	
	// Variables TOTALES
	definir cantTotalVentasFilial Como Entero
	definir filialMasVentas Como Entero
	definir cantTotalVentas Como Entero
	
	inicioTotal(cantTotalVentas)
	Para i<-1 Hasta 3 Hacer
		inicioFilial(i, cantFilMasVentas, cantTotalVentasFilial)
		
		// 'procesoFilial'
		Repetir
			inicioFecha(diaIng)
			inicializarVariable(cantVentasFilAct)
			// 'procesoFecha'
			ingresoLegajo(legIng)
			legAct = legIng
			inicializarVariables(cantVentasAct, impVentasAct)
			inicializarMaximos(cantVendMasVentas, cantVendMayorMonto)

			Mientras (legIng <> 0)
				Mientras (legIng = legAct)
					ingresoImporte(impIng)
					//Acumuladores vendedor
					cantVentasAct = cantVentasAct +1
					impVentasAct = impVentasAct + impIng
					
					ingresoLegajo(legIng)
				FinMientras
					//Acumuladores filial
					cantVentasFilAct = cantVentasFilAct + cantVentasAct
					// Defino vendedores con mas ventas y mas monto
					definirMaximos(cantVentasAct, cantVendMasVentas, legAct, vendMasVentas)
					definirMaximos(impVentasAct, cantVendMayorMonto, legAct, vendMayorMonto)
					//Actualizo el Legajo Actual
					legAct = legIng
					//Asigno los valores ingresados del nuevo Legajo
					cantVentasAct = 1
					impVentasAct = impIng
					inicializarVariables(cantVentasAct, impVentasAct)
			FinMientras
			finDiaVenta(cantVendMasVentas, vendMasVentas, cantVendMayorMonto, vendMayorMonto, cantVentasFilAct, cantFilMasVentas, diaFilMasVentas, diaIng, cantTotalVentasFilial)
			ingresoRespuesta(otroDia, i)
		Hasta Que otroDia='No' o otroDia ='NO'
		finalFilial(cantFilMasVentas, diaFilMasVentas, cantTotalVentasFilial, cantTotalVentas, filialMasVentas, i)
	FinPara
	finalGeneral(cantTotalVentas, filialMasVentas)
FinAlgoritmo

//Definir Maximos
Funcion definirMaximos(cantActual por Valor, cantMaximo Por Referencia, nomActual Por Valor, nomMaximo Por Referencia)
	Si cantActual >= cantMaximo
		cantMaximo = cantActual
		nomMaximo = nomActual
	FinSi
FinFuncion

//Inicios
Funcion inicioTotal(cantTotalVentas Por Referencia)
	Escribir "Medidor de ventas 3000"
	inicializarVariable(cantTotalVentas)
FinFuncion

Funcion inicioFecha (diaIng Por Referencia)
	Escribir "Ingrese Fecha de ventas"
	Leer diaIng
FinFuncion

Funcion inicioFilial(i Por Valor, cantFilMasVentas Por Referencia, cantTotalVentasFilial Por Referencia)
	Escribir "Filial ", i
	inicializarVariable(cantFilMasVentas)
	inicializarVariable(cantTotalVentasFilial)
FinFuncion

//Finales 
Funcion finalFilial(cantVentasFil por Valor, diaFilMasVentas POR VALOR, cantTotalVentasFilial Por Referencia, cantTotalVentas Por Referencia, filialMasVentas Por Referencia, numFilial Por Valor)
	Escribir 'El dia de más ventas en la filial fue el ', diaFilMasVentas, ' con ', cantVentasFil, ' ventas.'
	definirMaximos(cantTotalVentasFilial, cantTotalVentas, numFilial, filialMasVentas)
FinFuncion

Funcion finalGeneral (cantTotalVentas Por Valor, filialMasVentas Por Valor)
	Escribir 'La filial con mas ventas de todas es la filial ', filialMasVentas, ' con un total de ', cantTotalVentas, ' ventas.'
FinFuncion

Funcion finDiaVenta(cantVendMasVentas Por Valor, venMasVen Por Valor, cantVendMayorMonto Por Valor, vendMayorMonto Por Valor, cantVentasFilAct Por Valor, cantVentasFil Por Referencia, diaFilMasVentas Por Referencia, diaIng Por VALOR, ventasTotal Por Referencia)
	Escribir 'El vendedor que mas ventas hizo fue el vendedor ', venMasVen, ' con ', cantVendMasVentas
	Escribir 'El vendedor que mas dinero hizo fue el vendedor ', vendMayorMonto, ' con ', cantVendMayorMonto	
	ventasTotal = ventasTotal + cantVentasFilAct
	definirMaximos(cantVentasFilAct, cantVentasFil, diaIng, diaFilMasVentas)
FinFuncion

//Inicializadores
Funcion inicializarVariable (var1 Por Referencia)
	var1 <- 0
FinFuncion

Funcion inicializarVariables (var1 Por Referencia, var2 Por Referencia)
	var1 <- 0
	var2 <- 0
FinFuncion

Funcion inicializarMaximos (var1 Por Referencia, var2 Por Referencia)
	var1 <- -999
	var2 <- -999
FinFuncion

//Ingresos
Funcion ingresoLegajo( legIng Por Referencia)
	Escribir 'Ingrese Legajo del vendedor'
	Leer legIng	
	validarLegajo(legIng)
FinFuncion

Funcion ingresoImporte( impIng Por Referencia)
	Escribir 'Ingrese importe vendido'
	Leer impIng
	validarImporte(impIng)
FinFuncion

Funcion ingresoRespuesta ( respuesta Por Referencia, i Por Valor)
	Escribir 'Desea ingresar otra fecha de ventas para la filial ', i, '? Si/No'
	Leer respuesta
	validarRespuesta(respuesta)
FinFuncion

//Validadores
Funcion validarRespuesta(respuesta Por Referencia)
	Mientras respuesta <> "NO" o respuesta <> 'SI'
		Escribir "Error. Las respuestas validas son SI o NO"
		leer respuesta
	FinMientras
FinFuncion

Funcion validarImporte(importe Por Referencia)
	Mientras importe <= 0 
		Escribir "Error. Ingrese un importe mayor a 0"
		leer importe
	FinMientras
FinFuncion

Funcion validarLegajo(legajo Por Referencia)
	Mientras legajo < 0 
		Escribir "Error. Ingrese un legajo válido, debe ser a partir del legajo 1"
		leer legajo
	FinMientras
FinFuncion
