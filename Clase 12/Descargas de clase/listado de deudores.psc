Algoritmo sin_titulo
	definir suc, socio, dni como entero
	definir nombre, linea1, linea2, linea3, linea4 como cadena
	definir saldo, acum como real
	linea1 <- "Banco El Corralito: "
	linea2 <- "Sucursal: " + convertiratexto(suc)
	linea3 <- "Listado de Socios con saldo negativo:"
	linea4 <- "     socio     Nombre y Apellido     Dni      Saldo deudor      "
	inicio
	ingresoSocio
	Mientras socio <> 0 Hacer
		ingresoResto
		procesoDatos
		ingresoSocio
	FinMientras
	final
FinAlgoritmo

Funcion inicio()
	encabezo
	acum <- 0
FinFuncion

Funcion encabezo
	ingresoSuc()
	imprimoEncabezado()
FinFuncion

Funcion imprimoEncabezado()
	Escribir linea1
	Escribir linea2
	Escribir linea3
	Escribir linea4
FinFuncion

Funcion ingresoSuc()
	traigoSuc()
	Mientras suc < 1 o suc > 100 Hacer
		Escribir "Error, te dije entre 1 y 100"
		traigoSuc()
	FinMientras
FinFuncion

Funcion traigoSuc()
	Escribir "Ingrese nro. de sucursal, entre 1 y 100"
	Leer suc
FinFuncion

Funcion procesoDatos
	Si saldo < 0 Entonces
		acum <- acum + saldo
		Escribir socio, nombre, dni, saldo
	FinSi
FinFuncion

Funcion final()
	Escribir "Total de deuda: ", acum
FinFuncion
