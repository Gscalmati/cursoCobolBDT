Algoritmo Ventas3Filiales
	// Definición de variables
	// Variables a ingresar
	Definir ingFecha como Cadena
	Definir ingLegajo, ingMonto como Entero
	// Variables aux
	Definir i Como Entero
	Definir respFecha Como Caracter
	// Variables vendedor
	Definir cantVentasVendAux, montoVentasVendAux, acumVentasVendAux, legajoVendAux como Entero
	Definir cantVentasVend, montoVentasVend, acumVentasVend, legCantVentasVend, legMontoVentasVend como Entero
	// Variables filial
	
	
	//Inicio Programa
	Para i <- 1 Hasta 1 Hacer
		Repetir
			// Inicializo en 0 variables maximos
			cantVentasVend = 0
			montoVentasVend = 0
			// Inicializo en 0 variables auxiliares
			cantVentasVendAux = 0
			montoVentasVendAux = 0
			// Inicializo en 0 variables Vendedor MÁS VENTAS
			legCantVentasVend = 0
			cantVentasVend = 0
			// Inicializo en 0 variables Vendedor MÁS MONTO
			legMontoVentasVend = 0
			montoVentasVend = 0
			
			// Inicia Fecha
			Escribir "Ingrese fecha de ventas"
			Leer ingFecha
			
			// Luego de fecha se ingresan datos del vendedor
			Escribir "Ingrese número de legajo"
			Leer ingLegajo
			legajoVendAux:= ingLegajo
			
			//Comienzo Proceso de FECHA
			Mientras (ingLegajo <> 0) Hacer
				Escribir "Ingrese monto vendido"
				Leer ingMonto
				// Proceso del VENDEDOR
					Si (ingLegajo = legajoVendAux)
						cantVentasVendAux := cantVentasVendAux +1
						montoVentasVendAux:= montoVentasVendAux + ingMonto
					SiNo
						// Verifico si cambian los MAX
						Si (cantVentasVendAux > cantVentasVend)
							cantVentasVend:= cantVentasVendAux
							legCantVentasVend:= legajoVendAux
						FinSi
						Si (montoVentasVendAux > montoVentasVend)
							montoVentasVend:= montoVentasVendAux
							legMontoVentasVend:= legajoVendAux
						FinSi
						
						cantVentasVendAux:= 1
						montoVentasVendAux:= ingMonto
					FinSi
					//Actualizo LegajoAux
					legajoVendAux:= ingLegajo
					// Lee Legajo
					Escribir "Ingrese número de legajo"
					Leer ingLegajo
					// Final proceso del vendedor
			FinMientras
		Escribir "Desea registar otro dia?"
		Leer respFecha
		Hasta Que respFecha = "No" o respFecha = "N"
		//IMPRIMIR VENTAS DE LA FECHA
	Escribir "El vendedor con más ventas es ", legCantVentasVend, " con ", cantVentasVend, " ventas."
	Escribir "El vendedor que vendió más en dinero es ", legMontoVentasVend, " con $", montoVentasVend
	FinPara
	
FinAlgoritmo
