Algoritmo ElCorralito
	//----------------INGRESOS--------------------//
	definir ING_IMP Como Entero
	definir ING_MOD Como Caracter
	definir ING_SOC Como Entero
	//-------------VARIABLES SOCIO----------------//
	//Cantidad de transferencia del socio
	definir SOC_CAN_TRAN Como Entero
	//----------------VARIABLES GENERALES---------------//
	//Cantidad de socios que transfirieron
	definir GEN_TRAN_CAN_SOC Como Entero
	//Total de tranferencias
	definir GEN_TOT_TRAN Como Entero
	//Socio con max transferencia
	definir GEN_MAX_SOC Como Entero
	//Max transferencia
	definir GEN_MAX_TRAN Como Entero
	
	//-----------------VARIABLES DE TRABAJO---------------//
	definir SOC_ANT Como Entero
	definir linea_titulo como cadena
	definir linea_sub_titulo como cadena
	definir linea_encabezado como cadena
	
	//-------------------------MAIN---------------------//
	
	
	inicioGeneral(ING_SOC,ING_IMP,linea_titulo,linea_sub_titulo,linea_encabezado,GEN_TRAN_CAN_SOC,GEN_TOT_TRAN,GEN_MAX_SOC,GEN_MAX_TRAN)
	imprimirEncabezado(linea_titulo,linea_sub_titulo,linea_encabezado)
	ingresoSocio(ING_SOC,SOC_ANT)
	Mientras ING_SOC <> 0 Hacer
		inicioSocio(SOC_ANT,SOC_CAN_TRAN)
		Hacer
			ingresoRestoDatos(ING_IMP,ING_MOD)
			desarrollo(ING_IMP,ING_MOD,SOC_CAN_TRAN)
			ingresoSocio(ING_SOC,SOC_ANT)
		Hasta Que ING_SOC <> 0 Y ING_SOC = SOC_ANT
		finSocio(SOC_ANT,SOC_CAN_TRAN,GEN_TRAN_CAN_SOC,GEN_TOT_TRAN,GEN_MAX_SOC,GEN_MAX_TRAN)
	FinMientras
	finGeneral(GEN_TRAN_CAN_SOC,GEN_TOT_TRAN,GEN_MAX_SOC,GEN_MAX_TRAN)
FinAlgoritmo

Funcion imprimirEncabezado(linea_titulo,linea_sub_titulo,linea_encabezado)
	Escribir "                 ",linea_titulo
	Escribir "   ",linea_sub_titulo,"   "
	Escribir "          ",linea_encabezado
FinFuncion

Funcion inicioGeneral(ING_SOC Por Referencia,ING_IMP Por Referencia,linea_titulo Por Referencia,linea_sub_titulo Por Referencia,linea_encabezado Por Referencia,GEN_TRAN_CAN_SOC Por Referencia,GEN_TOT_TRAN Por Referencia,GEN_MAX_SOC Por Referencia,GEN_MAX_TRAN Por Referencia)
	traerLineas(linea_titulo,linea_sub_titulo,linea_encabezado)
	GEN_TRAN_CAN_SOC <- 0
	GEN_TOT_TRAN <- 0
	GEN_MAX_SOC <- 0
	GEN_MAX_TRAN <- -999
	ING_SOC <- 0
	ING_IMP <- 0
FinFuncion

Funcion traerLineas(linea_titulo Por Referencia,linea_sub_titulo Por Referencia,linea_encabezado Por Referencia)
	linea_titulo <- "BANCO: EL CORRALITO"
	linea_sub_titulo <- "LISTADO DE TRANSFERENCIAS BANCARIAS DE SOCIOS"
	linea_encabezado <- "      SOCIO      IMPORTE"
FinFuncion

Funcion ingresoSocio(ING_SOC Por Referencia,SOC_ANT Por Referencia)
	SOC_ANT <- ING_SOC
	Escribir "Ingrese socio. Para finalizar ingrese 0"
	Leer ING_SOC
	validarSocio(ING_SOC)
FinFuncion

Funcion validarSocio(ING_SOCIO Por Referencia)
	Mientras ING_SOCIO < 0
		Escribir "Socio invalido ingrese nuevamente"
		Leer ING_SOCIO
	FinMientras
FinFuncion

Funcion inicioSocio(SOC_ANT Por Referencia,SOC_CAN_TRAN Por Referencia)
	SOC_CAN_TRAN <- 0
FinFuncion

Funcion  ingresoRestoDatos(ING_IMP Por Referencia,ING_MOD Por Referencia)
	Escribir "Ingrese el importe"
	Leer ING_IMP
	validarImporte(ING_IMP)
	Escribir "Ingrese la modalidad. D:(Tarjeta de Credito) / E:(Efectivo) / T:(Transferencia)"
	Leer ING_MOD
	validarModalidad(ING_MOD)
FinFuncion

Funcion  validarImporte(ING_IMP Por Referencia)
	Mientras ING_IMP <= 0 Hacer
		Escribir "Importe invalido. Ingrese nuevamente"
		Leer ING_IMP
	FinMientras
FinFuncion

Funcion  validarModalidad(ING_MOD Por Referencia)
	Mientras ING_MOD <> 'D' Y ING_MOD <> 'E' Y ING_MOD <> 'T' Hacer
		Escribir "Modalidad invalida. Ingrese D:(Tarjeta de Credito), E:(Efectivo) o T:(Transferencia)"
		Leer ING_MOD
	FinMientras
FinFuncion

Funcion desarrollo(ING_IMP,ING_MOD,SOC_CAN_TRAN Por Referencia)
	Si ING_MOD == 'T' Entonces
		SOC_CAN_TRAN <- SOC_CAN_TRAN + ING_IMP
	FinSi
FinFuncion

Funcion finSocio(SOC_ANT,SOC_CAN_TRAN,GEN_TRAN_CAN_SOC Por Referencia,GEN_TOT_TRAN Por Referencia,GEN_MAX_SOC Por Referencia,GEN_MAX_TRAN Por Referencia)
	Escribir "                  ",SOC_ANT ,"        ",SOC_CAN_TRAN
	Si SOC_CAN_TRAN <> 0 Entonces
		GEN_TRAN_CAN_SOC <- GEN_TRAN_CAN_SOC + 1 
		GEN_TOT_TRAN <- GEN_TOT_TRAN + SOC_CAN_TRAN
		Si GEN_MAX_TRAN < SOC_CAN_TRAN Entonces
			GEN_MAX_TRAN <- SOC_CAN_TRAN
			GEN_MAX_SOC <- SOC_ANT
		FinSi
	FinSi
	
	
FinFuncion

Funcion finGeneral(GEN_TRAN_CAN_SOC,GEN_TOT_TRAN,GEN_MAX_SOC,GEN_MAX_TRAN)
	Escribir "ESTADISTICA GENERAL:"
	Escribir "En total se procesaron a ", GEN_TRAN_CAN_SOC," socios"
	SI GEN_TRAN_CAN_SOC <> 0 Entonces
		Escribir "El importe general transferido fue de ", GEN_TOT_TRAN
		Escribir "El socio que mas dinero transfirio fue el ",GEN_MAX_SOC," con ", GEN_MAX_TRAN
	SiNo
		Escribir "No se realizaron tranferencias"
	FinSi
FinFuncion
	