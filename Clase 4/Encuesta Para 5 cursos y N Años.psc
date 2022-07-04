Algoritmo EncuestaV2
	Definir i,anio Como Entero
	Definir nomIng Como Caracter
	Definir genIng,acuerIng,respPorAnio Como Caracter
	// Variables de la Temática de todos los años juntos - TOTAL
	Definir cantMujeresTotal,cantVaronesTotal,cantMujeresOkTotal Como Entero
	// Variables de la Temática de todos los cursos
	Definir cantMujeresAnio,cantVaronesAnio,cantMujeresOkAnio Como Entero
	// Variables de la Temática de 1 curso en particular
	Definir cantMujeresCur,cantVaronesCur,cantMujeresOkCur Como Entero
	// Inicio Temático todos los años juntos - TOTAL
	inicioGeneral(cantMujeresTotal,cantVaronesTotal,cantMujeresOkTotal)
	// Inicio contador de años
	anio <- 1
	Repetir
		// Inicio Temático todos los cursos en el año
		inicioGeneral(cantMujeresAnio,cantVaronesAnio,cantMujeresOkAnio)
		Escribir 'Relevamiento de alumnos, año número ',anio
		// procesamientoPorAnio(cantMujeresTotal,cantVaronesTotal,cantMujeresOkTotal,cantMujeresAnio,cantVaronesAnio,cantMujeresOkAnio, i)
		Para i<-1 Hasta 2 Hacer
			// Inicio Temático de 1 curso en particular
			inicioCurso(cantMujeresCur,cantVaronesCur,cantMujeresOkCur,i)
			// ingreso del nombre de 1 alumno
			ingresoNombre(nomIng)
			Mientras nomIng<>'FIN' Hacer
				// ingreso el resto de los datos del alumno
				ingresoRestoDatos(nomIng,genIng,acuerIng)
				// proceso a 1 alumno
				procesamiento1Alumno(genIng,acuerIng,cantMujeresCur,cantVaronesCur,cantMujeresOkCur)
				// ingreso del nombre de 1 alumno
				ingresoNombre(nomIng)
			FinMientras
			// Fin Temático de 1 curso en particular
			imprimir1Curso(cantMujeresCur,cantVaronesCur,cantMujeresOkCur,cantMujeresAnio,cantVaronesAnio,cantMujeresOkAnio,i)
		FinPara
		// Fin Temático todos los cursos
		imprimirGeneral(cantMujeresAnio,cantVaronesAnio,cantMujeresOkAnio,cantMujeresTotal,cantVaronesTotal,cantMujeresOkTotal)
		Escribir '------------**------------'
		Escribir 'Hay otro año para encuestar? Si/No'
		Leer respPorAnio
		Mientras (respPorAnio<>'Si') Y (respPorAnio<>'No') Hacer
			Escribir 'Ingrese un valor afirmativo (Si) o negativo (No)'
			Leer respPorAnio
		FinMientras
		Si respPorAnio='Si' Entonces
			anio <- anio+1
		FinSi
	Hasta Que respPorAnio='No'
	// Fin Temático todos los años
	imprimirTotal(cantMujeresTotal,cantVaronesTotal,cantMujeresOkTotal)
FinAlgoritmo

Funcion inicioGeneral (cantGen1 Por Referencia,cantGen2 Por Referencia,cantGen3 Por Referencia)
	cantGen1 <- 0
	cantGen2 <- 0
	cantGen3 <- 0
FinFuncion

Funcion inicioCurso (cantPart1 Por Referencia,cantPart2 Por Referencia,cantPart3 Por Referencia,i Por Valor)
	cantPart1 <- 0
	cantPart2 <- 0
	cantPart3 <- 0
	Escribir 'Curso Número ',i
	Escribir '----------------------'
FinFuncion

Funcion ingresoNombre (nombre Por Referencia)
	Escribir 'Por favor, ingrese nombre'
	Leer nombre
FinFuncion

Funcion ingresoRestoDatos (nombre Por Valor,genero Por Referencia,confirmacion Por Referencia)
	Escribir 'Excelente, favor ingrese el resto de datos'
	Escribir '----------------------'
	Escribir 'Ingrese su género:'
	Leer genero
	Mientras (genero<>'H') Y (genero<>'M') Y (genero<>'NB') Hacer
		Escribir 'Ingrese un valor válido entre estas opciones:'
		Escribir 'H - Hombre/ M - Mujer/ NB - No Binario'
		Leer genero
	FinMientras
	Escribir '----------------------'
	Escribir 'Ingrese si esta de acuerdo, o no, con el nivel del curso: S - Si/ N - No'
	Leer confirmacion
	Mientras (confirmacion<>'S') Y (confirmacion<>'Si') Y (confirmacion<>'N') Y (confirmacion<>'No') Hacer
		Escribir 'Ingrese un valor válido entre estas opciones:'
		Escribir 'S - Si/ N - No'
		Leer confirmacion
	FinMientras
FinFuncion

Funcion procesamiento1Alumno (genIng Por Valor,acuerIng Por Valor,cantMujeres Por Referencia,cantVarones Por Referencia,cantMujeresOk Por Referencia)
	Si (genIng='H') O (genIng='M') Entonces
		Si (genIng='M') Entonces
			cantMujeres <- cantMujeres+1
			Si (acuerIng='S') O (acuerIng='Si') Entonces
				cantMujeresOk <- cantMujeresOk+1
			FinSi
		SiNo
			cantVarones <- cantVarones+1
		FinSi
	FinSi
FinFuncion

Funcion imprimir1Curso (cantMujeresPart Por Valor,cantVaronesPart Por Valor,cantMujeresOkPart Por Valor,cantMujeresGen Por Referencia,cantVaronesGen Por Referencia,cantMujeresOkGen Por Referencia,i Por Valor)
	cantMujeresGen <- cantMujeresGen+cantMujeresPart
	cantVaronesGen <- cantVaronesGen+cantVaronesPart
	cantMujeresOkGen <- cantMujeresOkGen+cantMujeresOkPart
	Escribir 'Resultados de curso ',i
	Escribir '----------------------'
	// Porcentaje de Mujeres por curso
	Escribir 'El porcentaje de mujeres en el curso ',i,' es ',(cantMujeresPart*100)/(cantVaronesPart+cantMujeresPart)
	Escribir '----------------------'
	Escribir '----------------------'
	// Porcentaje de Mujeres de acuerdo
	Escribir 'El porcentaje de mujeres DE ACUERDO con el nivel en el curso ',i,' es ',(cantMujeresOkPart*100)/(cantMujeresPart)
	Escribir '----------------------'
	Escribir '----------------------'
FinFuncion

Funcion imprimirGeneral (cantMujeresAnio Por Valor,cantVaronesAnio Por Valor,cantMujeresOkAnio Por Valor,cantMujeresTotal Por Referencia,cantVaronesTotal Por Referencia,cantMujeresOkTotal Por Referencia)
	cantMujeresTotal <- cantMujeresTotal+cantMujeresAnio
	cantVaronesTotal <- cantVaronesTotal+cantVaronesAnio
	cantMujeresOkTotal <- cantMujeresOkTotal+cantMujeresOkAnio
	Escribir 'Resultados del año'
	// Porcentaje de Mujeres general
	Escribir 'El porcentaje de mujeres en este año es ',(cantMujeresAnio*100)/(cantVaronesAnio+cantMujeresAnio)
	Escribir '----------------------'
	Escribir '----------------------'
	// Porcentaje de Mujeres de acuerdo
	Escribir 'El porcentaje de mujeres DE ACUERDO con el nivel en el curso en este año es ',(cantMujeresOkAnio*100)/(cantMujeresAnio)
	Escribir '----------------------'
	Escribir '----------------------'
FinFuncion

Funcion procesamientoPorAnio (cantMujeresTotal Por Referencia,cantVaronesTotal Por Referencia,cantMujeresOkTotal Por Referencia,cantMujeresAnio Por Valor,cantVaronesAnio Por Valor,cantMujeresOkAnio Por Valor,i Por Valor)
	i <- 0
	Para i<-1 Hasta 5 Hacer
		// Inicio Temático de 1 curso en particular
		inicioCurso(cantMujeresCur,cantVaronesCur,cantMujeresOkCur,i)
		// ingreso del nombre de 1 alumno
		ingresoNombre(nomIng)
		Mientras nomIng<>'FIN' Hacer
			// ingreso el resto de los datos del alumno
			ingresoRestoDatos(nomIng,genIng,acuerIng)
			// proceso a 1 alumno
			procesamiento1Alumno(genIng,acuerIng,cantMujeresCur,cantVaronesCur,cantMujeresOkCur)
			// ingreso del nombre de 1 alumno
			ingresoNombre(nomIng)
		FinMientras
		// Fin Temático de 1 curso en particular
		imprimir1Curso(cantMujeresCur,cantVaronesCur,cantMujeresOkCur,cantMujeresGen,cantVaronesGen,cantMujeresOkGen,i)
	FinPara
	// Fin Temático todos los cursos
	imprimirGeneral(cantMujeresAnio,cantVaronesAnio,cantMujeresOkAnio,cantMujeresTotal,cantVaronesTotal,cantMujeresOkTotal)
FinFuncion

Funcion imprimirTotal (cantMujeresTotal Por Valor,cantVaronesTotal Por Valor,cantMujeresOkTotal Por Valor)
	Escribir 'Resultados totales de todos los años'
	// Cantidades Totales
	Escribir 'La cantidad de mujeres en todos los años es ',cantMujeresTotal,' y la cantidad de varones es ',cantVaronesTotal
	// Porcentaje de Mujeres Total
	Escribir 'El porcentaje de mujeres en total es ',(cantMujeresTotal*100)/(cantVaronesTotal+cantMujeresTotal)
	Escribir '----------------------'
	Escribir '----------------------'
FinFuncion
