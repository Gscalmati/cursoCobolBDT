Algoritmo EncuestaV1
	Definir i Como Entero
	Definir nomIng Como Cadena
	Definir genIng,acuerIng Como Caracter
	// Variables de la Temática de todos los cursos
	Definir cantMujeresGen,cantVaronesGen,cantMujeresOkGen Como Entero
	// Variables de la Temática de 1 curso en particular
	Definir cantMujeresCur,cantVaronesCur,cantMujeresOkCur Como Entero
	// Inicio Temático todos los cursos
	inicioGeneral(cantMujeresGen,cantVaronesGen,cantMujeresOkGen)
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
	imprimirGeneral(cantMujeresGen,cantVaronesGen,cantMujeresOkGen)
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
	Mientras (genero <> "H") y (genero <> "M") y (genero <> "NB")	
		Escribir "Ingrese un valor válido entre estas opciones:"
		Escribir "H - Hombre/ M - Mujer/ NB - No Binario"
		Leer genero
	FinMientras
	Escribir '----------------------'
	Escribir 'Ingrese si esta de acuerdo, o no, con el nivel del curso: S - Si/ N - No'
	Leer confirmacion
	Mientras (confirmacion <> "S") y (confirmacion <> "Si") y (confirmacion <> "N") y (confirmacion <> "No")
		Escribir "Ingrese un valor válido entre estas opciones:"
		Escribir "S - Si/ N - No"
		Leer confirmacion
	FinMientras
FinFuncion

Funcion procesamiento1Alumno (genIng Por Valor,acuerIng Por Valor,cantMujeres Por Referencia,cantVarones Por Referencia,cantMujeresOk Por Referencia)
	Si (genIng='H') O (genIng='M') Entonces
		Si (genIng='M') Entonces
			cantMujeres <- cantMujeres+1
			Si (acuerIng='S') o (acuerIng='Si') Entonces
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

Funcion imprimirGeneral (cantMujeresFin Por Valor,cantVaronesFin Por Valor,cantMujeresOkFin Por Valor)
	Escribir 'Resultados final del test'
	// Porcentaje de Mujeres general
	Escribir 'El porcentaje de mujeres en la escuela es ',(cantMujeresFin*100)/(cantVaronesFin+cantMujeresFin)
	Escribir '----------------------'
	Escribir '----------------------'
	// Porcentaje de Mujeres de acuerdo
	Escribir 'El porcentaje de mujeres DE ACUERDO con el nivel en el curso en la ESCUELA es ',(cantMujeresOkFin*100)/(cantMujeresFin)
	Escribir '----------------------'
	Escribir '----------------------'
FinFuncion

