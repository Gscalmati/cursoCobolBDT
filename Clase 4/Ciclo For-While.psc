Algoritmo sin_titulo
	definir i como entero
	definir nomIng como cadena
	definir genIng, acuerIng como caracter
	// Variables de la Temática de todos los cursos
	definir cantMujeresGen, cantVaronesGen, cantMujeresOkGen como entero
	// Variables de la Temática de 1 curso en particular
	definir cantMujeresCur, cantVaronesCur, cantMujeresOkCur como entero
	// Inicio Temático todos los cursos
	inicioGeneral(cantMujeresGen, cantVaronesGen, cantMujeresOkGen)
	Para i<-1 Hasta 1 Hacer
		// Inicio Temático de 1 curso en particular
		inicioCurso(cantMujeresCur, cantVaronesCur, cantMujeresOkCur,i)
		// ingreso del nombre de 1 alumno
		ingresoNombre(nomIng)
		Mientras nomIng<>'FIN' Hacer
			// ingreso el resto de los datos del alumno
			ingresoRestoDatos(nomIng, genIng, acuerIng)
			// proceso a 1 alumno
			procesamiento1Alumno(genIng, acuerIng, cantMujeresCur, cantVaronesCur, cantMujeresOkCur)
			// ingreso del nombre de 1 alumno
			ingresoNombre(nomIng)
		FinMientras
		// Fin Temático de 1 curso en particular
		imprimir1Curso(cantMujeresCur, cantVaronesCur, cantMujeresOkCur, cantMujeresGen, cantVaronesGen, cantMujeresOkGen,i)
	FinPara
	// Fin Temático todos los cursos
	imprimirGeneral(cantMujeresGen, cantVaronesGen, cantMujeresOkGen)
FinAlgoritmo

funcion inicioGeneral (cantGen1 Por Referencia, cantGen2 Por Referencia, cantGen3 Por Referencia)
	cantGen1 := 0
	cantGen2 := 0
	cantGen3 := 0
FinFuncion

funcion inicioCurso (cantPart1 Por Referencia, cantPart2 Por Referencia, cantPart3 Por Referencia,i Por Valor)
	cantPart1 := 0
	cantPart2 := 0
	cantPart3 := 0
	Escribir "Curso Número ", i
	Escribir "----------------------"
FinFuncion

funcion ingresoNombre (nombre Por Referencia)
	Escribir "Por favor, ingrese nombre"
	Leer nombre
FinFuncion

funcion ingresoRestoDatos (nombre Por Valor, genero Por Referencia, confirmacion Por Referencia)
	Escribir "Excelente, favor ingrese el resto de datos"
	Escribir "Ingrese su género: H - Hombre/ M - Mujer/ NB - No Binario"
	Leer genero
	Escribir "Ingrese si esta de acuerdo, o no, con el nivel del curso: S - Si/ N - No"
	Leer confirmacion
FinFuncion

funcion procesamiento1Alumno (genIng Por Valor, acuerIng Por Valor, cantMujeres Por Referencia, cantVarones Por Referencia, cantMujeresOk Por Referencia)
	Si (genIng = "H") o (genIng = "M") Entonces
		Si (genIng = "M") Entonces
			cantMujeres := cantMujeres +1
			Si acuerIng == "S" Entonces
				cantMujeresOk := cantMujeresOk +1
			FinSi
		SiNo
			cantVarones := cantVarones +1
		FinSi
	FinSi
FinFuncion

funcion imprimir1Curso (cantMujeresPart Por Valor, cantVaronesPart Por Valor, cantMujeresOkPart Por Valor, cantMujeresGen Por Referencia, cantVaronesGen Por Referencia, cantMujeresOkGen Por Referencia, i Por Valor)
	cantMujeresGen := cantMujeresGen + cantMujeresPart
	cantVaronesGen := cantVaronesGen + cantVaronesPart
	cantMujeresOkGen := cantMujeresOkGen + cantMujeresOkPart
	Escribir "Resultados de curso ", i
	
	// Porcentaje de Mujeres por curso
	Escribir "El porcentaje de mujeres en el curso ", i, " es ", (cantMujeresPart*100)/(cantVaronesPart+cantMujeresPart)
	//Porcentaje de Mujeres de acuerdo
	Escribir "El porcentaje de mujeres DE ACUERDO con el nivel en el curso ", i, " es ", (cantMujeresOkPart*100)/(cantMujeresPart)
FinFuncion

funcion imprimirGeneral (cantMujeresFin Por Valor, cantVaronesFin Por Valor, cantMujeresOkFin Por Valor)
	Escribir "Resultados final del test"
	
	// Porcentaje de Mujeres general
	Escribir "El porcentaje de mujeres en la escuela es ", (cantMujeresFin*100)/(cantVaronesFin+cantMujeresFin)
	//Porcentaje de Mujeres de acuerdo
	Escribir "El porcentaje de mujeres DE ACUERDO con el nivel en el curso en la ESCUELA es ", (cantMujeresOkFin*100)/(cantMujeresFin)
FinFuncion
	