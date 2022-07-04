Algoritmo Ej1VersionGio
	definir cantRecaudada como Entero
	definir cantTotal como Entero
	definir i como Entero
	inicio(cantTotal)
	Para i<-0 Hasta 28 Hacer
		Escribir "Cuánto querés ingresar?"
		Leer cantRecaudada
			Si cantRecaudada >= 0 Entonces
				cantTotal = cantTotal + cantRecaudada
			SiNo
				Escribir "Ingrese un número mayor o igual a 0"
				Leer cantRecaudada
				Mientras cantRecaudada >= 0
					Escribir "Ingrese un número mayor o igual a 0"
					Leer cantRecaudada
				FinMientras
			FinSi
	FinPara
	Escribir "La cantidad recaudada es ", cantTotal
FinAlgoritmo

Funcion inicio(cant Por Referencia)
	cant = 0
FinFuncion

