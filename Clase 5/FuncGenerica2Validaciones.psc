Algoritmo FuncGenerica
	
	Escribir func2validaciones(50, "El número no se encuentra en ese rango", "El número esta entre 50 y 100", 50, 100)
	
FinAlgoritmo

funcion returnFuncion <- func2validaciones (num, error, respuesta, validacion1, validacion2)
	definir returnFuncion como cadena
	Si (num >= validacion1) y (num < validacion2)
		returnFuncion <- respuesta
	SiNo
		returnFuncion <- error
	FinSi
FinFuncion