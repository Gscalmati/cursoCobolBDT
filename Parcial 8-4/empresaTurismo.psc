// Crear un programa para una empresa de turismo, la misma empresa necesita registrar sus empleados
// y sus sueldos por oficina. Se sabe que en cada oficina trabajan 10 empleados y que existen al menos 1 oficina. 
// Al finalizar la oficina se debe consultar si queda otra por ingresar. 
// Informar: POR OFICINA, el empleado con el sueldo más alto, el empleado con el más bajo 
// y el promedio de sueldos para los empleados y las empleadas.
// POR EMPRESA: la oficina con el mejor promedio de sueldo de las empleadas y con la menor cantidad de empleadas por piso.
Algoritmo empresaTurismo
	inicioEmpresa(mejorPromedioEmpleadas, oficinaMejorEmpleadas, menorCantEmpleadas, oficinaCantEmpleadas)
	Repetir
		inicioOficina
		Para i	<-1 Hasta cantEmpleados Hacer
			ingresoEmpleado(nomEmpleado, codEmpleado, sueldo)
			procesarEmpleado(nomEmpleado, codEmpleado, sueldo, cantEmpleados, cantEmpleadas, sueldoEmpleados, sueldoEmpleadas)
		FinPara
		finalOficina(nomEmpleadoMenorSueldo, menorSueldo, nomEmpleadoMenorSueldo, menorSueldo, promedioEmpleados, promedioEmpleadas)
	Hasta Que respuesta = "No" o respuesta = "NO"
	finalEmpresa(mejorPromedioEmpleadas, oficinaMejorPromedio, menorCantEmpleadas, oficinaCantEmpleadas)
FinAlgoritmo
