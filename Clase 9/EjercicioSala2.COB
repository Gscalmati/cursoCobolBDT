      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EjercicioSala2.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  i pic 99.
      *variablesIngreso
       77  nombreIng   pic A(15).
       77  montoIng    pic s999999v99.
       77  mesIng      pic s9(2).
       77  cantCumpIng pic s9(2).
      *variables General
       77  genMontoTotal pic 999999v99.
       77  genMaxMonto   pic 99999999v99.
       77  genNombMax    pic A(15).
      *variables Cumpleanieros
       77  cumplCantRecaud pic s9999999v99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM inicioGeneral.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > cantCumpIng
               PERFORM inicioCumpleaniero
               PERFORM ingresos
               PERFORM UNTIL montoIng = 0
                   PERFORM procesoMonto
                   PERFORM ingresoMonto
               END-PERFORM
               PERFORM finCumpleaniero
           END-PERFORM
           PERFORM finGeneral.
           STOP RUN.

      *    inicializoVariablesGeneral
       inicializoVariablesGeneral.
           MOVE ZEROS TO genMontoTotal.
           MOVE ZEROS TO genMaxMonto.
           MOVE " " TO genNombMax.
      *    validoMes
       validoMes.
           DISPLAY "mes Incorrecto!!.Ingrese un mes del año(1 a 12)".
           ACCEPT mesIng.
      *    ingresoMes
       ingresoMes.
           DISPLAY "Ingrese un mes del anio(1 a 12)".
           ACCEPT mesIng.
           PERFORM validoMes UNTIL mesIng >= 1 AND mesIng <= 12.
      *     validoCantidadCumpleanieros
       validoCantidadCumpleanieros.
           DISPLAY "mes incorrecto!!.Ingrese la cantidad de"
           " cumpleanieros que cumplen anios en el mes.(1 a 35)".
           ACCEPT cantCumpIng.
      *    ingresoCantidadCumpleanieros
       ingresoCantidadCumpleanieros.
           DISPLAY "Ingrese la cantidad de cumpleanieros que cumplen"
           " anios en el mes ",mesIng.
           ACCEPT cantCumpIng.
           PERFORM validoCantidadCumpleanieros UNTIL cantCumpIng >= 1
           AND cantCumpIng <= 35.
      *    inicioGeneral
       inicioGeneral.
           PERFORM inicializoVariablesGeneral.
           PERFORM ingresoMes.
           PERFORM ingresoCantidadCumpleanieros.
      *    inicializoVariablesCumpleaniero
       inicializoVariablesCumpleaniero.
           MOVE ZEROS TO cumplCantRecaud.
      *    inicioCumpleaniero
       inicioCumpleaniero.
           PERFORM inicializoVariablesCumpleaniero.
      *    ingresoNombreCumpleaniero
       ingresoNombreCumpleaniero.
           DISPLAY "Ingrese el nombre del cumplaniero"
           ACCEPT nombreIng.
      *    validoMonto
       validoMonto.
           DISPLAY "Error!!.Cuanto desea aportar para el cumpleanios"
           "de ",nombreIng, " (0 para finalizar)".
           ACCEPT montoIng.
      *    ingresoMonto
       ingresoMonto.
           DISPLAY "Cuanto desea aportar para el cumpleanios de ",
           nombreIng, " (0 para finalizar)"
           ACCEPT montoIng.
           PERFORM validoMonto UNTIL montoIng >= 0.
      *    ingresos
       ingresos.
           PERFORM ingresoNombreCumpleaniero.
           PERFORM ingresoMonto.
      *    procesoMonto
       procesoMonto.
           COMPUTE cumplCantRecaud = cumplCantRecaud + montoIng.
      *    mostrarDatosGeneral
       mostrarDatosGeneral.
           DISPLAY "La cantidad de dinero recaudado total es $",
           genMontoTotal.
           DISPLAY "El cumpleaniero que más dinero recaudo es ",
           genNombMax, "con $",genMaxMonto.
      *    mostrarDatosCumpleaniero
       mostrarDatosCumpleaniero.
           DISPLAY "El cumpleaniero ",nombreIng, " recaudo $",
           cumplCantRecaud.
      *    procesoVariablesGeneral
       procesoVariablesGeneral.
           COMPUTE genMontoTotal = genMontoTotal + cumplCantRecaud.
           IF cumplCantRecaud > genMaxMonto
               MOVE cumplCantRecaud TO genMaxMonto
               MOVE nombreIng TO genNombMax
           END-IF.
      *    finCumpleaniero
       finCumpleaniero.
           PERFORM procesoVariablesGeneral.
           PERFORM mostrarDatosCumpleaniero.
      *    finGeneral
       finGeneral.
           PERFORM mostrarDatosGeneral.
       END PROGRAM EjercicioSala2.
