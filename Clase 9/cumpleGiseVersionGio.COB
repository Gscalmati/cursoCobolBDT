      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EjercicioClase9.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  cantCumpleaneros pic 9(2).
       01  totalRecaudado pic 9(10).
       01  nombreCumpleanero pic A(10).
       01  monto1Estudiante pic S9(5).
       01  total1Estudiante pic 9(6).
       01  i pic 9(2).
       01  nombreCumpleaneroMax pic A(10).
       01  montoCumpleaneroMax pic S9(6) VALUE -999.



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM inicioCumpleaneros.

           PERFORM procesoCumpleaneros VARYING i FROM 1 BY 1
           UNTIL i > cantCumpleaneros.

           PERFORM finalCumpleaneros.
           STOP RUN.

      *****PROCESOS PARA TODOS LOS CUMPLEANEROS
       inicioCumpleaneros.
           DISPLAY "Acumulador de dinero para cumpleanieros 3000"
           MOVE ZEROES TO totalRecaudado.
           DISPLAY "Ingrese cuantos cumpleanieros hay en el mes"
           ACCEPT cantCumpleaneros
           PERFORM validarTotalEst UNTIL (cantCumpleaneros > 0) and
           (cantCumpleaneros <= 35).

       validarTotalEst.
           DISPLAY "La cant. debe ser al menos 1 y maximo 35"
           ACCEPT cantCumpleaneros.

       procesoCumpleaneros.
           PERFORM inicio1Estudiante.
           PERFORM ingreso1Estudiante.
           PERFORM proceso1Estudiante UNTIL (monto1Estudiante = 0).
           PERFORM fin1Estudiante.

       finalCumpleaneros.
           DISPLAY "El total recaudado para los ", cantCumpleaneros,
           " cumpleaneros es ", totalRecaudado.
           DISPLAY "------------------------------------------------"
           DISPLAY "Al cumpleanero que mas aportaron fue a ",
           nombreCumpleanero, " con un total de ", montoCumpleaneroMax.


      *****PROCESOS PARA 1 ESTUDIANTE
       inicio1Estudiante.
           DISPLAY "Ingrese nombre del cumpleaniero".
           ACCEPT nombreCumpleanero.
           MOVE ZEROES TO total1Estudiante.

       ingreso1Estudiante.
           DISPLAY
           "Ingrese cuanto desea aportar para el cumpleaniero ", i.
           ACCEPT monto1Estudiante
           perform validarMonto1Est UNTIL monto1Estudiante >= 0.

       validarMonto1Est.
           DISPLAY "La cant. debe ser mayor a 0"
           ACCEPT monto1Estudiante.

       proceso1Estudiante.
           COMPUTE
           total1Estudiante = total1Estudiante + monto1Estudiante.
           PERFORM ingreso1Estudiante.

       fin1Estudiante.
           DISPLAY "El total total recaudado para el estudiante ", i,
           " es ", total1Estudiante.

           IF total1Estudiante > montoCumpleaneroMax
               MOVE nombreCumpleanero TO nombreCumpleaneroMax
               MOVE total1Estudiante TO montoCumpleaneroMax
           END-IF.
           COMPUTE totalRecaudado = totalRecaudado + total1Estudiante.

           END PROGRAM EjercicioClase9.
