      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    Asi seleccionamos un archivo SECUENCIAL
           SELECT SOCIOS ASSIGN TO
           "..\socios.dat".
      *    Si uno no configura la ORGANIZACION, se toma como SECUENCIAL
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01 soc-reg.
           03 soc-filial pic 99.
           03 soc-socio pic 99999.
           03 soc-importe pic s9(8)v99.
           03 soc-modalidad pic x(1).
       WORKING-STORAGE SECTION.
       01  cont pic 999.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO.
            PERFORM 200-ING-SOCIO.
            PERFORM UNTIL (soc-filial = 0)
               PERFORM 300-ING-RESTO
               PERFORM 400-PROCESO
               PERFORM 200-ING-SOCIO
            END-PERFORM.
            PERFORM 500-FIN.
            STOP RUN.

       100-INICIO.
           MOVE ZEROES TO cont.
           DISPLAY "Grabador de Archivos Secuenciales de Salida 3000"
           OPEN OUTPUT SOCIOS.

       200-ING-SOCIO.
           DISPLAY "Ingrese Numero de Filial".
           ACCEPT soc-filial.
           PERFORM 205-VALIDAR-FILIAL.

       205-VALIDAR-FILIAL.
           PERFORM UNTIL (soc-filial >= 0 and soc-filial <= 9)
               DISPLAY "Error. Inserte un Codigo de filial Valido"
               ACCEPT soc-filial
           END-PERFORM.

       300-ING-RESTO.
           PERFORM 320-ING-SOCIO.
           PERFORM 330-ING-MONTO.
           PERFORM 340-ING-OPERACION.

       320-ING-SOCIO.
           DISPLAY "Ingrese numero de socio".
           ACCEPT soc-socio.
           PERFORM 325-VALIDAR-NUM-SOC.

       325-VALIDAR-NUM-SOC.
           PERFORM UNTIL (soc-socio > 0 and soc-socio <= 9999)
               DISPLAY "Error. Inserte un Numero de socio valido"
               ACCEPT soc-socio
           END-PERFORM.

       330-ING-MONTO.
           DISPLAY "Ingrese monto de operacion".
           ACCEPT soc-importe.

       340-ING-OPERACION.
           DISPLAY "Ingrese modalidad de operacion".
           DISPLAY "D: Tarj. Debito / E: Efectivo / T: Transferencia".
           ACCEPT soc-modalidad.
           PERFORM 345-VALIDAR-MODALIDAD.

       345-VALIDAR-MODALIDAD.
           PERFORM UNTIL (soc-modalidad = "T" or soc-modalidad = "E"
               or soc-modalidad = "D")
               DISPLAY "Error. Inserte una modalidad valida"
               DISPLAY "D: Tarj. Debito / E: Efectivo / T: Transf."
               ACCEPT soc-modalidad
           END-PERFORM.

       400-PROCESO.
           ADD 1 TO cont.
           WRITE soc-reg.


       500-FIN.
           CLOSE SOCIOS.
           DISPLAY "La cantidad de socios es ", cont.


       END PROGRAM YOUR-PROGRAM-NAME.
