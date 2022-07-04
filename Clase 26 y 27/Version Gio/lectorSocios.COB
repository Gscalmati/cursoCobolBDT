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
       01  flag pic 9 VALUE 1.
       01  monto pic 9(10)v99 value 0.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO.
            PERFORM 200-LEER-REG.
            PERFORM UNTIL (flag = 0)

               PERFORM 300-PROCESO
               PERFORM 200-LEER-REG
            END-PERFORM.
            PERFORM 500-FIN.
            STOP RUN.

       100-INICIO.
           MOVE ZEROES TO cont.
           DISPLAY "Lector de Archivos Secuenciales de Salida 3000"
           OPEN INPUT SOCIOS.

       200-LEER-REG.
           READ SOCIOS AT END MOVE 0 TO flag.

       300-PROCESO.
           ADD 1 TO cont.
           if (soc-modalidad = "T")
               ADD soc-importe TO monto.
           DISPLAY soc-filial, " ", soc-socio, " ", soc-importe " ",
           soc-modalidad.



       500-FIN.
           CLOSE SOCIOS.
           DISPLAY "La cantidad de socios es ", cont.
           DISPLAY "Monto total ", monto.

       END PROGRAM YOUR-PROGRAM-NAME.
