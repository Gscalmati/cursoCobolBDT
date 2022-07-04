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
           SELECT SOCIOS-ACTUAL ASSIGN TO
           "..\sociosActual.dat".
      *    Si uno no configura la ORGANIZACION, se toma como SECUENCIAL
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01 soc-reg.
           03 soc-socio pic 99999.
           03 soc-nombre pic x(20).
           03 soc-importe pic s9(8)v99.
       FD  SOCIOS-ACTUAL.
       01 soc-reg-act.
           03 soc-socio-act pic 99999.
           03 soc-nombre-act pic x(20).
           03 soc-importe-act pic s9(8)v99.
       WORKING-STORAGE SECTION.
       01  cont pic 999.
       01  flag pic 9 VALUE 1.
       01  monto pic 9(10)v99 value 0.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 1100-INICIO.
            PERFORM 1200-LEER-REG.
            PERFORM UNTIL (flag = 0)

               PERFORM 1300-PROCESO
               PERFORM 1200-LEER-REG
            END-PERFORM.
            PERFORM 1400-FIN.
            STOP RUN.

       100-INICIO.
           MOVE ZEROES TO cont.
           DISPLAY "Lector de Archivos Secuenciales de Salida 3000"
           OPEN INPUT SOCIOS.

       200-LEER-REG.
           READ SOCIOS AT END MOVE 0 TO flag.

       300-PROCESO.
           DISPLAY soc-socio, " ", soc-nombre, " ", soc-importe " ".

       400-FIN.
           CLOSE SOCIOS.


       1100-INICIO.
           MOVE ZEROES TO cont.
           DISPLAY "Lector de Archivos Secuenciales de Salida 3000"
           OPEN INPUT SOCIOS-ACTUAL.

       1200-LEER-REG.
           READ SOCIOS-ACTUAL AT END MOVE 0 TO flag.

       1300-PROCESO.
           DISPLAY soc-socio-act, " ",
           soc-nombre-act, " ", soc-importe-act " ".

       1400-FIN.
           CLOSE SOCIOS-ACTUAL.

       END PROGRAM YOUR-PROGRAM-NAME.
