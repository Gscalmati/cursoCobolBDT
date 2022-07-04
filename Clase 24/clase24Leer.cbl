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
           "C:\Users\Usuario\Desktop\Curso COBOL\Clase 24\arSocios.dat".
      *    Si uno no configura la ORGANIZACION, se toma como SECUENCIAL
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01 soc-reg.
           03 soc-id pic 9(4).
           03 soc-nombre pic x(15).
           03 soc-saldo pic s9(8)v99.
       WORKING-STORAGE SECTION.
       01  cont pic 999.
       01  flag pic 9 VALUE 1.
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
           DISPLAY soc-id, " ", soc-nombre, " ", soc-saldo.



       500-FIN.
           CLOSE SOCIOS.
           DISPLAY "La cantidad de socios es ", cont.


       END PROGRAM YOUR-PROGRAM-NAME.
