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
           SELECT FILIALES ASSIGN TO
           "..\filiales.dat".
      *    Si uno no configura la ORGANIZACION, se toma como SECUENCIAL
       DATA DIVISION.
       FILE SECTION.
       FD  FILIALES.
       01  fil-reg.
           03 fil-codigo pic 99.
           03 fil-nombre pic x(15).
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
           OPEN INPUT FILIALES.

       200-LEER-REG.
           READ FILIALES AT END MOVE 0 TO flag.

       300-PROCESO.
           ADD 1 TO cont.
           DISPLAY fil-codigo, " ", fil-nombre.


       500-FIN.
           CLOSE FILIALES.
           DISPLAY "La cantidad de filiales es ", cont.


       END PROGRAM YOUR-PROGRAM-NAME.
