******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
       SELECT FILIAL ASSIGN TO "../filiales.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  FILIAL.
       01  fil-reg.
           03 fil-cod pic 9.
           03 fil-nombre pic x(20).
       WORKING-STORAGE SECTION.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-INGRESO-FIL.
           PERFORM UNTIL fil-cod = 0
              PERFORM 300-INGRESO-RESTO
              PERFORM 400-PROCESO
              PERFORM 200-INGRESO-FIL
           END-PERFORM.
           PERFORM 500-FIN.
            STOP RUN.

       100-INICIO.
           OPEN OUTPUT FILIAL.

       200-INGRESO-FIL.
           DISPLAY "Ingrese filial (0 para finalizar)".
           ACCEPT fil-cod.

       300-INGRESO-RESTO.
           DISPLAY "Ingrese nombre".
           ACCEPT fil-nombre.

       400-PROCESO.
           WRITE fil-reg.

       500-FIN.
           CLOSE FILIAL.


       END PROGRAM YOUR-PROGRAM-NAME.
