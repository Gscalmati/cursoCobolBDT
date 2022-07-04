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
       SELECT FILIAL ASSIGN TO "../filiales.dat"
               ACCESS MODE IS RANDOM
               ORGANIZATION IS INDEXED
               RECORD KEY IS fil-cod.
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
              PERFORM 400-PROCESO THRU 400-FIN-PROCESO
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
           WRITE fil-reg
               INVALID KEY
                   DISPLAY "Error en la grabacion"
                   GO TO 400-FIN-PROCESO
               NOT INVALID KEY
                   DISPLAY "Filial grabada correctamente".

       400-FIN-PROCESO.
           EXIT.

       500-FIN.
           CLOSE FILIAL.


       END PROGRAM YOUR-PROGRAM-NAME.
