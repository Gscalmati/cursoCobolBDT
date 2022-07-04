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
       01  w-flag-soc pic 9 value 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO.
            PERFORM 200-PIDO-FILIAL.
            PERFORM UNTIL fil-cod = 0
               PERFORM 300-PROCESO-FILIAL
               PERFORM 200-PIDO-FILIAL
            END-PERFORM.
            PERFORM 400-FIN.
            STOP RUN.

       100-INICIO.
           OPEN INPUT FILIAL.

       200-PIDO-FILIAL.
           DISPLAY "Ingrese codigo de filial (0 para finalizar)".
           ACCEPT fil-cod.

       300-PROCESO-FILIAL.
           READ FILIAL
               INVALID KEY
                   DISPLAY "No se encontro dicho codigo"
               NOT INVALID KEY
                   DISPLAY fil-cod, " ", fil-nombre.
       400-FIN.
           CLOSE FILIAL.

       END PROGRAM YOUR-PROGRAM-NAME.
