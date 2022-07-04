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
           SELECT FILIAL ASSIGN TO "../filiales.txt"
                         ORGANIZATION IS LINE SEQUENTIAL.
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
            PERFORM 200-LEER-FILIAL.
            PERFORM 300-MOSTRAR-CABECERA.
            PERFORM UNTIL w-flag-soc = 1
               PERFORM 400-PROCESO-SOCIO
               PERFORM 200-LEER-FILIAL
            END-PERFORM.
            PERFORM 500-FIN.
            STOP RUN.

       100-INICIO.
           OPEN INPUT FILIAL.

       200-LEER-FILIAL.
           READ FILIAL AT END MOVE 1 TO w-flag-soc.

       300-MOSTRAR-CABECERA.
           DISPLAY "Codigo Nombre".

       400-PROCESO-SOCIO.
           DISPLAY "   ", fil-cod, "   ", fil-nombre.

       500-FIN.
           CLOSE FILIAL.

       END PROGRAM YOUR-PROGRAM-NAME.
