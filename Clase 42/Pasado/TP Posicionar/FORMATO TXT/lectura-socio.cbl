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
           SELECT SOCIOS ASSIGN TO "../socios.txt"
                         ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01  soc-reg.
           03 soc-clave.
               05 soc-filial pic 9.
               05 soc-socio pic 999.
           03 soc-nombre pic x(15).
           03 soc-saldo pic 9999v99.

       WORKING-STORAGE SECTION.
       01  w-flag-soc pic 9 value 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO.
            PERFORM 200-LEER-SOCIO.
            PERFORM 300-MOSTRAR-CABECERA.
            PERFORM UNTIL w-flag-soc = 1
               PERFORM 400-PROCESO-SOCIO
               PERFORM 200-LEER-SOCIO
            END-PERFORM.
            PERFORM 500-FIN.
            STOP RUN.

       100-INICIO.
           OPEN INPUT SOCIOS.

       200-LEER-SOCIO.
           READ SOCIOS AT END MOVE 1 TO w-flag-soc.

       300-MOSTRAR-CABECERA.
           DISPLAY "Filial Codigo Nombre          Saldo".

       400-PROCESO-SOCIO.
           DISPLAY soc-filial, "      ", soc-socio, "    "              -
                   , soc-nombre, " ", soc-saldo.

       500-FIN.
           CLOSE SOCIOS.

       END PROGRAM YOUR-PROGRAM-NAME.
