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
           SELECT SOCIOS ASSIGN TO "../socios.dat"
                         ACCESS MODE IS RANDOM
                         ORGANIZATION IS INDEXED
                         RECORD KEY IS soc-clave.
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
            PERFORM 200-PIDO-SOCIO.
            PERFORM UNTIL soc-socio = 0
               PERFORM 300-PROCESO-SOCIO
               PERFORM 200-PIDO-SOCIO
            END-PERFORM.
            PERFORM 400-FIN.
            STOP RUN.

       100-INICIO.
           OPEN INPUT SOCIOS.

       200-PIDO-SOCIO.
           DISPLAY "Ingrese codigo clave [filial+socio] (0 para finaliz"-
                   "ar)".
           ACCEPT soc-clave.

       300-PROCESO-SOCIO.
           READ SOCIOS
               INVALID KEY
                   DISPLAY "No se encontro dicho codigo"
               NOT INVALID KEY
                   DISPLAY soc-filial " " soc-socio " " soc-nombre,     -
                   "      " soc-saldo.
       400-FIN.
           CLOSE SOCIOS.

       END PROGRAM YOUR-PROGRAM-NAME.
