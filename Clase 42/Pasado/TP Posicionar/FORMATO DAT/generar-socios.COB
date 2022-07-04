      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INDEXADOS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOCIOS ASSIGN TO "../socios.dat"
                         ORGANIZATION IS INDEXED
                         ACCESS MODE IS RANDOM
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
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO.
            PERFORM 200-INGRESO-FIL.
            PERFORM UNTIL soc-filial = 0
               PERFORM 300-INGRESO-RESTO
               PERFORM 400-PROCESO THRU 400-FIN-PROCESO
               PERFORM 200-INGRESO-FIL
            END-PERFORM.
            PERFORM 500-FIN.
            STOP RUN.

       100-INICIO.
           OPEN OUTPUT SOCIOS.

       200-INGRESO-FIL.
           DISPLAY "Ingrese el codigo de filial (por fin, ingrese 0)".
           ACCEPT soc-filial.

       300-INGRESO-RESTO.
           PERFORM 340-INGRESO-CODIGO.
           PERFORM 350-INGRESO-NOM.
           PERFORM 360-INGRESO-SUELDO.

       340-INGRESO-CODIGO.
           DISPLAY "Ingrese codigo de socio".
           ACCEPT soc-socio.

       350-INGRESO-NOM.
           DISPLAY "Ingrese el nombre del socio"
           ACCEPT soc-nombre.

       360-INGRESO-SUELDO.
           DISPLAY "Ingrese sueldo".
           ACCEPT soc-saldo.

       400-PROCESO.
           WRITE soc-reg
               INVALID KEY
                   DISPLAY "Error en la grabacion"
                   GO TO 400-FIN-PROCESO
               NOT INVALID KEY
                   DISPLAY "Grabado correcto".

       400-FIN-PROCESO.
           EXIT.

       500-FIN.
           CLOSE SOCIOS.

       END PROGRAM INDEXADOS.
