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
           SELECT NOVEDADES ASSIGN TO
           "..\novedades.dat".
      *    Si uno no configura la ORGANIZACION, se toma como SECUENCIAL
       DATA DIVISION.
       FILE SECTION.
       FD  NOVEDADES.
       01  nov-reg.
           03 nov-socio pic 9999.
           03 nov-mov pic x.
           03 nov-nombre pic x(20).
           03 nov-importe pic s9(7)v99.
       WORKING-STORAGE SECTION.
       01  cont pic 999.
       01  flag pic 9 VALUE 1.
       01  monto pic 9(10)v99 value 0.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO.
            PERFORM 200-LEER-REG.
            PERFORM UNTIL (flag = 0)

               PERFORM 300-PROCESO
               PERFORM 200-LEER-REG
            END-PERFORM.
            PERFORM 400-FIN.
            STOP RUN.

       100-INICIO.
           MOVE ZEROES TO cont.
           DISPLAY "Lector de Archivos Secuenciales de Salida 3000"
           OPEN INPUT NOVEDADES.

       200-LEER-REG.
           READ NOVEDADES AT END MOVE 0 TO flag.

       300-PROCESO.
           DISPLAY nov-socio, " ", nov-mov, " ", nov-nombre " ",
           nov-importe.

       400-FIN.
           CLOSE NOVEDADES.

       END PROGRAM YOUR-PROGRAM-NAME.
