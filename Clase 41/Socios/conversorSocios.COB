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
           SELECT SOCIOS-SEC ASSIGN TO "../sociosSec.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SOCIOS-INDEX ASSIGN TO "../sociosIndex.dat"
           ORGANIZATION IS INDEXED
           ACCESS IS SEQUENTIAL
           RECORD KEY IS soc-ind-cod.


       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS-SEC.
       01  reg-soc-sec.
           03 reg-cod-sec.
               05 soc-sec-fil pic 9.
               05 soc-sec-cod pic 999.
           03 soc-sec-nom pic x(15).
           03 soc-sec-saldo pic s9(6)v99.

       FD  SOCIOS-INDEX.
       01  reg-soc-ind.
           03 reg-cod-ind.
               05 soc-ind-fil pic 9.
               05 soc-ind-cod pic 999.
           03 soc-ind-nom pic x(15).
           03 soc-ind-saldo pic s9(6)v99.
       WORKING-STORAGE SECTION.

       01  flag-sec pic 9 value 1.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO.
            PERFORM 200-TRAER-FIL.
            PERFORM UNTIL flag-sec = 0
               PERFORM 300-CONVERTIR
               PERFORM 200-TRAER-FIL
            END-PERFORM.
            PERFORM 400-FIN.
           STOP RUN.

           100-INICIO.
               OPEN INPUT SOCIOS-SEC.
               OPEN OUTPUT SOCIOS-INDEX.

           200-TRAER-FIL.
               READ SOCIOS-SEC AT END MOVE 0 TO flag-sec.
      *         DISPLAY reg-fil-sec.

           300-CONVERTIR.
               MOVE reg-soc-sec TO reg-soc-ind.
               WRITE reg-soc-ind INVALID KEY
                                       DISPLAY "ERROR - Grabacion"
                                DISPLAY soc-ind-cod, " - ", soc-ind-nom.
           400-FIN.
               CLOSE SOCIOS-SEC.
               CLOSE SOCIOS-INDEX.
       END PROGRAM YOUR-PROGRAM-NAME.
