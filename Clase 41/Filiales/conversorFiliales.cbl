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
           SELECT FILIALES-SEC ASSIGN TO "../filialesSec.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT FILIALES-INDEX ASSIGN TO "../filialesIndex.dat"
           ORGANIZATION IS INDEXED
           ACCESS IS SEQUENTIAL
           RECORD KEY IS fil-ind-cod.


       DATA DIVISION.
       FILE SECTION.
       FD  FILIALES-SEC.
       01  reg-fil-sec.
           03 fil-sec-cod pic 99.
           03 fil-sec-nom pic x(15).

       FD  FILIALES-INDEX.
       01  reg-fil-ind.
           03 fil-ind-cod pic 99.
           03 fil-ind-nom pic x(15).

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
               OPEN INPUT FILIALES-SEC.
               OPEN OUTPUT FILIALES-INDEX.

           200-TRAER-FIL.
               READ FILIALES-SEC AT END MOVE 0 TO flag-sec.
      *         DISPLAY reg-fil-sec.

           300-CONVERTIR.
               MOVE reg-fil-sec TO reg-fil-ind.
               WRITE reg-fil-ind INVALID KEY
                                       DISPLAY "ERROR - Grabacion"
                                DISPLAY fil-ind-cod, " - ", fil-ind-nom.
           400-FIN.
               CLOSE FILIALES-SEC.
               CLOSE FILIALES-INDEX.
       END PROGRAM YOUR-PROGRAM-NAME.
