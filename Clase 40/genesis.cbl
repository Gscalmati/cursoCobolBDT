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
           SELECT socios ASSIGN TO "..\socios.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS soc-cod.

       DATA DIVISION.
       FILE SECTION.
       FD  socios.
       01  soc-reg.
           03 soc-cod pic 999.
           03 soc-nom pic x(20).
           03 soc-saldo pic 9(6)v99.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN OUTPUT SOCIOS.
            CLOSE SOCIOS.
       END PROGRAM YOUR-PROGRAM-NAME.
