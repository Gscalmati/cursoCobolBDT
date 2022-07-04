      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  linea1.
           03 filler  pic x(30) value spaces.
           03 filler  pic x(19) value "Banco: EL CORRALITO".
           03 filler  pic x(31) value spaces.
       01  linea2.
           03 filler pic x(33) value spaces.
           03 filler pic x(10) values "Sucursal: ".
           03 l-suc  pic zz9.
           03 filler pic x(34) values spaces.
       01  suc pic 999.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           display "Ingrese nro. sucursal".
           accept suc.
            DISPLAY linea1.
            move suc to l-suc.
            display linea2.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
