      ********************************************************************
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
      *    LINEAS DE IMPRESIÓN
       01  linea1.
           03 filler pic x(30) value SPACES.
           03 filler pic x(19) value "Banco: El CORRALITO".
           03 filler pic x(31) values SPACES.
       01  linea2.
           03 filler pic x(33) values SPACES.
           03 filler pic x(10) values "Sucursal: ".
           03 l-Suc pic zz9.
           03 filler pic x(34) values SPACES.
       01  linea3.
           03 filler pic x(22) value SPACES.
           03 filler pic x(36) value
           "Listado de Socios con Saldo Negativo".
           03 filler pic x(22) values SPACES.
      *    VARIABLES
       01  suc pic s9(4).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            perform inicio.

            STOP RUN.

       inicio.

            PERFORM ingresoSuc.
            DISPLAY linea1.
            MOVE suc TO l-Suc.
            DISPLAY linea2.
            DISPLAY linea3.
       ingresoSuc.
           DISPLAY "Ingrese num. de sucursal"
           ACCEPT suc.
           perform until (suc >= 1) and (suc <= 999)
               DISPLAY 'Ingrese un valor de sucursal entre 1 y 999'
               ACCEPT suc
           END-PERFORM.

       END PROGRAM YOUR-PROGRAM-NAME.
