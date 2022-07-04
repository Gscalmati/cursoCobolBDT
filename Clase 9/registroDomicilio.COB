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
       01  datosPers.
           02  nombre.
               03 nom pic x(10).
               03 ape pic x(10).
           02  domicilio.
               03 calle pic x(20) VALUE "Juan B Aberdi".
               03 num pic 9(05) VALUE 261.
               03 piso pic 99.
               03 depto pic xx.
           02  localidad pic x(15).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY domicilio
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
