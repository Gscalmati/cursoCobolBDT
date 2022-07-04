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
           01 fecha.
               03 anio pic 99.
               03 mes pic 99.
               03 dia pic 99.
           01 l-fecha.
               03 filler pic x(14) value "Fecha de hoy:".
               03 l-dia-sem pic x(10).
               03 l-dia pic 99.
               03 filler pic xxxxx value " de ".
               03 l-mes pic x(10).
               03 filler pic xxxxx value " del ".
               03 filler pic 99 value 20.
               03 l-anio pic 99.
           01 dia-sem pic 9.
           01 dia-de-semana.
               03 filler pic x(9) value "Lunes".
               03 filler pic x(9) value "Martes".
               03 filler pic x(9) value "Miercoles".
               03 filler pic x(9) value "Jueves".
               03 filler pic x(9) value "Viernes".
               03 filler pic x(9) value "Sabado".
               03 filler pic x(9) value "Domingo".
           01 vec-sem REDEFINES dia-de-semana OCCURS 7 TIMES.
               03 vec-dia-sem pic x(9).
           01 mes-del-anio.
               03 filler pic x(10) value "Enero".
               03 filler pic x(10) value "Febrero".
               03 filler pic x(10) value "Marzo".
               03 filler pic x(10) value "Abril".
               03 filler pic x(10) value "Mayo".
               03 filler pic x(10) value "Junio".
               03 filler pic x(10) value "Julio".
               03 filler pic x(10) value "Agosto".
               03 filler pic x(10) value "Septiembre".
               03 filler pic x(10) value "Octubre".
               03 filler pic x(10) value "Noviembre".
               03 filler pic x(10) value "Diciembre".
           01 vec-mes REDEFINES mes-del-anio OCCURS 12 TIMES.
               03 vec-mes-anio pic x(10).
           01 hora pic 9(8).
           01 var1 pic 99 value 4.
           01 var2 pic 99 value 8.
           01 var3 pic 99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *      ACCEPT hora FROM TIME.
      *      DISPLAY hora.
      *      COMPUTE var3 = (var1 + var2) * var2.
      *      DISPLAY var3.
            ADD var1 TO var2 GIVING var3.
            DISPLAY var2.
            DISPLAY var3.
       END PROGRAM YOUR-PROGRAM-NAME.
