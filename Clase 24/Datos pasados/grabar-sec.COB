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
           SELECT SOCIOS
               ASSIGN TO "D:\Datos\Cobol\2022-05-05\socios.dat".
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01  soc-reg.
           03 soc-id     pic 9(4).
           03 soc-nombre pic x(15).
           03 soc-saldo  pic s9(8)V99.
       WORKING-STORAGE SECTION.
       01  cont pic 999 value zeros.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-ING-SOCIO
           PERFORM UNTIL soc-id = 0
             PERFORM 300-INGRESO-RESTO
             PERFORM 400-PROCESO
             PERFORM 200-ING-SOCIO
           END-PERFORM.
           PERFORM 500-FIN.
            STOP RUN.
      *************************** R U T I N A S ***********************
       100-INICIO.
           display "Programa para grabar un archivo de salida".
           OPEN OUTPUT SOCIOS.
       200-ING-SOCIO.
           Display "Ingrese nro. socio, x fin ingrese 0".
           accept soc-id.
       300-INGRESO-RESTO.
           PERFORM 320-ING-NOMBRE.
           PERFORM 340-ING-SALDO.
       320-ING-NOMBRE.
           display "Ingrese el nombre del socio".
           accept soc-nombre.
       340-ING-SALDO.
           display "Ingrese el saldo"
           accept soc-saldo.
       400-PROCESO.
           add 1 to cont.
           WRITE soc-reg.
       500-FIN.
           CLOSE SOCIOS.
           display "se procesaron ", cont, " socios".
       END PROGRAM YOUR-PROGRAM-NAME.
