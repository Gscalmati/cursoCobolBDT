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
           SELECT SOCIOS ASSIGN TO
           "..\socios.dat".
      *    Si uno no configura la ORGANIZACION, se toma como SECUENCIAL
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01 soc-reg.
           03 soc-socio pic 99999.
           03 soc-nombre pic x(20).
           03 soc-importe pic s9(8)v99.

       WORKING-STORAGE SECTION.
       01  cont pic 999.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO.
            PERFORM 200-ING-SOCIO.
            PERFORM UNTIL (soc-socio = 0)
               PERFORM 300-ING-RESTO
               PERFORM 400-PROCESO
               PERFORM 200-ING-SOCIO
            END-PERFORM.
            PERFORM 500-FIN.
            STOP RUN.

       100-INICIO.
           MOVE ZEROES TO cont.
           DISPLAY "Grabador de Archivos Secuenciales de Salida 3000"
           OPEN OUTPUT SOCIOS.

       200-ING-SOCIO.
           DISPLAY "Ingrese Numero de Socio".
           ACCEPT soc-socio.
           PERFORM 205-VALIDAR-FILIAL.

       205-VALIDAR-FILIAL.
           PERFORM UNTIL (soc-socio >= 0 and soc-socio <= 9999)
               DISPLAY "Error. Inserte un Codigo de filial Valido"
               ACCEPT soc-socio
           END-PERFORM.

       300-ING-RESTO.
           PERFORM 320-ING-NOMBRE.
           PERFORM 330-ING-SALDO.

       320-ING-NOMBRE.
           DISPLAY "Ingrese nombre de socio".
           ACCEPT soc-nombre.
           PERFORM 325-VALIDAR-NOM-SOC.

       325-VALIDAR-NOM-SOC.
           PERFORM UNTIL (soc-nombre <> " ")
               DISPLAY "Error. Inserte un Nombre de socio valido"
               ACCEPT soc-nombre
           END-PERFORM.

       330-ING-SALDO.
           DISPLAY "Ingrese saldo del socio".
           ACCEPT soc-importe.

       400-PROCESO.
           ADD 1 TO cont.
           WRITE soc-reg.


       500-FIN.
           CLOSE SOCIOS.
           DISPLAY "La cantidad de socios es ", cont.


       END PROGRAM YOUR-PROGRAM-NAME.
