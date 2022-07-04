      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT SOCIOS ASSIGN TO "../socios.dat"
               ACCESS IS DYNAMIC
               ORGANIZATION IS INDEXED
               RECORD KEY IS soc-clave.

       SELECT FILIAL ASSIGN TO "../filiales.dat"
               ACCESS IS RANDOM
               ORGANIZATION IS INDEXED
               RECORD KEY IS fil-cod.
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD  SOCIOS.
       01  soc-reg.
           03 soc-clave.
               05 soc-filial pic 99.
               05 soc-socio pic 999.
           03 soc-nombre pic x(15).
           03 soc-saldo pic 9999v99.

       FD  FILIAL.
       01  fil-reg.
           03 fil-cod pic 9.
               88 fin-programa value 0.
           03 fil-nombre pic x(20).

       WORKING-STORAGE SECTION.
        01 w-flag-soc pic 9.
           88 fin-socio value 1.
        01 w-act-fil pic 9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-INGRESO-CLAVE.
           PERFORM UNTIL fin-programa
               PERFORM 300-PROCESO-DATOS
               PERFORM 200-INGRESO-CLAVE
           END-PERFORM.
           PERFORM 400-FIN.
           STOP RUN.

       100-INICIO.
           OPEN INPUT SOCIOS
                      FILIAL.
           MOVE ZERO TO w-flag-soc.

       200-INGRESO-CLAVE.
           DISPLAY "Ingrese nro de filial (0 para finalizar)".
           ACCEPT fil-cod.

       300-PROCESO-DATOS.
           DISPLAY "Voy a leer la filial".
           READ FILIAL
               INVALID KEY
                   PERFORM 310-ERROR
               NOT INVALID KEY
                   DISPLAY "Acabo de leer: "
                   DISPLAY fil-cod
                   PERFORM 320-PROCESO-SOCIOS THRU 320-F-PROCESO-SOCIOS
           END-READ.

       310-ERROR.
           DISPLAY "Filial no encontrada".

       320-PROCESO-SOCIOS.
           PERFORM 325-INICIO-SOCIO.
           PERFORM 330-MUESTRO-CABEC.
           START SOCIOS KEY IS > soc-clave
               INVALID KEY
                  DISPLAY "No hay socios en esta filial"
                  GO TO 320-F-PROCESO-SOCIOS
           END-START.
           PERFORM 350-ACT-FILIAL
           PERFORM 360-LEER-SOC
           PERFORM UNTIL soc-filial <> w-act-fil OR fin-socio
               PERFORM 370-MUESTRO-DETALLE
               PERFORM 360-LEER-SOC
           END-PERFORM.

       320-F-PROCESO-SOCIOS.
           EXIT.

       325-INICIO-SOCIO.
           MOVE fil-cod TO soc-filial.
           MOVE ZERO TO soc-socio.

       360-LEER-SOC.
           READ SOCIOS NEXT AT END MOVE 1 TO w-flag-soc.

      * 330-BUSCAR-FILIAL.
      *     READ FILIAL.

       330-MUESTRO-CABEC.
           DISPLAY "      Filial: ", fil-nombre, " - ", fil-cod.
           DISPLAY "SOCIO NOMBRE           SALDO".

       350-ACT-FILIAL.
           MOVE fil-cod TO w-act-fil.

       370-MUESTRO-DETALLE.
           DISPLAY soc-socio "   " soc-nombre " " soc-saldo.
           DISPLAY soc-reg.

       400-FIN.
           CLOSE SOCIOS
                 FILIAL.

       END PROGRAM YOUR-PROGRAM-NAME.
