      ******************************************************************
      * Author: Ezequiel Galluzo, Matías Medela, Cecilia Olmos,
      *         Martin Perez
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Ejercicio_Sala2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILIALES
           ASSIGN TO '..\Archivos\filiales.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS fil-cod.

           SELECT SOCIOS
           ASSIGN TO '..\Archivos\socios.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS soc-llave.
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01  soc-reg.
           03 soc-llave.
               05 soc-filial pic 9999.
               05 soc-cod pic 9999.
           03 soc-nom pic x(20).
           03 soc-saldo pic s9(7)v99.

       FD  FILIALES.
       01  fil-reg.
           03 fil-cod pic 9999.
               88 fin-prog value 0.
           03 fil-nom pic x(20).

       WORKING-STORAGE SECTION.
      ******** lineas de impresion ********************************************
       01  lin-titulo-filial.
           03 filler pic x(23) value spaces.
           03 filler pic x(8) value "Filial:".
           03 l-fil-cod pic zzz9.
           03 filler pic x(3) value " - ".
           03 l-fil-nom pic x(20).

       01  lin-titulo-socio.
           03 filler pic x(5) value spaces.
           03 filler pic x(6) value "FILIAL".
           03 filler pic x(5) value spaces.
           03 filler pic x(8) value "SOC.COD.".
           03 filler pic x(12) value spaces.
           03 filler pic x(6) value "NOMBRE".
           03 filler pic x(12) value spaces.
           03 filler pic x(5) value "SALDO".

       01  lin-det-socio.
           03 filler pic x(6).
           03 l-soc-filial pic zzz9.
           03 filler pic x(8).
           03 l-soc-cod pic zzz9.
           03 filler pic x(5).
           03 l-soc-nom pic x(20).
           03 filler pic x(5).
           03 l-soc-saldo pic z.zzz.zz9,99.
       01  lin-no-existe.
           03 filler pic x(30) value all "-".
           03 filler pic x(16) value "NO EXISTE FILIAL".
           03 filler pic x(30) value all "-".
       01  lin-no-socio.
           03 filler pic x(15) value all "-".
           03 filler pic x(30) value "NO HAY SOCIOS PARA LA FILIAL: ".
           03 l-filial pic zzz9.
           03 filler pic x(15) value all "-".
       01  lin-cant-filial.
           03 filler pic x(7) value "TOTAL: ".
           03 filler pic x(12) value spaces.
           03 l-cant pic zz9.
           03 filler pic x(7) value " socios".
           03 filler pic x(23) value spaces.
           03 l-saldo-filial pic z.zzz.zz9,99.

       01  lin-fin.
           03 filler pic x(30) value all "*".
           03 filler pic x(18) value "FINALIZO EJECUCION".
           03 filler pic x(30) value all "*".
       01  lin-salto.
           03 filler pic x(80) value spaces.
       01  lin-guarda.
           03 filler pic x(80) value all "*".
       01  lin-guarda2.
           03 filler pic x(80) value all "-".

      *********variables de trabajo *******************************************
       77  w-fil-cod-ant pic 9999.
       77  w-existe-filial pic 9 VALUE ZERO.
       77  w-eof-socios pic 9 VALUE ZERO.
       77  w-band pic x.
       77  w-cant-soc-xfilial pic 9(3).
       77  w-saldo-xfilial pic s9(8)v99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO-GENERAL.
            PERFORM 200-INGRESO-FILIAL.
            PERFORM UNTIL fin-prog
               PERFORM 300-EVALUO-FILIAL-EN-SOCIO
               PERFORM 200-INGRESO-FILIAL
            END-PERFORM.
            PERFORM 800-FIN-GENERAL.
            STOP RUN.


      *********INICIO RUTINAS *******************************************

       100-INICIO-GENERAL.
           PERFORM 120-ABRIR-ARCHIVO.

       120-ABRIR-ARCHIVO.
           OPEN I-O SOCIOS.
           OPEN I-O FILIALES.

       200-INGRESO-FILIAL.
           PERFORM WITH TEST AFTER UNTIL fil-cod IS EQUALS ZERO OR
                                         w-existe-filial IS EQUALS 1
               PERFORM 220-PIDO-CODIGO-FILIAL-Y-EVALUO
           END-PERFORM.

       220-PIDO-CODIGO-FILIAL-Y-EVALUO.
           PERFORM 230-PIDO-CODIGO-FILIAL.
           IF fil-cod IS NOT EQUALS ZERO
               PERFORM 250-LEER-FILIAL.

       230-PIDO-CODIGO-FILIAL.
           DISPLAY lin-guarda.
           DISPLAY "INGRESE CODIGO DE FILIAL 0 (CERO) PARA TERMINAR: ".
           ACCEPT fil-cod.

       250-LEER-FILIAL.
           READ FILIALES INVALID KEY
                         DISPLAY lin-no-existe
                         MOVE ZERO TO w-existe-filial
                         NOT INVALID KEY
                         MOVE 1 TO w-existe-filial.

       300-EVALUO-FILIAL-EN-SOCIO.
           PERFORM 500-INICIO-FILIAL
           PERFORM 350-POSICIONO-EN-ARCHIVO-SOCIO
           IF w-existe-filial = 1 THEN
               PERFORM 400-LEER-SOCIO
                   PERFORM UNTIL w-eof-socios IS EQUALS 1 OR
                                 w-fil-cod-ant IS NOT EQUALS soc-filial
                       PERFORM 600-PROCESO-SOCIO
                       PERFORM 400-LEER-SOCIO
                   END-PERFORM.
           PERFORM 700-FIN-FILIAL.


       320-ENCABEZO-FILIAL.
           PERFORM 330-ARMO-LIN-TITULO-FILIAL.
           DISPLAY lin-titulo-filial.

       330-ARMO-LIN-TITULO-FILIAL.
           MOVE fil-cod TO l-fil-cod.
           MOVE fil-nom TO l-fil-nom.

       350-POSICIONO-EN-ARCHIVO-SOCIO.
           MOVE fil-cod TO soc-filial.
           MOVE zeros TO soc-cod.
           START SOCIOS KEY IS > soc-llave INVALID KEY
                                           MOVE ZERO TO w-existe-filial
                                           NOT INVALID KEY
                                           MOVE 1 TO w-existe-filial
           END-START.

       400-LEER-SOCIO.
           READ SOCIOS NEXT AT END MOVE 1 TO w-eof-socios.

       500-INICIO-FILIAL.
           MOVE ZERO TO w-eof-socios.
           PERFORM 520-INICIO-VARIABLES-FILIAL.
           PERFORM 320-ENCABEZO-FILIAL.
           PERFORM 550-ENCABEZAR-SOCIO.

       520-INICIO-VARIABLES-FILIAL.
           MOVE fil-cod TO w-fil-cod-ant.
           MOVE ZERO to w-cant-soc-xfilial.
           MOVE ZERO to w-saldo-xfilial.

       550-ENCABEZAR-SOCIO.
           DISPLAY lin-titulo-socio.

       600-PROCESO-SOCIO.
           PERFORM 650-ARMO-LIN-DET-SOCIO.
           PERFORM 670-PREPARO-TOTALES.

       650-ARMO-LIN-DET-SOCIO.
           MOVE soc-filial TO l-soc-filial.
           MOVE soc-cod TO l-soc-cod.
           MOVE soc-nom TO l-soc-nom.
           MOVE soc-saldo TO l-soc-saldo.
           DISPLAY lin-det-socio.

       670-PREPARO-TOTALES.
           ADD 1 to w-cant-soc-xfilial.
           ADD soc-saldo TO w-saldo-xfilial.

       700-FIN-FILIAL.
           PERFORM 720-ARMO-TOTAL-FILIAL.



       720-ARMO-TOTAL-FILIAL.
           MOVE w-cant-soc-xfilial to l-cant.
           MOVE w-saldo-xfilial TO l-saldo-filial.
           DISPLAY lin-guarda2.
           DISPLAY lin-cant-filial.

       800-FIN-GENERAL.
           PERFORM 820-CERRAR-ARCHIVOS.

       820-CERRAR-ARCHIVOS.
           CLOSE SOCIOS.
           CLOSE FILIALES.
       END PROGRAM Ejercicio_Sala2.
