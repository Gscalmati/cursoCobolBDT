      ******************************************************************
      * Author: GRUPO 2 FUTURAS ADQUISICIONES DE BDT:  OLMOS, CECILIA; -
      *    ALCARAZ, EDUARDO; MEDELA, MATIAS; PEREZ, MARTIN; -
      *    GALLUZZO, EZEQUIEL.
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES.DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOCIOS
           ASSIGN TO
           "D:\Datos\Cobol\2022-05-09\socios.dat"
           FILE STATUS IS soc-fileStatus.
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01  soc-reg.
           03 soc-id pic s9(4).
           03 soc-nombre pic x(15).
           03 soc-saldo pic s9(8)v99.
       WORKING-STORAGE SECTION.
      ************************* LINEAS DE IMPRESION ***************************
       01  lin-enc-bienvenida pic x(83) values "BIENVENIDO AL PROGRAMA
      -    "SIMULADOR DE CONTROL DE ARCHIVOS CON LAS DISTINTAS OPCIONES"
           .
       01  lin-pres-1-opc pic x(41) values "Programa para grabar un "-
           "archivo de salida".
       01  lin-pres-2-opc pic x(40) values "Programa para leer un "-
           "archivo de entrada".
       01  lin-pres-3-opc pic x(43) values "Programa para extender un "-
           "archivo existente".
       01  lin-op.
           03 lin-titulo-op pic x(32) values "Seleccione la accion a "-
           "realizar:".
           03 lin-op-1 pic x(39) values " 1 - Crear y escribir archivo"-
           " (output).".
           03 lin-op-2 pic x(44) values " 2 - Abrir y leer archivo"-
           " existente (input).".
           03 lin-op-3 pic x(57) values " 3 - Abrir archivo existente "-
           "y agregar contenido (extend)".
           03 lin-op-4 pic x(23) values " 0 - Terminar programa.".
       01  lin-encabezado.
           03 filler pic x(5) values spaces.
           03 filler pic x(8) values "COD. SOC".
           03 filler pic x(10) values spaces.
           03 filler pic x(6) values "NOMBRE".
           03 filler pic x(13) values spaces.
           03 filler pic x(5) values "SALDO".
       01  lin-dat-socio.
           03 filler pic x(7) values spaces.
           03 lin-cod-soc pic zzz9.
           03 filler pic x(8) values spaces.
           03 lin-nom-soc pic x(15).
           03 filler pic x(5) values spaces.
           03 lin-saldo-soc pic zz.zzz.zz9,99.

       01  lin-guion pic x(80) values all "-".
       01  lin-1-cont-socios.
           03 filler pic x(30) values "El archivo se ha guardado con".
           03 lin-cont pic zz9.
           03 filler pic x(7) values " socios".
        01  lin-2-cont-socios.
           03 filler pic x(30) values "El archivo se ha guardado con" .
           03 lin-cont pic zz9.
           03 filler pic x(14) values " socios nuevos".
       01  lin-3-cont-socios.
           03 filler pic x(20) values "El archivo contiene".
           03 lin-cont pic zz9.
           03 filler pic x(14) values " socios".
      ************************* VARIABLES DE TRABAJO **************************
       01  soc-fileStatus pic xx.
       01  menu-princ pic 9.
       01  cont pic 999.
       01  flag pic 9.
       01  exite-archivo pic 9.
       PROCEDURE DIVISION.
      ************************PRINCIPAL****************************************
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-MENU-PRIN.
           PERFORM 900-FIN.
       END-PROGRAM.
           STOP RUN.
      *************************PROCEDURES**************************************

      *************************INICIO******************************************
       100-INICIO.
           DISPLAY lin-enc-bienvenida.

       180-EVALUAR-ARCHIVO.
           MOVE ZERO TO exite-archivo.
           EVALUATE TRUE
               WHEN soc-fileStatus = "35"
                   DISPLAY "No se encontro el archivo"
      ************ Funcion para cerrar todos los archivos abiertos ************
                   MOVE 1 TO exite-archivo
           END-EVALUATE.

      *****************MENU PRINCIPAL Y SU FUNCIONALIDAD***********************
       200-MENU-PRIN.
           PERFORM WITH TEST AFTER UNTIL menu-princ = 0
               PERFORM 205-INI-MENU
               PERFORM 210-DISP-OPC
               PERFORM 220-IC-MENU
               PERFORM 230-EJEC-OPC
           END-PERFORM.


       205-INI-MENU.
           MOVE ZEROS TO cont.
           MOVE ZEROS TO flag.

       210-DISP-OPC.
           DISPLAY lin-titulo-op.
           DISPLAY lin-op-1.
           DISPLAY lin-op-2.
           DISPLAY lin-op-3.
           DISPLAY lin-op-4.
           ACCEPT menu-princ.

       220-IC-MENU.
           PERFORM UNTIL menu-princ < 4
               DISPLAY "OPCION ERRONEA"
               PERFORM 210-DISP-OPC
           END-PERFORM.

       230-EJEC-OPC.
           IF menu-princ = 1 THEN
               PERFORM 231-OUTPUT.

           IF menu-princ = 2 THEN
               PERFORM 232-INPUT.

           IF menu-princ = 3 THEN
               PERFORM 233-EXTEND.

      *     IF menu-princ = 0 THEN
      *         PERFORM 900-FIN.

      *********************EJECUCION DE OUTPUT 300*****************************
       231-OUTPUT.
           PERFORM 300-INICIO.
           PERFORM 310-ING-SOCIO.
           PERFORM UNTIL soc-id = 0
               PERFORM 320-ING-RESTO
               PERFORM 340-PROC-DATOS
               PERFORM 310-ING-SOCIO
           END-PERFORM.
           PERFORM 350-FIN-OUTPUT.

       300-INICIO.
           DISPLAY lin-guion.
           DISPLAY lin-pres-1-opc.
           open OUTPUT SOCIOS.

       310-ING-SOCIO.
           DISPLAY "Ingrese numero de socio, por fin ingrese 0"-
           "(Regreso al menu principal)".
           accept soc-id.
           PERFORM 600-IC-SOCIO.

       320-ING-RESTO.
           PERFORM 321-ING-NOMBRE.
           PERFORM 322-ING-SALDO.

       321-ING-NOMBRE.
           DISPLAY "Ingrese el nombre del socio".
           ACCEPT soc-nombre.
           PERFORM 610-IC-NOMBRE.

       322-ING-SALDO.
           DISPLAY "Ingrese el saldo"
           ACCEPT soc-saldo.

       340-PROC-DATOS.
           ADD 1 TO cont.
           WRITE soc-reg.

       350-FIN-OUTPUT.
           CLOSE SOCIOS.
           PERFORM 360-ARMO-LIN-1-CONT-SOCIOS.
           DISPLAY lin-1-cont-socios.
           DISPLAY lin-guion.

       360-ARMO-LIN-1-CONT-SOCIOS.
           MOVE cont TO lin-cont OF lin-1-cont-socios.

      *********************EJECUCION DE INPUT 400******************************
       232-INPUT.
           PERFORM 400-INICIO.
           IF exite-archivo IS NOT EQUALS 1 THEN
               PERFORM 405-PRESENTACION
               PERFORM 410-LEER-REG
               PERFORM UNTIL flag = 1
                   PERFORM 420-PROC-DATOS
                   PERFORM 410-LEER-REG
               END-PERFORM
               PERFORM 430-FIN.

       400-INICIO.
           open INPUT SOCIOS.
           PERFORM 180-EVALUAR-ARCHIVO.

       405-PRESENTACION.
           DISPLAY lin-guion.
           DISPLAY lin-pres-2-opc.
           DISPLAY lin-encabezado.

       410-LEER-REG.
           READ SOCIOS AT END MOVE 1 TO flag.

       420-PROC-DATOS.
           ADD 1 TO cont.
           PERFORM 425-ARMO-LIN-DAT-SOCIO.
           DISPLAY lin-dat-socio.

       425-ARMO-LIN-DAT-SOCIO.
           MOVE soc-id TO lin-cod-soc.
           MOVE soc-nombre TO lin-nom-soc.
           MOVE soc-saldo TO lin-saldo-soc.

       430-FIN.
           CLOSE SOCIOS.
           PERFORM 440-ARMO-LIN-3-CONT-SOCIOS.
           DISPLAY lin-3-cont-socios.
           DISPLAY lin-guion.

       440-ARMO-LIN-3-CONT-SOCIOS.
           MOVE cont TO lin-cont OF lin-3-cont-socios.

      ********************EJECUCION DE APPEND 500*************************
       233-EXTEND.
           PERFORM 500-INICIO.
           IF exite-archivo IS NOT EQUALS 1 THEN
               PERFORM 505-PRESENTACION
               PERFORM 310-ING-SOCIO
               PERFORM UNTIL soc-id = 0
                   PERFORM 320-ING-RESTO
                   PERFORM 340-PROC-DATOS
                   PERFORM 310-ING-SOCIO
               END-PERFORM
               PERFORM 510-FIN-EXTEND.


       500-INICIO.
           OPEN EXTEND SOCIOS.
           PERFORM 180-EVALUAR-ARCHIVO.

       505-PRESENTACION.
           DISPLAY lin-guion.
           DISPLAY lin-pres-3-opc.

       510-FIN-EXTEND.
           CLOSE SOCIOS.
           PERFORM 530-ARMO-LIN-2-CONT-SOCIOS.
           DISPLAY lin-2-cont-socios.
           DISPLAY lin-guion.

       530-ARMO-LIN-2-CONT-SOCIOS.
           MOVE cont TO lin-cont OF lin-2-cont-socios.
      *****************PROCEDURES CONTROL 600**********************************
       600-IC-SOCIO.
           PERFORM UNTIL soc-id >= 0
               DISPLAY "ERROR"
               PERFORM 310-ING-SOCIO
           END-PERFORM.

       610-IC-NOMBRE.
           PERFORM UNTIL soc-nombre NOT = SPACE AND LOW-VALUE
               DISPLAY "ERROR"
               PERFORM 321-ING-NOMBRE
           END-PERFORM.

       620-IC-MONTO.

      ****************************FIN******************************************
       900-FIN.
           DISPLAY "PROGRAMA FINALIZADO".

       END PROGRAM YOUR-PROGRAM-NAME.
