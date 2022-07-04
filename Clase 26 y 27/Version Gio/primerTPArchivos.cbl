      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    Asi seleccionamos un archivo SECUENCIAL
           SELECT SOCIOS ASSIGN TO
           "..\socios.dat".
      *    Si uno no configura la ORGANIZACION, se toma como SECUENCIAL
           SELECT FILIALES ASSIGN TO
           "..\filiales.dat".
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01 soc-reg.
           03 soc-filial pic 99.
           03 soc-socio pic 99999.
           03 soc-importe pic s9(8)v99.
           03 soc-modalidad pic x(1).
       FD  FILIALES.
       01  fil-reg.
           03 fil-codigo pic 99.
           03 fil-nombre pic x(15).
       WORKING-STORAGE SECTION.
      ******************************************************************
      ********************** LINEAS DE IMPRESION **********************
      *****************************************************************
       01  lin-titulo.
           03 filler pic x(30) value spaces.
           03 filler pic x(19) value "BANCO EL CORRALITO".
           03 filler pic x(31) value spaces.

       01  lin-subtitulo.
           03 filler pic x(17) value spaces.
           03 filler pic x(46) value
           "LISTADO DE TRANSFERENCIAS BANCARIAS DE SOCIOS".
           03 filler pic x(17) value spaces.

       01  lin-filial.
           03 filler pic x(26) value spaces.
           03 filler pic x(8) value "FILIAL: ".
           03 l-num-filial pic 9.
           03 filler pic xxx value " - ".
           03 l-nom-filial pic x(16).
           03 filler pic x(26) value spaces.

       01  lin-columnas.
           03 filler pic x(22) value spaces.
           03 filler pic x(6) value "SOCIO".
           03 filler pic x(22) value spaces.
           03 filler pic x(10) value "IMPORTE".
           03 filler pic x(20) value spaces.

       01  lin-datos.
           03 filler pic x(22) value spaces.
           03 l-socio pic x(6).
           03 filler pic x(22) value spaces.
           03 l-importe pic zz.zzz.zz9,99.
           03 filler pic x(22) value spaces.

       01  lin-espacios.
           03 filler pic x(22) value spaces.
           03 filler pic x(6) value "------".
           03 filler pic x(22) value spaces.
           03 filler pic x(10) value "----------".
           03 filler pic x(20) value spaces.

       01  lin-datos-total.
           03 filler pic x(15) value spaces.
           03 filler pic x(7) value "Total: ".
           03 l-cant-filial pic x(6).
           03 filler pic x(22) value spaces.
           03 l-importe-filial pic zz.zzz.zz9,99.
           03 filler pic x(22) value spaces.

       01  lin-titulo-estadistica.
           03 filler pic x(10) value spaces.
           03 filler pic x(20) value "ESTADISTICA GENERAL:".

       01  lin-estad-importe.
           03 filler pic x(38) value
           "El importe general transferido fue de ".
           03 l-estad-importe pic zz.zzz.zz9,99.

       01  lin-estad-cant.
           03 filler pic x(25) value "En total se procesaron a ".
           03 l-cant-socios pic 99.
           03 filler pic x(6) value "socios".

       01  lin-estad-filial-mas-socios.
           03 filler pic x(56) value
           "La filial que mas socios operaron transferencias fue la ".
           03 l-num-filial-mas-socios pic 9.
           03 filler pic xxx value " - ".
           03 l-nom-filial-mas-socios pic x(15).
           03 filler pic x(4) value "con ".
           03 l-cant-mas-socios pic 99.
           03 filler pic x(6) value "socios".
      *****************************************************************
      ********************** VARIABLES DE TRABAJO *********************
      *****************************************************************
       01  flag-filial pic 9 value 1.
       01  flag-socio pic 9 value 1.

       01  filialActual pic 99.
       01  socAnterior pic 9999.
      ********************** ACUMULADORES ******************************
       01  cantGeneral pic 999.
       01  totalGeneral pic s9(12)v99.
       01  importeSocio pic s9(12)v99.
       01  cantTransfSocio pic 99.
       01  cantTransfFilial pic 99.
       01  totalFilial pic 9(12)v99.
      ********************** MAXIMOS ***********************************
       01  codMax pic 999.
       01  cantMax pic 999 value 0.
       01  nomMax pic x(15).


       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM 100-INICIO-GENERAL.
      *    ES NECESARIO LEER AMBOS REGISTROS ANTES DEL CICLO PARA NOPERDER 1 REGISTRO MAS ADELANTE CUANDO OCURRA EL CAMBIO DE FILIAL
           PERFORM 200-LEER-REG-FILIALES.
           PERFORM 300-LEER-REG-SOCIOS.
           PERFORM UNTIL (flag-filial = 0)
               PERFORM 600-INICIO-FILIAL
      *        PERFORM 300-LEER-REG-SOCIOS este esta mal ya que cuando cambia de filial, elLEER 300-LEER-REG-SOCIOS de la linea 143 se pierde
               PERFORM UNTIL
               (flag-socio = 0) or (fil-codigo <> soc-filial)
                       PERFORM 400-INICIO-CIRCUITO-SOCIO
                       PERFORM UNTIL (socAnterior <> soc-socio)
                       or (fil-codigo <> soc-filial) or (flag-socio = 0)
                               PERFORM 450-PROCESO-CIRCUITO-SOCIO
                               PERFORM 300-LEER-REG-SOCIOS
                       END-PERFORM
                       PERFORM 475-FIN-CIRCUITO-SOCIO
               END-PERFORM
               PERFORM 820-FIN-FILIAL
               PERFORM 200-LEER-REG-FILIALES
           END-PERFORM

           PERFORM 800-FIN-GENERAL.

           STOP RUN.


       100-INICIO-GENERAL.
           OPEN INPUT SOCIOS.
           OPEN INPUT FILIALES.
           DISPLAY lin-titulo.
           DISPLAY lin-subtitulo.
           MOVE ZEROES TO totalGeneral.
           MOVE ZEROES TO cantGeneral.

       200-LEER-REG-FILIALES.
           READ FILIALES AT END MOVE 0 TO flag-filial.

       300-LEER-REG-SOCIOS.
           READ SOCIOS AT END MOVE 0 TO flag-socio.

       400-INICIO-CIRCUITO-SOCIO.
           MOVE soc-socio TO socAnterior.
           MOVE ZEROES to importeSocio.
           MOVE ZEROES TO cantTransfSocio.

       450-PROCESO-CIRCUITO-SOCIO.
           if (soc-modalidad = "T")
               ADD 1 to cantTransfSocio
               ADD soc-importe TO importeSocio.

       475-FIN-CIRCUITO-SOCIO.
           ADD cantTransfSocio TO cantTransfFilial
           ADD importeSocio TO totalFilial.
           PERFORM 480-IMPRESIONES-SOCIO.


       480-IMPRESIONES-SOCIO.
           if (importeSocio > 0)
               MOVE socAnterior TO l-socio
               MOVE importeSocio TO l-importe
               DISPLAY lin-datos
           END-IF.
      *     DISPLAY
      *     "Cant de Transferencias del Socio ",
      *     cantTransfSocio.


       600-INICIO-FILIAL.
           MOVE ZEROES TO cantTransfFilial.
           MOVE ZEROES TO totalFilial.
           MOVE fil-codigo TO l-num-filial.
           MOVE fil-nombre TO l-nom-filial.
           DISPLAY lin-filial.
           DISPLAY lin-columnas.

       800-FIN-GENERAL.
           DISPLAY lin-titulo-estadistica.
           DISPLAY "En total se procesaron ", cantGeneral, " socios".
           DISPLAY
           "El importe general transferido fue de ", totalGeneral.
           DISPLAY "La filial que mas socio transfirio fue la ",
               codMax, " - ", nomMax, " con ", cantMax, " socios".
           PERFORM 999-CERRAR-ARCHIVOS.

       820-FIN-FILIAL.

           ADD cantTransfFilial TO cantGeneral.
           ADD totalFilial TO totalGeneral.
           PERFORM 830-CONSULTAR-MAX.
           DISPLAY lin-espacios.
           MOVE cantTransfFilial TO l-cant-filial.
           MOVE totalFilial TO l-importe-filial.
           DISPLAY lin-datos-total.

       830-CONSULTAR-MAX.
           if (cantTransfFilial > cantMax)
               MOVE fil-codigo TO codMax
               MOVE fil-nombre TO nomMax
               MOVE cantTransfFilial TO cantMax
           END-IF.

       999-CERRAR-ARCHIVOS.
               CLOSE SOCIOS.
               CLOSE FILIALES.

       END PROGRAM YOUR-PROGRAM-NAME.
