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
           03 l-nom-filial pic x(15).
           03 filler pic x(4) value "con ".
           03 l-cant-mas-socios pic 99.
           03 filler pic x(6) value "socios".


       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
            100-INICIO-GENERAL.
            200-LEER-REG-FILIALES.
            PERFORM UNTIL (flag-filial = 0)
               MOVE fil-codigo TO filialActual
               300-LEER-REG-SOCIOS
               PERFORM UNTIL filialActual <> soc-filial
                   socioActual = soc-filial
                   PERFORM UNTIL socActual <> soc-filial
                       MOVE ZEROES to importeSocio
                       if soc-modalidad = "T"
                           ADD soc-importe TO importeSocio
                       300-LEER-REG-SOCIOS

       END PROGRAM YOUR-PROGRAM-NAME.
