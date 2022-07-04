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
           SELECT socios ASSIGN TO "..\socios.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS soc-cod.

       DATA DIVISION.
       FILE SECTION.
       FD  socios.
       01  soc-reg.
           03 soc-cod pic 999.
           03 soc-nom pic x(20).
           03 soc-saldo pic 9(6)v99.


       WORKING-STORAGE SECTION.

       01  opc-menu pic 9.
       01  opc-mod pic 9.

       01  cod-aux pic 999.
       01  nom-aux pic x(20).
       01  saldo-aux pic s9(6)v99.

       01  flagExiste pic 9 value 1.
       01  flagBaja pic 9.
       01  flag-soc pic 9.
       01  flag-mod pic 9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO.
            PERFORM WITH TEST AFTER UNTIL opc-menu = 6
               PERFORM 200-MOSTRAR-MENU
               PERFORM 250-PEDIR-OPCION
               EVALUATE opc-menu
                   WHEN 1
                       PERFORM 300-ALTA
                   WHEN 2
                       PERFORM 400-BAJA
                   WHEN 3
                       PERFORM 500-MODIFICACION
                   WHEN 4
                       PERFORM 600-CONSULTA
                   WHEN 5
                       PERFORM 700-CONSULTA-UNICA
               END-EVALUATE
            END-PERFORM.
            PERFORM 900-FIN.
            STOP RUN.

           100-INICIO.
               DISPLAY "EL ABM 30000".

           200-MOSTRAR-MENU.
               DISPLAY "MENU".
               DISPLAY "INGRESE OPCION PREFERIDA".
               DISPLAY "1 - ALTA".
               DISPLAY "2 - BAJA".
               DISPLAY "3 - MODIFICACION".
               DISPLAY "4 - VER TODO".
               DISPLAY "5 - VER USUARIO".
               DISPLAY "6 - SALIR".

           250-PEDIR-OPCION.
               ACCEPT opc-menu.
               PERFORM UNTIL (opc-menu >= 1) OR (opc-menu <= 6)
                   DISPLAY "Opcion Invalida"
                   PERFORM 200-MOSTRAR-MENU
                   ACCEPT opc-menu
               END-PERFORM.
      *************************************************************************
           300-ALTA.
               PERFORM 310-INICIO.
               PERFORM 320-INGRESO.
               PERFORM UNTIL cod-aux = 0
                   PERFORM 330-INGRESO-RESTO
                   PERFORM 340-PROCESO
                   PERFORM 320-INGRESO
               END-PERFORM.
               PERFORM 350-FIN.

           310-INICIO.
               OPEN I-O SOCIOS.

           320-INGRESO.
               DISPLAY "Ingrese codigo del socio. 0 para fin."
               ACCEPT cod-aux.
               MOVE cod-aux TO soc-cod.
               PERFORM 322-VALIDAR-EXIST.

           322-VALIDAR-EXIST.
               READ SOCIOS NOT INVALID KEY
                               DISPLAY "El Socio ", cod-aux, " existe"
                               PERFORM 320-INGRESO
                           INVALID KEY
                               NEXT SENTENCE.

           330-INGRESO-RESTO.
               DISPLAY "Ingrese nombre del socio."
               ACCEPT soc-nom.
               DISPLAY "Ingrese saldo del socio."
               ACCEPT soc-saldo.

           340-PROCESO.
               WRITE soc-reg INVALID KEY
                   DISPLAY "ERROR Grabacion impedida".

           350-FIN.
               CLOSE SOCIOS.

      *************************************************************************
           400-BAJA.
               PERFORM 310-INICIO.
               PERFORM 420-INGRESO-BAJA.
               PERFORM UNTIL cod-aux = 0
                   PERFORM 440-PROCESO-BAJA
                   PERFORM 420-INGRESO-BAJA
               END-PERFORM.
               PERFORM 350-FIN.

           420-INGRESO-BAJA.
               DISPLAY "Ingrese cod. del socio a eliminar, 0 para fin."
               ACCEPT cod-aux.
               MOVE cod-aux TO soc-cod.

           440-PROCESO-BAJA.
               PERFORM 441-VERIFICAR-BAJA.
               IF (flagBaja = 1)
                   DELETE SOCIOS INVALID KEY
                       DISPLAY "ERROR Borrado Impedido"
                               NOT INVALID KEY
                       DISPLAY "BORRADO EXITOSO - Socio ", cod-aux.
               MOVE 0 TO flagBaja.

           441-VERIFICAR-BAJA.
               DISPLAY "Estas seguro de querer ELIMINAR al socio ",
               soc-cod, "? Ingrese 1-Si o 2-No"
               ACCEPT flagBaja.
               PERFORM UNTIL (flagBaja = 1) OR (flagBaja = 2)
                   DISPLAY "Opcion Invalida"
                   DISPLAY "Ingrese 1-Si o 2-No"
                   ACCEPT flagBaja
               END-PERFORM.

      *************************************************************************

           500-MODIFICACION.
               PERFORM 310-INICIO.
               PERFORM 520-INGRESO-MOD.
               PERFORM UNTIL (cod-aux = 0)
                   PERFORM 540-PROCESO-MOD
                   PERFORM 520-INGRESO-MOD
               END-PERFORM.
               PERFORM 350-FIN.

           520-INGRESO-MOD.
               DISPLAY "Ingrese cod. del socio a modificar, 0 para fin."
               ACCEPT cod-aux.
               IF (cod-aux <> 0)
                   MOVE cod-aux TO soc-cod
                   PERFORM 522-VALIDAR-EXIST.

           522-VALIDAR-EXIST.
               READ SOCIOS INVALID KEY
                               DISPLAY "Socio Inexistente"
                               PERFORM 520-INGRESO-MOD
                           NOT INVALID KEY
                               NEXT SENTENCE.

           540-PROCESO-MOD.
               DISPLAY "Modificando: ", soc-cod.
               DISPLAY
               "Valores a modificar: 1 - Nombre, 2 - Saldo, 3 - Ambos".
               ACCEPT opc-mod.
               EVALUATE opc-mod
                   WHEN 1
                       PERFORM 541-CAMBIAR-NOMBRE
                   WHEN 2
                       PERFORM 542-CAMBIAR-SALDO
                   WHEN 3
                       PERFORM 543-CAMBIAR-AMBOS
               END-EVALUATE.

           541-CAMBIAR-NOMBRE.
               DISPLAY "Nombre del socio ", soc-nom.
               DISPLAY "Inserte nuevo nombre".
               ACCEPT nom-aux.
               DISPLAY "Confirmar Operacion? 1-Si, 2-No"
               ACCEPT flag-mod.
               PERFORM UNTIL (flag-mod = 1) OR (flag-mod = 2)
                   DISPLAY "Opcion Invalida"
                   DISPLAY "Ingrese 1-Si o 2-No"
                   ACCEPT flag-mod
               END-PERFORM.
               IF (flag-mod = 1)
                   MOVE nom-aux TO soc-nom
                   REWRITE soc-reg
               END-IF.

           542-CAMBIAR-SALDO.
               DISPLAY "Saldo del socio ", soc-saldo.
               DISPLAY "Inserte operacion".
               ACCEPT saldo-aux.
               DISPLAY "Confirmar Operacion? 1-Si, 2-No"
               ACCEPT flag-mod.
               PERFORM UNTIL (flag-mod = 1) OR (flag-mod = 2)
                   DISPLAY "Opcion Invalida"
                   DISPLAY "Ingrese 1-Si o 2-No"
                   ACCEPT flag-mod
               END-PERFORM.
               IF (flag-mod = 1)
                   COMPUTE soc-saldo = soc-saldo + saldo-aux
                   REWRITE soc-reg
               END-IF.

           543-CAMBIAR-AMBOS.
               PERFORM 541-CAMBIAR-NOMBRE.
               PERFORM 542-CAMBIAR-SALDO.

      *************************************************************************
           600-CONSULTA.
            PERFORM 610-INICIO-CONSULTA.
            PERFORM 620-TRAIGO.
            PERFORM UNTIL flag-soc = 0
               PERFORM 640-PROCESO-CONSULTA
               PERFORM 620-TRAIGO
            END-PERFORM.
            PERFORM 350-FIN.

           610-INICIO-CONSULTA.
               OPEN INPUT SOCIOS.
               MOVE 1 TO flag-soc.

           620-TRAIGO.
               READ SOCIOS AT END MOVE 0 TO flag-soc.

           640-PROCESO-CONSULTA.
               DISPLAY soc-cod, " - ", soc-nom, " - $", soc-saldo.

      *************************************************************************

           700-CONSULTA-UNICA.

               PERFORM 610-INICIO-CONSULTA.
               PERFORM 720-INGRESE-NUM-BUSQUEDA.
               PERFORM UNTIL cod-aux = 0
                   PERFORM 740-PROCESO
                   PERFORM 720-INGRESE-NUM-BUSQUEDA
               END-PERFORM.
               PERFORM 350-FIN.



           720-INGRESE-NUM-BUSQUEDA.
               DISPLAY "Ingrese cod. del socio a buscar, 0 para fin."
               ACCEPT cod-aux.
               MOVE cod-aux TO soc-cod.

           200-INGRESE-NUM.
               DISPLAY "Ingrese numero del socio a leer. 0 para FIN"
               ACCEPT cod-aux.
               MOVE cod-aux TO soc-cod.

           740-PROCESO.
               READ SOCIOS INVALID KEY
                               DISPLAY "Socio Inexistente"
                           NOT INVALID KEY
                     DISPLAY soc-cod, " - ", soc-nom, " - $", soc-saldo.




      *************************************************************************
           900-FIN.
               DISPLAY "ABM CERRADO".


       END PROGRAM YOUR-PROGRAM-NAME.
