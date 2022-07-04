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

       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOCIOS
           ASSIGN TO "..\SOCIOS.txt".

           SELECT NOVEDADES
           ASSIGN TO "..\NOVEDADES.txt".

           SELECT LOG-ERRORES
           ASSIGN TO "..\LOG-ERRORES.txt".

           SELECT SOCIOS-ACT
           ASSIGN TO "..\SOCIOS-ACT.txt".

       DATA DIVISION.
       FILE SECTION.

       FD  SOCIOS.
       01  soc-reg.
           03 soc-socio pic 9(4).
           03 soc-nombre pic x(20).
           03 soc-saldo pic s9(8)v99.

       FD  NOVEDADES.
       01  nov-reg.
           03 nov-socio pic 9(4).
           03 nov-mov pic x.
           03 nov-nombre pic x(20).
           03 nov-importe pic s9(7)v99.

       FD  LOG-ERRORES.
       01  log-reg.
           03 log-socio pic 9(4).
           03 log-mov pic x.
           03 log-importe pic s9(7)v99.
           03 log-error pic x(40).

       FD  SOCIOS-ACT.
       01  socAct-reg.
           03 socAct-socio pic 9(4).
           03 socAct-nombre pic x(20).
           03 socAct-saldo pic s9(8)v99.

      *-----------------------
       WORKING-STORAGE SECTION.
       01  WS-esAlta PIC X.
       01  WS-esBaja PIC X.
       01  flag-SociosAct pic x.
       01  flag-LogErrores pic x.

      *********** VARIABLE PARA CREAR ARCHIVOS DE TRABAJO ***************
       01  salir pic x.
       01  resp pic x.
       01  flag-Socios pic x.
       01  flag-Novedades pic x.
      *-----------------------
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
      **************** CREAR ARCHIVOS DE TRABAJO ************************
           display "****************ARCHIVOS DE TRABAJO ***************"
           PERFORM 010-CrearArchivosDeTrabajo
           PERFORM 040-VER-SOCIOS
           PERFORM 050-VER-NOVEDADES

      **************** INICIO PROGRAMA PRINCIPAL ************************
           display "****************INICIO PROGRAMA PRINCIPAL**********"
           PERFORM 200-iniciarArchivos
           PERFORM 300-leerSocio-batch
           display "leo socio: "soc-reg
           PERFORM 400-pasoSoc-SocAct-batch
           PERFORM 500-leerNovedades-batch
           display "leo novedades: "nov-reg
           PERFORM UNTIL   nov-socio=9999 and soc-socio=9999
           and socAct-socio = 9999
           DISPLAY "******ENTRO AL CICLO********"
               EVALUATE TRUE
                   WHEN nov-socio > socAct-socio
                   DISPLAY "NOVEDADES > SOCIOS-ACT"
                        PERFORM 600-graboAct
                        PERFORM 1000-esBaja
                             IF WS-esBaja = "N"
                               PERFORM 300-leerSocio-batch
                               display "leo socio: "soc-reg
                             END-IF
                        PERFORM 400-pasoSoc-SocAct-batch
                    WHEN nov-socio = socAct-socio
                    DISPLAY "NOVEDADES = SOCIOS-ACT"
                        PERFORM 700-esAlta
                        IF WS-esAlta = "N"
                           PERFORM 1000-esBaja
                           IF WS-esBaja = "S"
                               display "valor socio: "soc-socio
                               display "valor monto: "soc-saldo
                             PERFORM 300-leerSocio-batch
                             display "leo socio: "soc-reg
                             PERFORM 400-pasoSoc-SocAct-batch
                             PERFORM 500-leerNovedades-batch
                             display "leo novedades**"nov-reg
                           ELSE
                             PERFORM 1100-modificoNovEnAct
                             PERFORM 500-leerNovedades-batch
                             display "leo novedades: "nov-reg
                           END-IF
                        ELSE
                           PERFORM 1300-VarificaError-MARI-BARI
                           PERFORM 800-pasoNov-Log-Errores
                           PERFORM 900-graboLog-Errores
                           PERFORM 500-leerNovedades-batch
                           display "leo novedades: "nov-reg
                        END-IF
                   WHEN nov-socio < socAct-socio
                   DISPLAY "NOVEDADES < SOCIOS-ACT"
                       PERFORM 700-esAlta
                       IF WS-esAlta = "S"
                           PERFORM 1200-pasoNov-Act
                           PERFORM 500-leerNovedades-batch
                           display "leo novedades: "nov-reg
                       ELSE
                           PERFORM 1300-VarificaError-MARI-BARI
                           PERFORM 800-pasoNov-Log-Errores
                           PERFORM 900-graboLog-Errores
                           PERFORM 500-leerNovedades-batch
                           display "leo novedades: "nov-reg
                       END-IF
               END-EVALUATE
           END-PERFORM.
               display "********FIN CICLO***********"
           PERFORM 1400-cerrarArchivos.
      ************************ VER ARCHIVOS CREADOS *********************
           display "**************** ARCHIVOS GENERADOS ***************"
           PERFORM 1500-VER-SOCIOS-ACT.
           PERFORM 1600-VER-LOG-ERRORES.
           STOP RUN.

      *************************** RUTINAS *******************************

       200-iniciarArchivos.
           OPEN INPUT SOCIOS
           display "habro socio"
           OPEN INPUT NOVEDADES
           display "habro novedades"
           OPEN OUTPUT SOCIOS-ACT
           display "habro socio-act"
           OPEN OUTPUT LOG-ERRORES
           display "habro log-errores".
       300-leerSocio-batch.
           if soc-reg is EQUAL "9999"
               move "9999" to soc-reg
     *     ELSE
               READ SOCIOS AT END MOVE "9999" TO soc-reg
           END-IF.
       400-pasoSoc-SocAct-batch.
           move soc-reg to socAct-reg
           display "muevo: " soc-reg " socio a socio act".
       500-leerNovedades-batch.
           if nov-reg is EQUAL "9999"
               move "9999" to nov-reg
           ELSE
               READ NOVEDADES AT END move "9999" to nov-reg
           END-IF.
       600-graboAct.
           WRITE socAct-reg
           display "grabo: "socAct-reg" socio-act".
       700-esAlta.
           IF nov-mov IS EQUAL "A" THEN
               MOVE "S" TO WS-esAlta
               display "es alta"
           ELSE
               MOVE "N" TO WS-esAlta
               display "no es alta"
           END-IF.
       800-pasoNov-Log-Errores.
           MOVE nov-reg to log-reg.
           DISPLAY "muevo: "nov-reg" a LOG-ERRORES".
       900-graboLog-Errores.
           WRITE log-reg.
           DISPLAY "grabo: "log-reg" en LOG-ERRORES".
       1000-esBaja.
           IF nov-mov IS EQUAL "B" THEN
               MOVE "S" TO WS-esBaja
               display "es baja"
           ELSE
               MOVE "N" TO WS-esBaja
               display "no es baja"
           END-IF.
       1100-modificoNovEnAct.
           IF nov-nombre IS NOT EQUAL SPACE
              MOVE  nov-nombre TO socAct-nombre
              display "modifico nombre "nov-nombre"en soc-act"
           END-IF
           ADD nov-importe to socAct-saldo.
           display "sumo importe: "nov-importe.
       1200-pasoNov-Act.
           MOVE nov-socio to socAct-socio
           MOVE nov-nombre to socAct-nombre
           MOVE nov-importe to socAct-saldo
           display
           "muevo: "nov-socio nov-nombre nov-importe"a socio-act".
       1300-VarificaError-MARI-BARI.
           IF nov-mov = "B" THEN
               display "ERROR Baja A Registro Inexistente"
           end-if
           IF nov-mov = "M" THEN
               display "ERROR Modificacion A Registro Inexistente"
           end-if
           IF nov-mov = "A" THEN
               display "ERROR Alta A Registro Existente"
           end-if.
       1400-cerrarArchivos.
           CLOSE SOCIOS
           DISPLAY "CIERRO SOCIO"
           CLOSE NOVEDADES
           DISPLAY "CIERRO NOVEDADES"
           CLOSE SOCIOS-ACT
           DISPLAY "CIERRO SOCIOS-ACT"
           CLOSE LOG-ERRORES
           DISPLAY "CIERRO LOG-ERRORES".
      **************************** RUTINAS PARA VER ARCHIVOS ************
       1500-VER-SOCIOS-ACT.
           PERFORM 1510-ABRIR-SOCIOS-ACT.
           PERFORM 1520-LEER-SOCIOS-ACT.
           PERFORM UNTIL flag-SociosAct="s"
               PERFORM 1530-MOSTAR-SOCIOS-ACT
               PERFORM 1520-LEER-SOCIOS-ACT
           END-PERFORM
           PERFORM 1540-CERRAR-SOCIOS-ACT.

       1510-ABRIR-SOCIOS-ACT.
           DISPLAY "**********************************"
           display "    Archivo Socio-Act Generado"
           DISPLAY "**********************************"
           OPEN INPUT SOCIOS-ACT.
       1520-LEER-SOCIOS-ACT.
           READ SOCIOS-ACT AT END MOVE "s" TO flag-SociosAct.
       1530-MOSTAR-SOCIOS-ACT.
           DISPLAY socAct-reg.
       1540-CERRAR-SOCIOS-ACT.
           CLOSE SOCIOS-ACT.

       1600-VER-LOG-ERRORES.
           PERFORM 1610-ABRIR-LOG-ERRORES.
           PERFORM 1620-LEER-LOG-ERRORES.
           PERFORM UNTIL flag-LogErrores="s"
               PERFORM 1630-MOSTAR-LOG-ERRORES
               PERFORM 1620-LEER-LOG-ERRORES
           END-PERFORM
           PERFORM 1640-CERRAR-LOG-ERRORES.

       1610-ABRIR-LOG-ERRORES.
           DISPLAY "**********************************"
           display "    Archivo LOG-ERRORES Generado"
           DISPLAY "**********************************"
           OPEN INPUT LOG-ERRORES.
       1620-LEER-LOG-ERRORES.
           READ LOG-ERRORES AT END MOVE "s" TO flag-LogErrores.
       1630-MOSTAR-LOG-ERRORES.
           DISPLAY log-reg.
       1640-CERRAR-LOG-ERRORES.
           CLOSE LOG-ERRORES.
      *****************RUTINAS PARA CREAR ARCHIVOS DE TRABAJO************
       010-CrearArchivosDeTrabajo.
           display "necesita crear archivos de trabajo? S/N"
           ACCEPT resp
           if resp is EQUAL "S"
               PERFORM 020-CREAR-SOCIOS
               PERFORM 030-CREAR-NOVEDADES
           END-IF.
       020-CREAR-SOCIOS.
           OPEN OUTPUT SOCIOS.

           MOVE "N" TO salir
           PERFORM UNTIL salir="S"
               DISPLAY "INGRESE LOS VALORES DE SOCIOS"
               DISPLAY "Numero de Socio"
               ACCEPT soc-socio
               DISPLAY "Nombre de Socio"
               ACCEPT soc-nombre
               DISPLAY "Saldo de Socio"
               ACCEPT soc-saldo
               WRITE soc-reg
               DISPLAY "salir S/N"
               ACCEPT salir
           END-PERFORM

           CLOSE SOCIOS.
       030-CREAR-NOVEDADES.
           OPEN OUTPUT NOVEDADES.

           MOVE "N" TO salir
           PERFORM UNTIL salir="S"
               DISPLAY "INGRESE LOS VALORES DE NOVEDADES"
               DISPLAY "Numero de Socio"
               ACCEPT nov-socio
               DISPLAY "Tipo de movimiento (A;M;B)"
               ACCEPT nov-mov
               DISPLAY "Nombre de Socio"
               ACCEPT nov-nombre
               DISPLAY "Saldo de Socio"
               ACCEPT nov-importe
               WRITE nov-reg
               DISPLAY "salir S/N"
               ACCEPT salir
           END-PERFORM
           CLOSE NOVEDADES.

       040-VER-SOCIOS.
           PERFORM 041-ABRIR-SOCIOS.
           PERFORM 042-LEER-SOCIOS.
           PERFORM UNTIL flag-Socios="s"
               PERFORM 043-MOSTAR-SOCIOS
               PERFORM 042-LEER-SOCIOS
           END-PERFORM
           PERFORM 044-CERRAR-SOCIOS.

       041-ABRIR-SOCIOS.
           DISPLAY "**********************************"
           display "        Archivo Socio"
           DISPLAY "**********************************"
           OPEN INPUT SOCIOS.
       042-LEER-SOCIOS.
           READ SOCIOS AT END MOVE "s" TO flag-Socios.
       043-MOSTAR-SOCIOS.
           DISPLAY soc-reg.
       044-CERRAR-SOCIOS.
           CLOSE SOCIOS.

       050-VER-NOVEDADES.
           PERFORM 051-ABRIR-NOVEDADES.
           PERFORM 052-LEER-NOVEDADES.
           PERFORM UNTIL flag-Novedades="s"
               PERFORM 053-MOSTAR-NOVEDADES
               PERFORM 052-LEER-NOVEDADES
           END-PERFORM
           PERFORM 054-CERRAR-NOVEDADES.

       051-ABRIR-NOVEDADES.
           DISPLAY "**********************************"
           display "        Archivo Novedades"
           DISPLAY "**********************************"
           OPEN INPUT NOVEDADES.
       052-LEER-NOVEDADES.
           READ NOVEDADES AT END MOVE "s" TO flag-Novedades.
       053-MOSTAR-NOVEDADES.
           DISPLAY nov-reg.
       054-CERRAR-NOVEDADES.
           CLOSE NOVEDADES.




      ** add other procedures here
       END PROGRAM YOUR-PROGRAM-NAME.
