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

           SELECT NOVEDADES ASSIGN TO
           "..\novedades.dat".

           SELECT SOCIOS ASSIGN TO
           "..\socios.dat".

           SELECT ERROR-LOG ASSIGN TO
           "..\errorLog.dat".

           SELECT SOC-ACTUAL ASSIGN TO
           "..\sociosActual.dat".

       DATA DIVISION.
       FILE SECTION.
       FD  NOVEDADES.
       01  nov-reg.
           03 nov-socio pic 9999.
           03 nov-mov pic x.
           03 nov-nombre pic x(20).
           03 nov-importe pic s9(7)v99.
       FD  SOCIOS.
       01  soc-reg.
           03 soc-socio pic 99999.
           03 soc-nombre pic x(20).
           03 soc-importe pic s9(8)v99.
       FD  ERROR-LOG.
       01  log-reg.
           03 log-socio pic 99999.
           03 log-mov pic x.
           03 log-importe pic s9(7)v99.
           03 log-error pic x(40).
       FD  SOC-ACTUAL.
       01  soc-actual-reg.
           03 soc-socio-act pic 99999.
           03 soc-nombre-act pic x(20).
           03 soc-importe-act pic s9(8)v99.

       WORKING-STORAGE SECTION.
      *****************************************************************
      ********************** VARIABLES DE TRABAJO *********************
      *****************************************************************
       01  flag-novedades pic 9 value 1.
       01  flag-socio pic 9 value 1.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM 100-INICIO-GENERAL.
           PERFORM 200-LEER-REG-SOCIOS.
           PERFORM 210-PASAR-A-ACTUAL.
           PERFORM 300-LEER-REG-NOVEDADES.
           PERFORM UNTIL (soc-socio-act = 9999)
           and (nov-socio = 9999) and (soc-socio = 9999)
               DISPLAY "-----ENTRO-----"
               IF (nov-socio = soc-socio-act)
                   PERFORM 400-PERFORM-EQUAL-CONDITION
               ELSE IF (nov-socio < soc-socio-act)
                   PERFORM 500-PERFORM-LESS-CONDITION
               ELSE IF (nov-socio > soc-socio-act)
                   PERFORM 600-PERFORM-GREAT-CONDITION
               END-IF
               DISPLAY "-----SALGO-----"
           END-PERFORM.
           PERFORM 999-CERRAR-ARCHIVOS.
       STOP RUN.

       100-INICIO-GENERAL.
           OPEN INPUT SOCIOS.
           OPEN INPUT NOVEDADES.
           OPEN OUTPUT ERROR-LOG.
           OPEN OUTPUT SOC-ACTUAL.

       200-LEER-REG-SOCIOS.
               IF (soc-socio NOT EQUAL 9999)
                   READ SOCIOS AT END MOVE 9999 TO soc-socio
                   IF (soc-socio EQUAL 9999)
                       DISPLAY "99999 A SOCIO"
                   END-IF
               END-IF.
               DISPLAY "SOCIOS: ", soc-socio, " ", soc-nombre.

       210-PASAR-A-ACTUAL.
             MOVE soc-reg TO soc-actual-reg.
             DISPLAY "SOCIOS-ACT: ", soc-socio-act, " ", soc-nombre-act.


       300-LEER-REG-NOVEDADES.
           IF (nov-socio NOT EQUAL 9999)
                   READ NOVEDADES AT END MOVE 9999 TO nov-socio

                   IF (nov-socio = 9999)
                       DISPLAY "99999 A NOVEDADES"
                   END-IF
               END-IF.
                    DISPLAY "NOVEDADES: ", nov-socio, " ", nov-mov.

       400-PERFORM-EQUAL-CONDITION.
           DISPLAY "EQUAL CON".
           IF (nov-mov NOT EQUALS "A")
               IF (nov-mov = "B")
                   DISPLAY "EQUAL BAJA"
                   PERFORM 200-LEER-REG-SOCIOS
      *ES IMPORTANTE ACA PASAR TAMBIEN, SINO FALTA ACTUALIZAR
                   PERFORM 210-PASAR-A-ACTUAL
                   PERFORM 300-LEER-REG-NOVEDADES
               ELSE
                   DISPLAY "EQUAL MODIF"
                   PERFORM 420-MODIFICAR-REG
                   PERFORM 300-LEER-REG-NOVEDADES
               END-IF
           ELSE
               DISPLAY "EQUAL ALTA"
               PERFORM 520-DEFINIR-ERROR
               PERFORM 530-GRABAR-ERROR-LOG
               PERFORM 300-LEER-REG-NOVEDADES
           END-IF.

       420-MODIFICAR-REG.
           IF (nov-nombre NOT EQUAL " ")
               MOVE nov-nombre TO soc-nombre-act
           END-IF.
           IF (nov-importe <> 0)
               COMPUTE soc-importe-act = soc-importe-act + nov-importe
           END-IF.

       500-PERFORM-LESS-CONDITION.
           DISPLAY "LESS CON".
           IF (nov-mov EQUAL "A")
               DISPLAY "LESS ALTA"
      *    ACA FALTABA EL MOV DEL NOV-SOCIO
               MOVE nov-socio TO soc-socio-act
               MOVE nov-nombre TO soc-nombre-act
               MOVE nov-importe TO soc-importe-act
               DISPLAY soc-actual-reg
               PERFORM 300-LEER-REG-NOVEDADES
           ELSE
               DISPLAY "LESS NO ALTA"
               PERFORM 520-DEFINIR-ERROR
               PERFORM 530-GRABAR-ERROR-LOG
               PERFORM 300-LEER-REG-NOVEDADES
           END-IF.

       520-DEFINIR-ERROR.
               MOVE nov-socio TO log-socio.
               MOVE nov-mov TO log-mov.
               MOVE nov-importe TO log-importe.
           IF (nov-mov EQUAL "M")
               MOVE "M.A.R.I - Modif. A Reg. Inexistente" TO log-error
           ELSE IF (nov-mov EQUAL "B")
               MOVE "B.A.R.I - Baja A Reg. Inexistente" TO log-error
           ELSE IF (nov-mov EQUAL "A")
               MOVE "A.A.R.I - Alta A Reg. Existente" TO log-error
           END-IF.

       530-GRABAR-ERROR-LOG.
           DISPLAY "GRABAR ERROR ", log-error.
           WRITE log-reg.

       600-PERFORM-GREAT-CONDITION.
           DISPLAY "GREAT CON".
           PERFORM 610-GRABAR-SOC-ACT.
           IF (nov-mov NOT EQUAL "B")
               PERFORM 200-LEER-REG-SOCIOS
           END-IF.
           PERFORM 210-PASAR-A-ACTUAL.

       610-GRABAR-SOC-ACT.
           DISPLAY "**GRABO** ", soc-actual-reg.
           WRITE soc-actual-reg.

       999-CERRAR-ARCHIVOS.
           DISPLAY "FIN".
           CLOSE NOVEDADES.
           CLOSE SOCIOS.
           CLOSE ERROR-LOG.
           CLOSE SOC-ACTUAL.

       END PROGRAM YOUR-PROGRAM-NAME.
