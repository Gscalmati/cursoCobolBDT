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
           SELECT SOCIOS ASSIGN TO "socios.dat".
           SELECT ARCH-SORT ASSIGN TO "SORTWORK".
           SELECT SOCIOS-ORD ASSIGN TO "socios-act.dat".
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01  SOC-REG.
           03 SOC-COD pic 9999.
           03 SOC-NOM  pic x(20).
           03 SOC-SALDO PIC S9(8)V99.

       SD  ARCH-SORT.
       01  SRT-REG.
           03 SRT-COD  pic 9999.
           03 SRT-NOM  pic x(20).
           03 SRT-SALDO PIC S9(8)V99.
      *    EN EL CASO DEL EJERCICIO PEDIDO
      *     03 SRT-NOM-FILIAL X(15).
       FD  SOCIOS-ORD.
       01  ACT-REG PIC X(34).

       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

      *     CASO 1 DE SORT
            SORT ARCH-SORT ASCENDING KEY SRT-NOM
               USING SOCIOS GIVING SOCIOS-ORD.


      *     CASO 2 DE SORT
            SORT ARCH-SORT ASCENDING KEY SRT-NOM
               USING SOCIOS OUTPUT PROCEDURE PROCESO-SALIDA.
      *      STOP RUN.

       PROCESO-SALIDA.
      *    LA INSTRUCCIÓN QUE LEE DEL SORT ES: RETURN
           RETURN ARCH-SORT AT END
      *    EN ESTA PARTE VA TODO EL PROCESO QUE NECESITES

      *           CASO 4 DE SORT
            SORT ARCH-SORT ASCENDING KEY SRT-NOM
               INPUT PROCEDURE PROCESO-ENTRADA OUTPUT PROCEDURE PROCESO-SALIDA.
      *      STOP RUN.

       PROCESO-ENTRADA.
      *    LA INSTRUCCIÓN QUE GRABA EN EL SORT ES: RELEASE
           RELEASE SRT-REG
       PROCESO-SALIDA.
      *    LA INSTRUCCIÓN QUE LEE DEL SORT ES: RETURN
           RETURN ARCH-SORT AT END
      *    EN ESTA PARTE VA TODO EL PROCESO QUE NECESITES



       END PROGRAM YOUR-PROGRAM-NAME.
