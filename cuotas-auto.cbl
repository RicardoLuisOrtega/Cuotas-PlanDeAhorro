      ******************************************************************
      * Author: RICARDO ORTEGA
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUOTASAUTO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALIDA ASSIGN TO
           "C:\Users\dante\Desktop\R\COBOL\salida.txt"
           ORGANIZATION IS SEQUENTIAL ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-SALIDA.

       DATA DIVISION.
       FILE SECTION.
       FD  salida
       RECORD CONTAINS 37 CHARACTERS
       BLOCK CONTAINS 0 RECORDS.
       01 REGISTROSALIDA PIC X(37).
       WORKING-STORAGE SECTION.
       01 FS-VAR.
           05 FS-SALIDA PIC X(2).

       01 WS-REG-SALIDA.
           05 WS-REG-TEXTO    PIC X(21) VALUE "El valor de la cuota ".
           05 WS-REG-NUMCUOTA PIC X(2).
           05 WS-REG-ES       PIC X(4) VALUE " es:".
           05 WS-REG-VALOR    PIC 9(9).
       01 WS-REG-SALIDA2.
           05 WS-REG2-TEXTO PIC X(27)
           VALUE "El valor total del auto es ".
           05 WS-REG2-VALOR PIC 9(11).
       01 WC-CONST.
           05 WC-CONST-CUOTA1 PIC 9(5) VALUE 44070.
           05 WC-CONST-CUOTA2 PIC 9(5) VALUE 54658.
           05 WC-CONST-CUOTA3 PIC 9(5) VALUE 52626.
           05 WC-CONST-CUOTA4 PIC 9(5) VALUE 56738.
       01 WS-VARIABLES.
           05 WS-TOTAL        PIC 9(11).
           05 WS-CONT-1       PIC 9(2).
           05 WS-CONT-CUOTA   PIC 9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 1000-INICIO
               THRU 1000-INICIO-EXIT

            PERFORM 2000-PROCESO
               THRU 2000-PROCESO-EXIT

            PERFORM 3000-FIN
               THRU 3000-FIN-EXIT.

           1000-INICIO.
               INITIALIZE WS-VARIABLES
                WS-REG2-VALOR
                WS-REG-VALOR

               OPEN OUTPUT SALIDA
               IF FS-SALIDA NOT EQUAL "00"
                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
               END-IF.

           1000-INICIO-EXIT.
           EXIT.

           2000-PROCESO.
               COMPUTE WS-TOTAL = WS-TOTAL + WC-CONST-CUOTA1

               ADD 1                TO WS-CONT-CUOTA

               MOVE WC-CONST-CUOTA1 TO WS-REG-VALOR
               MOVE WS-CONT-CUOTA   TO WS-REG-NUMCUOTA

               WRITE REGISTROSALIDA FROM WS-REG-SALIDA AFTER ADVANCING 1

               IF FS-SALIDA NOT EQUAL "00"
                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
               END-IF

               MOVE WC-CONST-CUOTA2 TO WS-REG-VALOR

               PERFORM 2100-CUOTAS-1
                  THRU 2100-CUOTAS-1-FIN
                 UNTIL WS-CONT-1 = 4

               COMPUTE WS-CONT-1 = 1
               MOVE WC-CONST-CUOTA3 TO WS-REG-VALOR

               PERFORM 2200-CUOTAS-2
                  THRU 2200-CUOTAS-2-FIN
                 UNTIL WS-CONT-1 > 19

               COMPUTE WS-CONT-1 = 1
               MOVE WC-CONST-CUOTA4 TO WS-REG-VALOR

               PERFORM 2300-CUOTAS-3
                  THRU 2300-CUOTAS-3-FIN
                 UNTIL WS-CONT-1 > 60

               COMPUTE WS-TOTAL = WS-TOTAL + 1579000
               MOVE WS-TOTAL TO WS-REG2-VALOR
               WRITE REGISTROSALIDA FROM WS-REG-SALIDA2
               AFTER ADVANCING 1

               IF FS-SALIDA NOT EQUAL "00"
                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
               END-IF.

           2000-PROCESO-EXIT.
           EXIT.

           2100-CUOTAS-1.
               COMPUTE WS-REG-VALOR =
               WS-REG-VALOR + WS-REG-VALOR*0.05
               COMPUTE WS-TOTAL = WS-TOTAL + WS-REG-VALOR

               ADD 1                TO WS-CONT-CUOTA
               MOVE WS-CONT-CUOTA   TO WS-REG-NUMCUOTA

               WRITE REGISTROSALIDA FROM WS-REG-SALIDA AFTER ADVANCING 1

               IF FS-SALIDA NOT EQUAL "00"
                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
               END-IF

               COMPUTE WS-CONT-1 = WS-CONT-1 + 1.
           2100-CUOTAS-1-FIN.
           EXIT.

           2200-CUOTAS-2.
               COMPUTE WS-REG-VALOR =
               WS-REG-VALOR + WS-REG-VALOR*0.05
               COMPUTE WS-TOTAL = WS-TOTAL + WS-REG-VALOR

               ADD 1                TO WS-CONT-CUOTA
               MOVE WS-CONT-CUOTA   TO WS-REG-NUMCUOTA

               WRITE REGISTROSALIDA FROM WS-REG-SALIDA AFTER ADVANCING 1

               IF FS-SALIDA NOT EQUAL "00"
                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
               END-IF

               COMPUTE WS-CONT-1 = WS-CONT-1 + 1.
           2200-CUOTAS-2-FIN.
           EXIT.

           2300-CUOTAS-3.
               COMPUTE WS-REG-VALOR =
               WS-REG-VALOR + WS-REG-VALOR*0.05
               COMPUTE WS-TOTAL = WS-TOTAL + WS-REG-VALOR

               ADD 1                TO WS-CONT-CUOTA
               MOVE WS-CONT-CUOTA   TO WS-REG-NUMCUOTA

               WRITE REGISTROSALIDA FROM WS-REG-SALIDA AFTER ADVANCING 1

               IF FS-SALIDA NOT EQUAL "00"
                   PERFORM 3000-FIN
                      THRU 3000-FIN-EXIT
               END-IF

               COMPUTE WS-CONT-1 = WS-CONT-1 + 1.
           2300-CUOTAS-3-FIN.
           EXIT.

           3000-FIN.
               IF FS-SALIDA NOT EQUAL "42"
                   CLOSE SALIDA
               END-IF.
           3000-FIN-EXIT.
           STOP RUN.
