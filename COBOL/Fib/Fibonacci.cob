       IDENTIFICATION DIVISION.
       PROGRAM-ID. Fibonacci.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 I          PIC S9(4)  COMP-5.
       01 RESULT     PIC S9(9)  COMP-5.
       01 OUT-I      PIC 99.
       01 OUT-RES    PIC Z(8)9.

       PROCEDURE DIVISION.
           PERFORM VARYING I FROM 0 BY 1 UNTIL I > 30
               CALL 'Fib' USING BY VALUE I
                              BY REFERENCE RESULT
               MOVE I      TO OUT-I
               MOVE RESULT TO OUT-RES
               DISPLAY "fib(" OUT-I ") = " OUT-RES
           END-PERFORM
           STOP RUN.
       END PROGRAM Fibonacci.


       IDENTIFICATION DIVISION.
       PROGRAM-ID. Fib RECURSIVE.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01 A          PIC S9(9)  COMP-5.
       01 B          PIC S9(9)  COMP-5.
       01 TMP        PIC S9(9)  COMP-5.

       LINKAGE SECTION.
       01 N          PIC S9(4)  COMP-5.
       01 RES        PIC S9(9)  COMP-5.

       PROCEDURE DIVISION USING BY VALUE N
                                BY REFERENCE RES.
           IF N <= 1
               MOVE N TO RES
           ELSE
               SUBTRACT 1 FROM N GIVING TMP
               CALL 'Fib' USING BY VALUE TMP BY REFERENCE A

               SUBTRACT 2 FROM N GIVING TMP
               CALL 'Fib' USING BY VALUE TMP BY REFERENCE B

               ADD A TO B GIVING RES
           END-IF
           GOBACK.
       END PROGRAM Fib.
