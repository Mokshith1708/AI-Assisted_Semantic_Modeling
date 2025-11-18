       IDENTIFICATION DIVISION.
       PROGRAM-ID. SimpleBonusEval.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  EMP-DEPT        PIC X(3).
       01  EMP-AGE         PIC 99          USAGE COMP-4.
       01  EMP-SALARY      PIC 9(5)V99     USAGE COMP-5.
       01  BONUS           PIC 9(4)V99.

       PROCEDURE DIVISION.

           IF EMP-DEPT = "HR "
               IF EMP-AGE >= 30
                   COMPUTE BONUS = 1000.00
               ELSE
                   COMPUTE BONUS = 700.00
               END-IF
           ELSE
               IF EMP-AGE >= 25
                   COMPUTE BONUS = 900.00
               ELSE
                   COMPUTE BONUS = 600.00
               END-IF
           END-IF

           COMPUTE EMP-SALARY = 28000 + BONUS

           STOP RUN.
