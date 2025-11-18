       IDENTIFICATION DIVISION.
       PROGRAM-ID. EmployeeBonusEvaluation.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  EMP-DEPT        PIC X(3).
       01  EMP-AGE         PIC 99          USAGE COMP-4.
       01  EMP-SALARY      PIC 9(5)V99     USAGE COMP-5.
       01  BONUS           PIC 9(4)V99.

       PROCEDURE DIVISION.

           IF EMP-DEPT = "HR "
               IF EMP-AGE >= 30
                   COMPUTE BONUS = 1200.00
               ELSE
                   COMPUTE BONUS = 800.00
               END-IF
           ELSE
               IF EMP-DEPT = "ENG"
                   IF EMP-AGE >= 25
                       COMPUTE BONUS = 1000.00
                   ELSE
                       COMPUTE BONUS = 700.00
                   END-IF
               ELSE
                   IF EMP-AGE >= 40
                       COMPUTE BONUS = 900.00
                   ELSE
                       COMPUTE BONUS = 600.00
                   END-IF
               END-IF
           END-IF

           COMPUTE EMP-SALARY = 30000 + BONUS

           STOP RUN.