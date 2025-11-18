       IDENTIFICATION DIVISION.
       PROGRAM-ID. LoanApprovalCheckAdvanced.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  CUST-ID             PIC X(10).
       01  CUST-NAME           PIC X(30).
       01  LOAN-STATUS         PIC X(20).
       01  REMARK              PIC X(40).

       01  CUST-AGE            PIC 9(03) USAGE COMP.           
       01  CUST-INCOME         PIC S9(09)V99 USAGE COMP-3.     
       01  LOAN-AMOUNT         PIC S9(09)V99 USAGE COMP-4.    
       01  CREDIT-SCORE        PIC 9(03) USAGE COMP-5.         
       01  INTEREST-RATE       PIC 9(02)V9(02) USAGE COMP-3.  

       PROCEDURE DIVISION.

           IF CUST-AGE >= 21
               IF CREDIT-SCORE >= 800
                   IF CUST-INCOME >= LOAN-AMOUNT * 3
                       MOVE "APPROVED PRIME" TO LOAN-STATUS
                       MOVE "EXCELLENT CREDIT AND HIGH INCOME" TO REMARK
                   ELSE
                       MOVE "REJECTED" TO LOAN-STATUS
                       MOVE "INSUFFICIENT INCOME" TO REMARK
                   END-IF
               ELSE
                   IF CREDIT-SCORE >= 650
                       IF INTEREST-RATE < 10.00
                           MOVE "APPROVED STANDARD" TO LOAN-STATUS
                           MOVE "GOOD CREDIT SCORE" TO REMARK
                       ELSE
                           MOVE "REVIEW" TO LOAN-STATUS
                           MOVE "INTEREST RATE TOO HIGH" TO REMARK
                       END-IF
                   ELSE
                       MOVE "REJECTED" TO LOAN-STATUS
                       MOVE "CREDIT SCORE TOO LOW" TO REMARK
                   END-IF
               END-IF
           ELSE
               MOVE "REJECTED" TO LOAN-STATUS
               MOVE "AGE BELOW ELIGIBILITY" TO REMARK
           END-IF.

           STOP RUN.
