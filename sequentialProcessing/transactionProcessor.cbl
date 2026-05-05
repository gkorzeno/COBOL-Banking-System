      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDATE-ACCOUNTS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO DYNAMIC WS-MasterFile
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-MS-STATUS.

           SELECT TRANS-FILE ASSIGN TO DYNAMIC WS-TransactionFile
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRNS-STATUS.

           SELECT UPDATED-MASTER-FILE ASSIGN TO DYNAMIC
               WS-UpdatedMasterFile
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-UMS-STATUS.

           SELECT REPORT-FILE ASSIGN TO DYNAMIC WS-ReportFile
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPRT-STATUS.

           SELECT ERROR-REPORT ASSIGN TO DYNAMIC WS-ErrorReport
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ERR-STATUS.

       DATA DIVISION.

       FILE SECTION.

       FD  MASTER-FILE.
       01  MASTER-REC.
           05 M-ACC-NUMBER    PIC X(10).
           05 M-ACC-NAME      PIC X(20).
           05 M-BALANCE       PIC S9(10).
           05 M-ACC-TYPE      PIC X.
           05 M-ACC-STATUS    PIC X.

       FD  TRANS-FILE.
       01  TRANS-REC.
           05 T-ACC-NUMBER    PIC X(10).
           05 T-ID            PIC 9(8).
           05 T-DATE          PIC X(8).
           05 T-TIME          PIC X(6).
           05 T-TRX-TYPE      PIC X.
           05 T-AMOUNT        PIC 9(10).

       FD  UPDATED-MASTER-FILE.
       01  UPDATED-MASTER-REC.
           05 U-ACC-NUMBER    PIC X(10).
           05 U-ACC-NAME      PIC X(20).
           05 U-BALANCE       PIC S9(10).
           05 U-ACC-TYPE      PIC X.
           05 U-ACC-STATUS    PIC X.

       FD ERROR-REPORT.
       01 ERROR-REPORT-FILE PIC X(80).

       FD REPORT-FILE.
       01 SUMMARY-REPORT PIC X(300).

       WORKING-STORAGE SECTION.
       01 WS-CMD-LINE PIC X(200).
       01 WS-MasterFile PIC X(200).
       01 WS-TransactionFile PIC X(200).
       01 WS-UpdatedMasterFile PIC X(200).
       01 WS-ReportFile PIC X(200).
       01 WS-ErrorReport PIC X(200).
       01 WS-MS-STATUS PIC XX.
       01 WS-TRNS-STATUS PIC XX.
       01 WS-UMS-STATUS PIC XX.
       01 WS-RPRT-STATUS PIC XX.
       01 WS-ERR-STATUS PIC XX.
       01 Difference PIC S9(10).
       01 WS-CopyBalance PIC S9(10).

       01  EOF-FLAGS.
           05 MASTER-EOF      PIC X VALUE 'N'.
           05 TRANS-EOF       PIC X VALUE 'N'.

       01 WS-ERROR-REPORT PIC X(80).

       01 WS-BALANCE-DISPLAY    PIC -ZZZZZZZZZ9.
       01 WS-PREV-DISPLAY       PIC -ZZZZZZZZZ9.
       01 WS-DIFF-DISPLAY       PIC -ZZZZZZZZZ9.
       01 WS-AMOUNT-DISPLAY     PIC ZZZZZZZZZ9.

       01 WS-numDeposits PIC 9(10).
       01 WS-numWithdrawals PIC 9(10).
       01 WS-numTransactions PIC 9(10).
       01 WS-totalDeposits PIC 9(10).
       01 WS-totalWithdrawals PIC 9(10).
       01 WS-netChange PIC s9(10).
       01 WS-numOverdrafts PIC 9(10).
       01 WS-numUnprocessed PIC 9(10).
       01 WS-unprocessedPerAccount PIC 9(10).

       01 WS-GRAND-totalDeposits PIC 9(10).
       01 WS-GRAND-totalWithdrawals PIC 9(10).
       01 WS-GRAND-totalOverdrafts PIC 9(10).

       01 WS-DATE.
           05 WS-YEAR  PIC 9(2).
           05 WS-MONTH PIC 9(2).
           05 WS-DAY   PIC 9(2).

       01 WS-TIME.
           05 WS-HOUR   PIC 99.
           05 WS-MIN    PIC 99.
           05 WS-SEC    PIC 99.
           05 WS-HSEC   PIC 99.

       01 WS-DEPOSIT-DISPLAY        PIC ZZZZZZZZZ9.
       01 WS-WITHDRAWAL-DISPLAY     PIC ZZZZZZZZZ9.
       01 WS-NETCHANGE-DISPLAY      PIC -ZZZZZZZZZ9.
       01 WS-NUMTRANS-DISPLAY       PIC ZZZZZZZZZ9.
       01 WS-NUMDEP-DISPLAY         PIC ZZZZZZZZZ9.
       01 WS-NUMWITH-DISPLAY        PIC ZZZZZZZZZ9.
       01 WS-OVERDRAFT-DISPLAY      PIC ZZZZZZZZZ9.
       01 WS-UNPROC-DISPLAY         PIC ZZZZZZZZZ9.

       01 WS-OD-BUF PIC X(300) OCCURS 50 TIMES.
       01 WS-OD-INDEX PIC 9(3).

       01 WS-I PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE ZERO TO WS-GRAND-totalDeposits
           MOVE ZERO TO WS-GRAND-totalWithdrawals
           MOVE ZERO TO WS-GRAND-totalOverdrafts
           ACCEPT WS-CMD-Line FROM COMMAND-LINE
           UNSTRING WS-CMD-LINE DELIMITED BY SPACE
               INTO WS-MasterFile
                    WS-TransactionFile
                    WS-UpdatedMasterFile
                    WS-ReportFile
                    WS-ErrorReport
           END-UNSTRING

           ACCEPT WS-DATE FROM DATE
           ACCEPT WS-TIME FROM TIME

           OPEN INPUT  MASTER-FILE
                INPUT  TRANS-FILE
                OUTPUT UPDATED-MASTER-FILE
                OUTPUT ERROR-REPORT
                OUTPUT REPORT-FILE

           PERFORM WRITE-REPORT-HEADER

           PERFORM INITIAL-READS

           PERFORM UNTIL MASTER-EOF = 'Y'
               PERFORM PROCESS-NEXT-ACCOUNT
           END-PERFORM

           PERFORM WRITE-GRAND-TOTALS

           CLOSE MASTER-FILE
                 TRANS-FILE
                 UPDATED-MASTER-FILE
                 ERROR-REPORT
                 REPORT-FILE

           STOP RUN.

       WRITE-GRAND-TOTALS.
           MOVE SPACES TO SUMMARY-REPORT
           STRING
               "======================================================"
               DELIMITED BY SIZE
               INTO SUMMARY-REPORT
           END-STRING
           WRITE SUMMARY-REPORT

           MOVE SPACES TO SUMMARY-REPORT
           STRING
               "Total Deposits: " DELIMITED BY SIZE
               WS-GRAND-totalDeposits DELIMITED BY SIZE
               INTO SUMMARY-REPORT
           END-STRING
           WRITE SUMMARY-REPORT

           MOVE SPACES TO SUMMARY-REPORT
           STRING
               "Total Withdrawals: " DELIMITED BY SIZE
               WS-GRAND-totalWithdrawals DELIMITED BY SIZE
               INTO SUMMARY-REPORT
           END-STRING
           WRITE SUMMARY-REPORT

           MOVE SPACES TO SUMMARY-REPORT
           STRING
               "Total Overdraft Fees: " DELIMITED BY SIZE
               WS-GRAND-totalOverdrafts DELIMITED BY SIZE
               INTO SUMMARY-REPORT
           END-STRING
           WRITE SUMMARY-REPORT.

       INITIAL-READS.
           READ MASTER-FILE
               AT END MOVE 'Y' TO MASTER-EOF
           END-READ

           READ TRANS-FILE
               AT END MOVE 'Y' TO TRANS-EOF
           END-READ.

       PROCESS-NEXT-ACCOUNT.
           MOVE 0 TO WS-OD-INDEX
           MOVE ZERO TO WS-numDeposits
           MOVE ZERO TO WS-numWithdrawals
           MOVE ZERO TO WS-numTransactions
           MOVE ZERO TO WS-totalDeposits
           MOVE ZERO TO WS-totalWithdrawals
           MOVE ZERO TO WS-netChange
           MOVE ZERO TO WS-numOverdrafts
           MOVE ZERO TO WS-unprocessedPerAccount

           IF MASTER-EOF = 'Y'
               EXIT PARAGRAPH
           END-IF

           IF TRANS-EOF = 'Y'
              OR M-ACC-NUMBER < T-ACC-NUMBER
              MOVE MASTER-REC TO UPDATED-MASTER-REC
              WRITE UPDATED-MASTER-REC
              READ MASTER-FILE
                  AT END MOVE 'Y' TO MASTER-EOF
              END-READ

           ELSE
              IF M-ACC-NUMBER > T-ACC-NUMBER
                 PERFORM SKIP-LOWER-TRANSACTIONS
              ELSE
                 PERFORM APPLY-TRANSACTIONS-TO-ACCOUNT
              END-IF
           END-IF.

       SKIP-LOWER-TRANSACTIONS.
           PERFORM UNTIL TRANS-EOF = 'Y'
                     OR T-ACC-NUMBER >= M-ACC-NUMBER
               READ TRANS-FILE
                   AT END MOVE 'Y' TO TRANS-EOF
               END-READ
           END-PERFORM.

       WRITE-REPORT-HEADER.
           MOVE SPACES TO SUMMARY-REPORT
           STRING
               "Account Updates Summary Report: " DELIMITED BY SIZE
               INTO SUMMARY-REPORT
           END-STRING
           WRITE SUMMARY-REPORT

           MOVE SPACES TO SUMMARY-REPORT
           STRING
               "Date: "
               WS-MONTH "/"
               WS-DAY "/"
               WS-YEAR
               " Time: " WS-HOUR ":" WS-MIN ":" WS-SEC
               INTO SUMMARY-REPORT
           END-STRING
           WRITE SUMMARY-REPORT.

           APPLY-TRANSACTIONS-TO-ACCOUNT.
           MOVE MASTER-REC TO UPDATED-MASTER-REC

           PERFORM UNTIL TRANS-EOF = 'Y'
                     OR T-ACC-NUMBER > M-ACC-NUMBER

               EVALUATE T-TRX-TYPE
                   WHEN 'W'
                       IF T-AMOUNT > U-BALANCE
                           ADD 35 TO WS-GRAND-totalOverdrafts
                           ADD T-AMOUNT TO WS-GRAND-totalWithdrawals
                           ADD 1 TO WS-numWithdrawals
                           ADD T-AMOUNT TO WS-totalWithdrawals
                           ADD 1 TO WS-numOverdrafts
                           MOVE U-BALANCE TO WS-CopyBalance
                           SUBTRACT T-AMOUNT FROM U-BALANCE
                           SUBTRACT 35 FROM U-BALANCE
                           COMPUTE Difference = T-AMOUNT -
                           WS-CopyBalance
                           MOVE U-BALANCE      TO WS-BALANCE-DISPLAY
                           MOVE WS-CopyBalance TO WS-PREV-DISPLAY
                           MOVE Difference     TO WS-DIFF-DISPLAY
                           MOVE T-AMOUNT       TO WS-AMOUNT-DISPLAY
                           ADD 1 TO WS-OD-INDEX
                           STRING
                               "Transaction ID: " DELIMITED BY SIZE
                               T-ID DELIMITED BY SIZE
                               " Date: " DELIMITED BY SIZE
                               T-DATE DELIMITED BY SIZE
                               " Time: " DELIMITED BY SIZE
                               T-TIME DELiMITED BY SIZE
                               " Withdrawal: " DELIMITED BY SIZE
                               WS-AMOUNT-DISPLAY DELIMITED BY SIZE
                               " Previous Account Balance: "
                               DELIMITED BY SIZE
                               WS-PREV-DISPLAY DELIMITED BY SIZE
                               " Current Account Balance: "
                               DELIMITED BY SIZE
                               WS-BALANCE-DISPLAY DELIMITED BY SIZE
                               " Difference: " DELIMITED BY SIZE
                               WS-DIFF-DISPLAY DELIMITED BY SIZE
                               " Overdraft Fee: 35"
                               DELIMITED BY SIZE
                               INTO WS-OD-BUF(WS-OD-INDEX)
                           END-STRING
                       ELSE
                           SUBTRACT T-AMOUNT FROM U-BALANCE
                           ADD 1 TO WS-numWithdrawals
                           ADD T-AMOUNT TO WS-totalWithdrawals
                           ADD T-AMOUNT TO WS-GRAND-totalWithdrawals
                       END-IF
                   WHEN 'D'
                       ADD T-AMOUNT TO U-BALANCE
                       ADD T-AMOUNT TO WS-totalDeposits
                       ADD 1 TO WS-numDeposits
                       ADD T-AMOUNT TO WS-GRAND-totalDeposits
                   WHEN OTHER
                       ADD 1 To WS-unprocessedPerAccount
                       MOVE SPACES TO ERROR-REPORT-FILE
                       STRING
                           "Account Number: "      DELIMITED BY SIZE
                           T-ACC-NUMBER            DELIMITED BY SIZE
                           " Transaction Type: "   DELIMITED BY SIZE
                           T-TRX-TYPE              DELIMITED BY SIZE
                           " Transaction Amount: " DELIMITED BY SIZE
                           T-AMOUNT                DELIMITED BY SIZE
                           INTO ERROR-REPORT-FILE
                       END-STRING
                       WRITE ERROR-REPORT-FILE
               END-EVALUATE

               READ TRANS-FILE
                   AT END MOVE 'Y' TO TRANS-EOF
               END-READ
           END-PERFORM

           COMPUTE WS-netChange = WS-totalDeposits - WS-totalWithdrawals
           COMPUTE WS-numTransactions = WS-numDeposits +
           WS-numWithdrawals

           MOVE WS-numDeposits              TO WS-NUMDEP-DISPLAY
           MOVE WS-totalDeposits            TO WS-DEPOSIT-DISPLAY
           MOVE WS-numWithdrawals           TO WS-NUMWITH-DISPLAY
           MOVE WS-totalWithdrawals         TO WS-WITHDRAWAL-DISPLAY
           MOVE WS-netChange                TO WS-NETCHANGE-DISPLAY
           MOVE WS-numTransactions          TO WS-NUMTRANS-DISPLAY
           MOVE WS-numOverdrafts            TO WS-OVERDRAFT-DISPLAY
           MOVE WS-unprocessedPerAccount    TO WS-UNPROC-DISPLAY

           MOVE SPACES TO SUMMARY-REPORT
           STRING
               "--------------------------------------------------"
               DELIMITED BY SIZE
               INTO SUMMARY-REPORT
           END-STRING
           WRITE SUMMARY-REPORT

           MOVE SPACES TO SUMMARY-REPORT
           STRING
               "Account: " DELIMITED BY SIZE
               U-ACC-NUMBER DELIMITED BY SIZE
               INTO SUMMARY-REPORT
           END-STRING
           WRITE SUMMARY-REPORT

           MOVE SPACES TO SUMMARY-REPORT
           STRING
               "Deposits: " DELIMITED BY SIZE
               WS-NUMDEP-DISPLAY DELIMITED BY SIZE
               INTO SUMMARY-REPORT
           END-STRING
           WRITE SUMMARY-REPORT

           MOVE SPACES TO SUMMARY-REPORT
           STRING
               "Total Deposit Amount: " DELIMITED BY SIZE
               WS-DEPOSIT-DISPLAY DELIMITED BY SIZE
               INTO SUMMARY-REPORT
           END-STRING
           WRITE SUMMARY-REPORT

           MOVE SPACES TO SUMMARY-REPORT
           STRING
               "Withdrawals: " DELIMITED BY SIZE
               WS-NUMWITH-DISPLAY DELIMITED BY SIZE
               INTO SUMMARY-REPORT
           END-STRING
           WRITE SUMMARY-REPORT

           MOVE SPACES TO SUMMARY-REPORT
           STRING
               "Total Withdrawals Amount: " DELIMITED BY SIZE
               WS-WITHDRAWAL-DISPLAY DELIMITED BY SIZE
               INTO SUMMARY-REPORT
           END-STRING
           WRITE SUMMARY-REPORT

           MOVE SPACES TO SUMMARY-REPORT
           STRING
               "Net Change: " DELIMITED BY SIZE
               WS-NETCHANGE-DISPLAY DELIMITED BY SIZE
               INTO SUMMARY-REPORT
           END-STRING
           WRITE SUMMARY-REPORT

           MOVE SPACES TO SUMMARY-REPORT
           STRING
               "Number of Transactions: " DELIMITED BY SIZE
               WS-NUMTRANS-DISPLAY DELIMITED BY SIZE
               INTO SUMMARY-REPORT
           END-STRING
           WRITE SUMMARY-REPORT

           MOVE SPACES TO SUMMARY-REPORT
           STRING
               "Number of Overdrafts: " DELIMITED BY SIZE
               WS-OVERDRAFT-DISPLAY DELIMITED BY SIZE
               INTO SUMMARY-REPORT
           END-STRING
           WRITE SUMMARY-REPORT

           MOVE SPACES TO SUMMARY-REPORT
           STRING
               "Number of Unprocessed Transactions: " DELIMITED BY SIZE
               WS-UNPROC-DISPLAY DELIMITED BY SIZE
               INTO SUMMARY-REPORT
           END-STRING
           WRITE SUMMARY-REPORT

           IF WS-OD-INDEX > 0
               MOVE SPACES TO SUMMARY-REPORT
               STRING
                   "Overdraft Details:" DELIMITED BY SIZE
                   INTO SUMMARY-REPORT
               END-STRING
               WRITE SUMMARY-REPORT

               PERFORM VARYING WS-I FROM 1 BY 1
                 UNTIL WS-I > WS-OD-INDEX
                   MOVE SPACES TO SUMMARY-REPORT
                   MOVE WS-OD-BUF(WS-I) TO SUMMARY-REPORT
                   WRITE SUMMARY-REPORT
               END-PERFORM
           END-IF

           WRITE UPDATED-MASTER-REC

           READ MASTER-FILE
               AT END MOVE 'Y' TO MASTER-EOF

           END-READ.
