      ******************************************************************
      * Converts a line sequential master file to a native indexed file.
      * Usage:
      *   createIndexedFile [input-master] [output-indexed-master]
      * Defaults:
      *   input-master         = master.dat
      *   output-indexed-master = indexedMaster
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATE-INDEXED-FILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-MASTER-FILE ASSIGN TO DYNAMIC WS-INPUT-FILE
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.

           SELECT OUTPUT-MASTER-FILE ASSIGN TO DYNAMIC WS-OUTPUT-FILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS O-ACC-NUMBER
               FILE STATUS IS WS-OUT-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  INPUT-MASTER-FILE.
       01  INPUT-MASTER-REC.
           05 I-ACC-NUMBER    PIC X(10).
           05 I-ACC-NAME      PIC X(20).
           05 I-BALANCE       PIC 9(10).
           05 I-ACC-TYPE      PIC X.
           05 I-ACC-STATUS    PIC X.

       FD  OUTPUT-MASTER-FILE.
       01  OUTPUT-MASTER-REC.
           05 O-ACC-NUMBER    PIC X(10).
           05 O-ACC-NAME      PIC X(20).
           05 O-BALANCE       PIC 9(10).
           05 O-ACC-TYPE      PIC X.
           05 O-ACC-STATUS    PIC X.

       WORKING-STORAGE SECTION.
       01  WS-CMD-LINE                PIC X(200).
       01  WS-ARG-INPUT               PIC X(200).
       01  WS-ARG-OUTPUT              PIC X(200).
       01  WS-INPUT-FILE              PIC X(200)
           VALUE "master.dat".
       01  WS-OUTPUT-FILE             PIC X(200)
           VALUE "indexedMaster".
       01  WS-IN-STATUS               PIC XX.
       01  WS-OUT-STATUS              PIC XX.
       01  WS-EOF                     PIC X VALUE "N".
           88 INPUT-EOF VALUE "Y".
       01  WS-RECORDS-READ            PIC 9(9) VALUE 0.
       01  WS-RECORDS-WRITTEN         PIC 9(9) VALUE 0.
       01  WS-DUPLICATES-SKIPPED      PIC 9(9) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM LOAD-ARGUMENTS
           PERFORM OPEN-FILES
           PERFORM READ-FIRST-RECORD

           PERFORM UNTIL INPUT-EOF
               ADD 1 TO WS-RECORDS-READ
               MOVE INPUT-MASTER-REC TO OUTPUT-MASTER-REC
               WRITE OUTPUT-MASTER-REC
                   INVALID KEY
                       ADD 1 TO WS-DUPLICATES-SKIPPED
                       DISPLAY "Duplicate master account skipped: "
                           O-ACC-NUMBER
                   NOT INVALID KEY
                       ADD 1 TO WS-RECORDS-WRITTEN
               END-WRITE

               READ INPUT-MASTER-FILE
                   AT END
                       SET INPUT-EOF TO TRUE
               END-READ
           END-PERFORM

           CLOSE INPUT-MASTER-FILE
                 OUTPUT-MASTER-FILE

           DISPLAY "Master conversion complete."
           DISPLAY "Input file : " WS-INPUT-FILE
           DISPLAY "Output file: " WS-OUTPUT-FILE
           DISPLAY "Records read    : " WS-RECORDS-READ
           DISPLAY "Records written : " WS-RECORDS-WRITTEN
           DISPLAY "Duplicates skipped: " WS-DUPLICATES-SKIPPED

           STOP RUN.

       LOAD-ARGUMENTS.
           ACCEPT WS-CMD-LINE FROM COMMAND-LINE

           IF WS-CMD-LINE NOT = SPACES
               UNSTRING WS-CMD-LINE DELIMITED BY ALL SPACES
                   INTO WS-ARG-INPUT
                        WS-ARG-OUTPUT
               END-UNSTRING

               IF WS-ARG-INPUT NOT = SPACES
                   MOVE WS-ARG-INPUT TO WS-INPUT-FILE
               END-IF

               IF WS-ARG-OUTPUT NOT = SPACES
                   MOVE WS-ARG-OUTPUT TO WS-OUTPUT-FILE
               END-IF
           END-IF.

       OPEN-FILES.
           OPEN INPUT INPUT-MASTER-FILE
           IF WS-IN-STATUS NOT = "00"
               DISPLAY "Unable to open input master file: "
                   WS-INPUT-FILE
               DISPLAY "Input status: " WS-IN-STATUS
               STOP RUN
           END-IF

           OPEN OUTPUT OUTPUT-MASTER-FILE
           IF WS-OUT-STATUS NOT = "00"
               DISPLAY "Unable to create indexed master file: "
                   WS-OUTPUT-FILE
               DISPLAY "Output status: " WS-OUT-STATUS
               STOP RUN
           END-IF.

       READ-FIRST-RECORD.
           READ INPUT-MASTER-FILE
               AT END
                   SET INPUT-EOF TO TRUE
           END-READ.

       END PROGRAM CREATE-INDEXED-FILE.
