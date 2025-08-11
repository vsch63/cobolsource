000010*Property of FIS©. or its affiliates
000020*all rights reserved. FIS Confidential
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.           CACBALRB.
000300 AUTHOR.               LEENA DASGUPTA
000400 DATE-WRITTEN.         1996-09-20.
000500****************************************************************
000080*   $Modtime::   10 Oct 2018 10:15:16  $
000090*   $Log::   P:/pvcsproj/DONOTUSE.prj/source/CACBALRB.COV  $
000100*
000110*   Rev 1.0.1.11   10 Oct 2018 11:45:20   alex
000120*FDM4167
000130*fis copyright
000140*
000150*   Rev 1.0.1.10   19 Apr 2011 17:27:22   tom
000160*LSM5259
000170*Replace call to CUTDTCAL
000180*
000190*   Rev 1.0.1.9   23 Sep 2005 11:41:18   robin
000200*FDM3514
000210*Module Impact Analysis Tool - WS-REVISION awk script fix.
000220*
000230*   Rev 1.0.1.8   19 Sep 2005 18:58:12   robin
000240*FDM3514
000250*Module Impact Analysis Tool
000260*
000270*   Rev 1.0.1.7   22 Nov 2004 18:27:16   gabriela
000280*FLM0001
000290*scheme level currency
000300*
000310*   Rev 1.0.1.6   20 Dec 2002 10:55:52   rieko
000320*FDM3175
000330*COBOL modifications to support the Oracle 8 pre-complier.
000340*
000350*   Rev 1.0.1.5   Mar 24 2000 17:42:26   miker
000360*FDM1959
000370*Expanded Keys project
*******
*******   Rev 1.0.1.4   10 Sep 1997 17:02:10   dan
*******lmm0984
*******
*******   Rev 1.0.1.3   10 Jul 1997 15:17:32   andrea
*******FDM0786. Currency/unit value changes.
*******
*******   Rev 1.0.1.2   10 Jul 1997 14:42:56   paulm
*******Mass Check In for Currency Field Changes
*******
*******   Rev 1.0.1.1   09 Jul 1997 13:50:42   paulm
*******FDM0786
*******
*******   Rev 1.0.1.0   May 03 21:09:36 1997   paulm
*******No ERF
*******
*******Initial Check In for FDP Branch
*******3 May 1997
*******
*******   Rev 1.0   03 May 1997 14:28:48   paulm
*******No ERF
*******
*******Initial Check-In   3 May 1997
000600* MONTH END ACCOUNT BALANCING
000700*
000800****************************************************************
000900* THIS PROGRAM CREATES A REPORT OF BEGINING AND ENDING ACCOUNT
001000* BALANCES, ACCOUNT NUMBER ,ACCOUNT NAME, DEBITS, CREDITS AND
001100* ACCOUNT COMPANY CODE. IT INSERTS PRINDATA RECORDS FOR THE
001101* HEADER AND DETAIL LINES. THESE RECORDS WILL BE PICKED UP BY
001102* ANOTHER PROGRAM AND FORMATTED FOR THE HARD COPY OF THE REPORT.
001110* THIS REPORT WILL BE RUN ON A MONTHLY BASIS IN BATCH, OR ON
001120* REQUEST BY A USER FROM A SCREEN. THIS PROGRAM IS CALLED BY THE
001130* SUPER STUB.
001140*
001200* THE FOLLOWING TABLES ARE UTILIZED AS INPUT:
001300* 1.) COMPANY_ACCTS
001400* 2.) SYSTEM_DATA
001500* 3.) LEDGER_ACCOUNTS
001600* 4.) JOURNAL_LOGS
001610* 4.) PRINTDATA
001700*
001800****************************************************************
001900*    THE FOLLOWING ERROR CODES MAY BE RETURNED IN RTN-ERROR-CODE
002000*    FROM THIS PROGRAM...
002100*
002200*    999X SEVERE SQL ERROR IN RISK PROCESSING
002300*
002400****************************************************************
002500*                     CHANGES LOG
002600****************************************************************
002700*
002800*   DATE     PRG   PROJ#      DESCRIPTION
002900*  --------  --- ---------  ----------------------------------------
003000*
003100*  10-04-96  LJD  DVAC001   DATABASE CHANGES
003110*  10-11-96  LJD  DVAC001   CHANGED DELIMITERS FOR DETAIL AND HEADER
003200*                           RECORDS
003210*  12-12-96  AEC            ADDED NEW FIELDS FOR PRINTDATA TABLE
003211*  02-28-97  AEC            ADDED CODE TO MOVE 'HEAD' INTO RECORDTYPE
003220*                           FOR HEADERS AND 'DETL' INTO RECORDTYPE
003230*                           FOR DETAIL LINES.
003240*  03-24-97  DEW  AFI0038   DATABASE CHANGES TO LEDGER_ACCOUNTS
003250*                           CO-BAL-CD, CASE-BAL-CD, AND PROD-BAL-CD
003260*                           WERE REMOVED AND BAL-LVL-CD WAS ADDED
003270*  4-7-1997  AEC           ADDED SUB_ACCT_NO (NEW FIELD, LEDGER_ACCOUNTS)
001040*  7-7-1997  AEC  FDM9786    INCREASE SIZE OF CURRENCY FIELDS.
003240*  09-10-97  DEW  LMM0984   CHANGED BALANCE DATE TO MONTH END DATE
001060* 17-NOV-04  GVC  FLM0001   ADD EDIT FOR NEW SCHEME CURRENCY CD
003300***************************************************************
003400 ENVIRONMENT DIVISION.
003500 CONFIGURATION SECTION.
003600*
003700 DATA DIVISION.
003800 WORKING-STORAGE SECTION.
003900
003920
004000 01 BEG-WORK-STOR.
004100    05 FILLER                             PIC X(40) VALUE
004200       'THIS IS THE BEGINNING OF WORKING STORAGE'.
004300
004400 01  WS-PGM-NAME                          PIC X(8)
004500                                          VALUE 'CACBALRB'.
001210 01  WS-COMPILED                   PIC X(20).                     FLM0001
004600
004700 01  WS-SUBSCRIPTS.
004900     05  IACC                             PIC S9(4) COMP.
005000
005100 01  WS-MISC-VARIABLES.
006400     05  WS-CURRENT-ACCT-BAL-DT.
006500         10  WS-ISO-YYYY2                 PIC 9999.
006600         10  FILLER                       PIC X.
006700         10  WS-ISO-MM2                   PIC 99.
006800         10  FILLER                       PIC X.
006900         10  WS-ISO-DD2                   PIC 99.
006901     05  WS-SEQ-NUM                       PIC 9(8).
005000
004700     05  WS-BALANCE-LEVEL            PIC X(2) VALUE SPACES.
004900         88 LEDGER-LEVEL                  VALUE '01'.
004900         88 COMPANY-LEVEL                 VALUE '02'.
004900         88 PRODUCT-LEVEL                 VALUE '03'.
004900         88 SCHEME-LEVEL                  VALUE '04'.
007005
006902
006903 01  CHR34-NUM PIC 9(4) BINARY VALUE 34.
006904 01  CHR34-NUM-REDEF REDEFINES CHR34-NUM.
006905     05  FILLER PIC X.
006906     05  CHR34 PIC X.
006907
006908**   PUTS IN A DOUBLE QUOTE
006909 01  HEAD-REC.
006912     05  FILLER1                          PIC X(1).
006913     05  HEAD-LAST-ACCT-BAL-DT            PIC X(10).
006914     05  FILLER                           PIC X(3) VALUE '","'.
006915     05  HEAD-NEW-ACCT-BAL-DT             PIC X(10).
006916     05  FILLER2                          PIC X(1).
006917
006918 01  DETL-REC.
006921     05  FILLER3                          PIC X(1).
006922     05  DETL-ACCT-NO                     PIC X(12).
006923     05  FILLER                           PIC X(3) VALUE '","'.
006924     05  DETL-SUB-ACCT-NO                 PIC X(12).
006925     05  FILLER                           PIC X(3) VALUE '","'.
006926     05  DETL-ACCT-NM                     PIC X(30).
006927     05  FILLER                           PIC X(3) VALUE '","'.
006928     05  DETL-MKEY-CO-NUM                 PIC 9(5).
006929     05  FILLER                           PIC X(3) VALUE '","'.
006930     05  DETL-BEG-BAL-AMT                 PIC -ZZZZZZZZZZZZ9.99.
006931     05  FILLER                           PIC X(3) VALUE '","'.
006932     05  DETL-TOT-CREDITS                 PIC -ZZZZZZZZZZZZ9.99.
006933     05  FILLER                           PIC X(3) VALUE '","'.
006934     05  DETL-TOT-DEBITS                  PIC -ZZZZZZZZZZZZ9.99.
006935     05  FILLER                           PIC X(3) VALUE '","'.
006936     05  DETL-END-BAL-AMT                 PIC -ZZZZZZZZZZZZ9.99.
006950     05  FILLER4                          PIC X(1).
001730
001740 01  WS-ERROR-MESSAGES.                                           FLM0001
001750     05  WS-ERR-MSG1.                                             FLM0001
001760         10  FILLER                       PIC X(15)               FLM0001
001770             VALUE 'BEG BAL DATE: '.                              FLM0001
001780         10  WS-ERR-BEG-BAL-DT            PIC X(10).              FLM0001
001790
001800     05  WS-ERR-MSG2.                                             FLM0001
001810         10  FILLER                       PIC X(15)               FLM0001
001820             VALUE 'END BAL DATE: '.                              FLM0001
001830         10  WS-ERR-END-BAL-DT            PIC X(10).              FLM0001
007000
007005
007010*    SYSTEM DATA COPYBOOK
007100     COPY CL1SYSDD.
007110*    LEDGER ACCOUNT COPYBOOK
007200     COPY CL2ACCTD.
007210*    COMPANY ACCOUNTS COPYBOOK
007300     COPY CL1COACD.
001920**   JOURNAL_LOGS                                                 FLM0001
001930     COPY CL1JLOGD.                                               FLM0001
007301*    JOURNAL LOG COPYBOOK
007310     COPY CL3JLOGD.
007311*    PRINTDATA COPYBOOK
007312     COPY CL1PRDTD.
007313*    DATE CALCULATION COPYBOOK
007320     COPY CWSDTCAL.
007400
002010**  REVISION AREA **                                              FDM3514
002020 01  WS-REVISION           PIC X(35)  VALUE                       FDM3514
002030     ' $Revision::   1.0.1.11       $'.                           
007500 LINKAGE SECTION.
007600*    RMESSAGE AREA
007700     COPY CLKFDPNC.
007800*    CASE  DATA COPYBOOK
007900     COPY CL1CASDD.
008000*    STANDARD ERROR BLOCK
008100     COPY CLKERRBC.
008200
008300*********************************************************
008400**   P R O C E D U R E    D I V I S I O N              **
008500*********************************************************
008600
008700 PROCEDURE DIVISION USING RMESSAGE,
008800                          CASD1-DATA,
008900                          RTN-OUTPUT-FIELDS.
002190     IF FDP-DEBUG-RUNSTAT > '00'                                  FDM3514                                             FLM0001
002200        ADD +1  TO RTN-DIMT-CNT                                   FDM3514
002210        MOVE WHEN-COMPILED TO                                     FDM3514
002220             RTN-DIMT-COMPILE-DATE (RTN-DIMT-CNT)                 FDM3514
002230        MOVE WS-REVISION (16:10) TO                               FDM3514
002240             RTN-DIMT-REVISION (RTN-DIMT-CNT)                     FDM3514
002250        MOVE 'CACBALRB' TO                                        FDM3514
002260           RTN-DIMT-PROGRAM-NAME (RTN-DIMT-CNT)                   FDM3514
002270        CALL 'CUTDIMTD' USING RMESSAGE,                           FDM3514
002280                              RTN-OUTPUT-FIELDS                   FDM3514
002290     END-IF.                                                      FDM3514
009000
009100******************************************************************
009200**                MAIN ROUTINE                                  **
009300******************************************************************
009400
009500     PERFORM A000-INITIALIZE
009600        THRU A000-INITIALIZE-EXIT.
009700
002380     IF RTN-NO-ERROR                                              FLM0001
002390*       CHECK FOR MULTI-CURRENCY                                  FLM0001
002400        PERFORM A120-CHECK-ISO-CURR                               FLM0001
002410           THRU A120-EXIT                                         FLM0001
002420     END-IF.                                                      FLM0001
002430
002440     IF RTN-NO-ERROR                                              FLM0001
002450        PERFORM B000-MAIN
002460           THRU B000-MAIN-EXIT
002470     END-IF.                                                      FLM0001
010000
010100     GOBACK.
010200
010300 A000-INITIALIZE.
009100******************************************************************
009100*
009100******************************************************************
010400
010600     INITIALIZE WS-MISC-VARIABLES.
011400     MOVE CHR34   TO  FILLER1  FILLER2 FILLER3 FILLER4.
011300
011410*    GET LAST MONTH ACCOUNT BALANCE DATE FROM SYSTEM_DATA
011600     CALL 'CS1SYSDD' USING RMESSAGE,
011700                           SYSD1-DATA,
011800                           RTN-OUTPUT-FIELDS.
012061     MOVE SYSD1-LAST-ACCT-BAL-DT TO HEAD-LAST-ACCT-BAL-DT.
002640
002650* VALIDATE THE LAST MONTH ACCOUNT BALANCE DATE FROM SYSTEM_DATA
020300* IT SHOULD BE A MONTH END DATE
002670     IF RTN-NO-ERROR
002680        CALL 'CS2SYSDD' USING RMESSAGE,
002690                              SYSD1-DATA,
002700                              RTN-OUTPUT-FIELDS
002710     END-IF.
020400**
011900
012020*    CALCULATE NEXT MONTH END DATE, SAVE IN WORKING STORAGE
012020*    THIS DATE WILL BE USED FOR THE CURRENT MONTH BALANCING
012030     IF RTN-NO-ERROR
012040        INITIALIZE DATEPARMS
012050        MOVE 110 TO REQUEST-TYPE
012060        MOVE SYSD1-LAST-ACCT-BAL-DT TO INDATE
012070        MOVE +1 TO MONTHS-DAY-CHG
012080
002820        CALL 'CUTDTCAB' USING RMESSAGE,                           LSM5259
002830                              DATEPARMS,                          LSM5259
002840                              RTN-OUTPUT-FIELDS                   LSM5259
012091
012092        IF LK-ERROR-CD NOT = ZERO
012093           MOVE 'UT'        TO RTN-ERR-PREFIX
012094           MOVE 1100        TO RTN-ERROR-CODE
012095           MOVE 'CACBALRB'  TO RTN-PROG-NAME
002900           MOVE 'A000-INIT' TO RTN-PARA-NAME                      FLM0001
012097           CALL 'CUTSVARB' USING RMESSAGE,
012098                                RTN-OUTPUT-FIELDS
012099           DISPLAY 'ERROR IN DATE CALCULATION PROGRAM'
012100           DISPLAY 'CALLED FROM CACBALRB'
012101        ELSE
012102           MOVE ODATE TO WS-CURRENT-ACCT-BAL-DT
012103           MOVE ODATE TO HEAD-NEW-ACCT-BAL-DT
012110        END-IF
012030     END-IF.
012200
012210*    INSERT A PRINTDATA RECORD FOR THE REPORT HEADING
012030     IF RTN-NO-ERROR
012230        ADD    +1    TO    WS-SEQ-NUM
012240
012300        MOVE  FDP-REF-KEY           TO   PRDT1-RPTREQUESTID
012400        MOVE  WS-SEQ-NUM            TO   PRDT1-SEQNUM
012500        MOVE  HEAD-REC              TO   PRDT1-CDL
012600        MOVE  'N'                   TO   PRDT1-CONTINUECD
012700        MOVE  ZEROES                TO   PRDT1-AUDITID
012710        MOVE 'HEAD'                 TO   PRDT1-RECORDTYPE
012800        MOVE SPACES                 TO   PRDT1-SORTDATA
012810
012900        CALL 'CI1PRDTD' USING RMESSAGE,
013000                              PRDT1-DATA,
013100                              RTN-OUTPUT-FIELDS
012030     END-IF.
013200
010800 A000-INITIALIZE-EXIT.
010900     EXIT.
011000
003210
003220**********************************************************        FLM0001
003230*  CHECK FOR MULTIPLE CURRENCY CODES ON JLOGS TO BALANCE          FLM0001
003240**********************************************************        FLM0001
003250
003260 A120-CHECK-ISO-CURR.                                             FLM0001
003270
003280     INITIALIZE JLOG1-DATA.                                       FLM0001
003290     MOVE SYSD1-LAST-ACCT-BAL-DT TO JLOG1-BEGIN-DT.               FLM0001
003300     MOVE WS-CURRENT-ACCT-BAL-DT TO JLOG1-END-DT.                 FLM0001
003310     CALL 'CS6JLOGD' USING RMESSAGE,                              FLM0001
003320                           JLOG1-DATA,                            FLM0001
003330                           RTN-OUTPUT-FIELDS.                     FLM0001
003340
003350     IF RTN-NO-ERROR                                              FLM0001
003360        IF JLOG1-ROWS-FOUND > 1                                   FLM0001
003370           MOVE 'UT'    TO RTN-ERR-PREFIX                         FLM0001
003380           MOVE 317     TO RTN-ERROR-CODE                         FLM0001
003390           MOVE 'CACBALRB' TO RTN-PROG-NAME                       FLM0001
003400           MOVE 'A120'  TO RTN-PARA-NAME                          FLM0001
003410           MOVE SYSD1-LAST-ACCT-BAL-DT TO WS-ERR-BEG-BAL-DT       FLM0001
003420           MOVE WS-CURRENT-ACCT-BAL-DT TO WS-ERR-END-BAL-DT       FLM0001
003430           MOVE WS-ERR-MSG1          TO RTN-VAR-TEXT   (1)        FLM0001                             000167300
003440           MOVE 1                    TO RTN-VAR-SEQ-NO (1)        FLM0001                           000167400
003450           MOVE WS-ERR-MSG2          TO RTN-VAR-TEXT   (2)        FLM0001                             000167500
003460           MOVE 2                    TO RTN-VAR-SEQ-NO (2)        FLM0001                           000167600
003470           MOVE 2                    TO RTN-VAR-DETL-CNT          FLM0001
003480           CALL 'CUTSVARB' USING RMESSAGE,                        FLM0001
003490                                 RTN-OUTPUT-FIELDS                FLM0001
003500        END-IF                                                    FLM0001
003510     END-IF.                                                      FLM0001
003520
003530 A120-EXIT.                                                       FLM0001
003540     EXIT.
003550**********************************************************
011100
011200 B000-MAIN.
009100******************************************************************
009100*
009100******************************************************************
015110*    START LOOP TO PROCESS JOURNAL LOG ENTRIES FOR EACH ACCOUNT
015120*
015130     INITIALIZE ACCT2-DATA.
015140
015150     PERFORM UNTIL ACCT2-END-FLAG = 'Y'
015160                   OR RTN-ERROR
015180*
015190*       FETCH NEXT BLOCK OF LEDGER_ACCOUNTS
015191*       GET ALL ACCOUNT NUMBERS FROM LEDGER_ACCOUNTS
015192
015193        CALL 'CF1ACCTD'    USING RMESSAGE,
015194                                 ACCT2-DATA,
015195                                 RTN-OUTPUT-FIELDS
015196
015197**
015198**      PROCESS EACH ACCOUNT
015199**
015200        PERFORM VARYING IACC FROM 1 BY 1
015201        UNTIL IACC > ACCT2-ROWS-FOUND OR RTN-ERROR
015202
015204
015216**      THE COMPANY, PRODUCT AND SCHEME
015217**      BALANCING ARE NOT AVAILABLE IN THIS RELEASE
015218**
015229**      01 = LEDGER ACCOUNT BALANCING
015230           MOVE ACCT2-BAL-LVL-CD (IACC) TO WS-BALANCE-LEVEL
015230           IF LEDGER-LEVEL
015231              CONTINUE
015222           ELSE
015233**            ISSUE A WARNING AND FALL THRU TO END-PERFORM
015223              MOVE 'TX'              TO RTN-ERR-PREFIX
015224              MOVE 1001              TO RTN-ERROR-CODE
015225              MOVE 'CACBALRB'        TO RTN-PROG-NAME
015226              MOVE 'B000-MAIN '      TO RTN-PARA-NAME
015227              CALL 'CUTSVARB' USING RMESSAGE,
015228                                    RTN-OUTPUT-FIELDS
015229           END-IF
015230
015234           IF RTN-NO-ERROR  AND LEDGER-LEVEL
015231              PERFORM B100-GET-COMPANY-ACCTS
015232                 THRU B100-GET-COMPANY-ACCTS-EXIT
015239           END-IF
015233
015234           IF RTN-NO-ERROR AND LEDGER-LEVEL
015235              IF COAC1-ROWS-FOUND = ZERO
015236                 MOVE ZEROES TO COAC1-BEG-BAL-AMT
015237                 MOVE ZEROES TO COAC1-END-BAL-AMT
015238              END-IF
015239           END-IF
015240
015241           IF RTN-NO-ERROR AND LEDGER-LEVEL
015242              PERFORM B125-SUM-JLOG-ENTRIES
015243                 THRU B125-SUM-JLOG-ENTRIES-EXIT
015244           END-IF
015245
015246           IF RTN-NO-ERROR AND LEDGER-LEVEL
015247              PERFORM B150-INSERT-PRINTDATA
015248                 THRU B150-INSERT-PRINTDATA-EXIT
015249           END-IF
015250
015251        END-PERFORM
015252
015253     END-PERFORM.
015254
015255
015260 B000-MAIN-EXIT.
015300     EXIT.
015400
015410 B100-GET-COMPANY-ACCTS.
009100******************************************************************
015501*    GET THE COMPANY_ACCTS  TABLE  FOR THE ACCOUNT KEY FROM
015502*    LEDGER_ACCOUNTS
009100******************************************************************
015503
015504     MOVE ACCT2-ACCT-KEY (IACC)    TO  COAC1-ACCT-KEY.
015506     MOVE WS-CURRENT-ACCT-BAL-DT   TO  COAC1-ACCT-BAL-DT.
015507
015508     CALL 'CS1COACD' USING RMESSAGE,
015509                           COAC1-DATA,
015510                           RTN-OUTPUT-FIELDS.
015516
015517 B100-GET-COMPANY-ACCTS-EXIT.
015520     EXIT.
015521
015530 B125-SUM-JLOG-ENTRIES.
009100******************************************************************
015532*    SUM ALL THE DEBITS AND CREDITS BETWEEN THE LAST MONTH
015533*    ACCOUNT BALANCE DATE(FROM SYSTEM_DATA) AND THE NEW
015534*    ACCOUNT BALANCE DATE(FROM WORKING STORAGE)
009100******************************************************************
015535
015536     MOVE  ACCT2-ACCT-KEY (IACC)   TO  JLOG3-ACCT-KEY.
015538     MOVE  SYSD1-LAST-ACCT-BAL-DT  TO  JLOG3-LAST-ACCT-BAL-DT1.
015539     MOVE  WS-CURRENT-ACCT-BAL-DT  TO  JLOG3-NEW-ACCT-BAL-DT.
015540
015541     CALL 'CS1JLOGD' USING RMESSAGE,
015542                           JLOG3-DATA,
015543                           RTN-OUTPUT-FIELDS.
015544
015549
015550 B125-SUM-JLOG-ENTRIES-EXIT.
015551     EXIT.
015560
015570 B150-INSERT-PRINTDATA.
009100******************************************************************
015590*    MOVE DATA INTO THE DETAIL PRINT AREA IN WORKING STORAGE
009100******************************************************************
015593
015594     MOVE ACCT2-ACCT-NO (IACC)          TO   DETL-ACCT-NO.
015595     MOVE ACCT2-SUB-ACCT-NO (IACC)      TO   DETL-SUB-ACCT-NO.
015596     MOVE ACCT2-ACCT-NM (IACC)          TO   DETL-ACCT-NM.
015597     MOVE COAC1-MKEY-CO-NUM             TO   DETL-MKEY-CO-NUM.
015598     MOVE COAC1-BEG-BAL-AMT             TO   DETL-BEG-BAL-AMT.
015599     MOVE JLOG3-TOT-CREDITS             TO   DETL-TOT-CREDITS.
015600     MOVE JLOG3-TOT-DEBITS              TO   DETL-TOT-DEBITS.
015601     COMPUTE DETL-END-BAL-AMT       =    COAC1-BEG-BAL-AMT +
015602                                         JLOG3-DB-CR-TOTAL.
015603
015604*    INCREMENT COUNTER FOR PRINT  LINES
015605     ADD    +1    TO    WS-SEQ-NUM.
015606
015607*    INSERT A NEW ROW INTO PRINTDATA FOR THE DETAIL LINE
015608
015609     MOVE  FDP-REF-KEY               TO   PRDT1-RPTREQUESTID.
015610     MOVE  WS-SEQ-NUM                TO   PRDT1-SEQNUM.
015611     MOVE  DETL-REC                  TO   PRDT1-CDL.
015612     MOVE  'N'                       TO   PRDT1-CONTINUECD.
015613     MOVE  ZEROES                    TO   PRDT1-AUDITID.
015614     MOVE 'DETL'                     TO   PRDT1-RECORDTYPE.
015615     MOVE SPACES                     TO   PRDT1-SORTDATA.
015616
015617
015618     CALL 'CI1PRDTD' USING RMESSAGE,
015619                           PRDT1-DATA,
015620                           RTN-OUTPUT-FIELDS.
015621
015622 B150-INSERT-PRINTDATA-EXIT.
015623     EXIT.
015624
015630**
015700**   E N D     O F    P R O G R A M
015800**
015900
