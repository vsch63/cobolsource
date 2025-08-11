000010*Property of FIS©. or its affiliates
000020*all rights reserved. FIS Confidential
000030 IDENTIFICATION DIVISION.
000040 PROGRAM-ID.    CACDBALB.
000050 AUTHOR.        ROB SIDLOSKI.
000060 DATE-WRITTEN.  1997-09-05.
000070*****************************************************************
000080*   $Modtime::   10 Oct 2018 10:15:16  $
000090*   $Log::   P:/pvcsproj/DONOTUSE.prj/source/CACDBALB.COV  $
000100*
000110*   Rev 1.0.1.13   10 Oct 2018 11:45:26   alex
000120*FDM4167
000130*fis copyright
000140*
000150*   Rev 1.0.1.12   19 Apr 2011 17:27:26   tom
000160*LSM5259
000170*Replace call to CUTDTCAL
000180*
000190*   Rev 1.0.1.11   28 Apr 2006 14:19:40   katrina
000200*LPM0066
000210*Contract Number Size Change
000220*
000230*   Rev 1.0.1.10   23 Sep 2005 11:41:20   robin
000240*FDM3514
000250*Module Impact Analysis Tool - WS-REVISION awk script fix.
000260*
000270*   Rev 1.0.1.9   19 Sep 2005 18:58:20   robin
000280*FDM3514
000290*Module Impact Analysis Tool
000300*
000310*   Rev 1.0.1.8   22 Nov 2004 18:27:38   gabriela
000320*FLM0001
000330*scheme level currency
000340*
000350*   Rev 1.0.1.7   20 Dec 2002 10:56:12   rieko
000360*FDM3175
000370*COBOL modifications to support the Oracle 8 pre-complier.
000380*
000390*   Rev 1.0.1.6   25 May 2002 15:23:48   surinder
000400*FTS0279
000410*Support Last Mod User requirements
000420*
000430*   Rev 1.0.1.5   27 Sep 2000 10:35:02   richardf
000440*MLV0916
000450*error BT1217 if  next-run-dt = spaces
000460*
000470*  DAILY ACCOUNT BALANCE REPORTING SYSTEM
000480*
000490*****************************************************************
000500*
000510*  THIS PROGRAM WILL CREATE A REPORT OF THE DAILY ACCOUNT
000520*  BALANCES FROM JOURNAL_LOGS. IT WILL STORE THE INFORMATION
000530*  IN PRINTDATA AND WILL BE FORMATTED BY ANOTHER PROGRAM.
000540*
000550*  REQUIRES:  FDP-CYC-DT,FDP-USER
000560*
000570*****************************************************************
000580*
000590*  THIS PROGRAM RETURNS THE FOLLOWING ERROR MESSAGES:
000600*
000610*
000620*****************************************************************
000630*                        CHANGES LOG
000640*****************************************************************
000650*
000660*   DATE     PRG    PROJ#       DESCRIPTION
000670* ---------- ---  ---------    ------------------------------
000680* 10-28-1997 RPS   LIM0028     ADD BEGIN. BALANCE, END BALANCE,
000690*                              AND DUE DT TO REPORT
000700* 11-20-1997 RPS   LIM0028     CHANGE ERROR MESSAGE - TX 1007 TO
000710*                              TX 1010
000720* 09-13-2000 RCF   MLV0916     ERROR BT-1217 IF NEXT-RUN-DT=SPACES
000730* 05-17-2002 SD    FTS0279     USE FDP-LAUNCHED-BY-CD INSTEAD OF FDP-USER
000740* 17-NOV-04  GVC   FLM0001     ADD EDIT FOR NEW SCHEME CURRENCY CD
000750* 23-SEP-05  RAM   FDM3514     ADDED CL1BJRLD COPYBOOK.  CS1VBJBD WAS
000760*                              LOOKING FOR IT.
000770*****************************************************************
000780 ENVIRONMENT DIVISION.
000790 CONFIGURATION SECTION.
000800*
000810 DATA DIVISION.
000820 WORKING-STORAGE SECTION.
000830*
000840 01  WS-COMPILED                   PIC X(20).                     MLV0916
000850*
000860 01  CHR34-NUM PIC 9(4) BINARY VALUE 34.
000870 01  CHR34-NUM-REDEF REDEFINES CHR34-NUM.
000880     05  FILLER                    PIC X.
000890     05  CHR34                     PIC X.
000900
000910 01  WS-HOST-VARIABLES.
000920     05  WS-MISC-VARIABLES.
000930         10  WS-SEQ-NUM            PIC 9(08).
000940         10  WS-RUN-DT             PIC X(10).
000950         10  WS-BEGIN-DT.
000960             15  WS-YRS            PIC X(04).
000970             15  WS-FILL-1         PIC X(01).
000980             15  WS-MONTH          PIC X(02).
000990             15  WS-FILL-2         PIC X(01).
001000             15  WS-DAY            PIC X(02).
001010         10  WS-END-DT             PIC X(10).
001020         10  WS-BALANCE-DT         PIC X(10).
001030         10  WS-HOLD-ACCT-KEY      PIC S9(15) COMP-3.             FDM1959
001040         10  WS-HOLD-ACCT-NO       PIC X(80).
001050         10  WS-HOLD-SUB-ACCT-NO   PIC X(12).
001060         10  WS-BEG-BAL            PIC S9(13)V9(2).
001070         10  WS-END-BAL            PIC S9(13)V9(2).
001080         10  WS-BAL                PIC S9(13)V9(2).
001090
001100     05  WS-SUBSCRIPTS.
001110         10  SUB0                  PIC S9(4) COMP.
001120         10  IJLOG                 PIC S9(4) COMP.
001130
001140     05  HEAD-REC.
001150         10  FILLER1               PIC X(01).
001160         10  HEAD-DT               PIC X(10).
001170         10  FILLER2               PIC X(01).
001180
001190     05  DETL-REC.
001200         10  FILLER3               PIC X(01).
001210         10  DETL-FUND             PIC 9(05).
001220         10  FILLER                PIC X(03) VALUE '","'.
001230         10  DETL-ACCT-NO          PIC X(50).
001240         10  FILLER                PIC X(03) VALUE '","'.
001250         10  DETL-SUB-ACCT-NO      PIC X(12).
001260         10  FILLER                PIC X(03) VALUE '","'.
001270         10  DETL-BEG-BAL          PIC -ZZZZZZZZZZZZ9.99.
001280         10  FILLER                PIC X(03) VALUE '","'.
001290         10  DETL-END-BAL          PIC -ZZZZZZZZZZZZ9.99.
001300         10  FILLER                PIC X(03) VALUE '","'.
001310         10  DETL-SCHEME-NO        PIC X(15).                     LPM0066
001320         10  FILLER                PIC X(03) VALUE '","'.
001330         10  DETL-BOOK-DT          PIC X(10).
001340         10  FILLER                PIC X(03) VALUE '","'.
001350         10  DETL-EFF-DT           PIC X(10).
001360         10  FILLER                PIC X(03) VALUE '","'.
001370         10  DETL-AMOUNT-DB        PIC -ZZZZZZZZZZZZ9.99.
001380         10  FILLER                PIC X(03) VALUE '","'.
001390         10  DETL-AMOUNT-CR        PIC -ZZZZZZZZZZZZ9.99.
001400         10  FILLER4               PIC X(01).
001410
001420 01  WS-ERROR-MESSAGES.                                           FLM0001
001430     05  WS-ERR-MSG1.                                             FLM0001
001440         10  FILLER                       PIC X(15)               FLM0001
001450             VALUE 'BEG BAL DATE: '.                              FLM0001
001460         10  WS-ERR-BEG-BAL-DT            PIC X(10).              FLM0001
001470
001480     05  WS-ERR-MSG2.                                             FLM0001
001490         10  FILLER                       PIC X(15)               FLM0001
001500             VALUE 'END BAL DATE: '.                              FLM0001
001510         10  WS-ERR-END-BAL-DT            PIC X(10).              FLM0001
001520
001530     05  WS-ERR-MSG3.                                             FLM0001
001540         10  FILLER                       PIC X(15)               FLM0001
001550             VALUE 'TRANSACTION : '.                              FLM0001
001560         10  WS-ERR-TR-CD                 PIC X(04).              FLM0001
001570
001580
001590**
001600** OTHER WORKING STORAGE COPYBOKS
001610**
001620
001630
001640**   BAT_JOB_RULES                                                FDM3514
001650     COPY CL1BJRLD.                                               FDM3514
001660**   BAT_JOB_RULES/BAT_JB_GRP_LOGS/BAT_JB_LOGS
001670     COPY CL1VBJBD.
001680**   RPTDEF, RPTVERS, RPTREFLNK
001690     COPY CL1VRPDD.
001700**   JOURNAL_LOGS,LEDGER_ACCOUNTS,CASE_DATA
001710     COPY CL1VJLLD.
001720**   RPTREQUEST
001730     COPY CL1RPRQD.
001740**   JOURNAL_LOGS                                                 FLM0001
001750     COPY CL1JLOGD.                                               FLM0001
001760**   JOURNAL_LOGS
001770     COPY CL4JLOGD.
001780**   COMPANY_ACCTS
001790     COPY CL1COACD.
001800**   DATEPARMS
001810     COPY CWSDTCAL.
001820**   LEDGER_ACCOUNTS
001830     COPY CL1ACCTD.
001840**   PRINTDATA
001850     COPY CL1PRDTD.
001860
001870**  REVISION AREA **                                              FDM3514
001880 01  WS-REVISION           PIC X(35)  VALUE                       FDM3514
001890     ' $Revision::   1.0.1.13       $'.
001900 LINKAGE SECTION.
001910     COPY CLKFDPNC.
001920     COPY CL1CASDD.
001930     COPY CLKERRBC.
001940
001950*****************************************************************
001960** PROCEDURE DIVISION                                          **
001970*****************************************************************
001980
001990 PROCEDURE DIVISION USING RMESSAGE,
002000                          CASD1-DATA,
002010                          RTN-OUTPUT-FIELDS.
002020
002030     IF FDP-DEBUG-RUNSTAT > '00'                                  FDM3514
002040        ADD +1  TO RTN-DIMT-CNT                                   FDM3514
002050        MOVE WHEN-COMPILED TO                                     FDM3514
002060             RTN-DIMT-COMPILE-DATE (RTN-DIMT-CNT)                 FDM3514
002070        MOVE WS-REVISION (16:10) TO                               FDM3514
002080             RTN-DIMT-REVISION (RTN-DIMT-CNT)                     FDM3514
002090        MOVE 'CACDBALB' TO                                        FDM3514
002100           RTN-DIMT-PROGRAM-NAME (RTN-DIMT-CNT)                   FDM3514
002110        CALL 'CUTDIMTD' USING RMESSAGE,                           FDM3514
002120                              RTN-OUTPUT-FIELDS                   FDM3514
002130     END-IF.                                                      FDM3514
002140
002150*****************************************************************
002160** MAIN ROUTINE                                                **
002170*****************************************************************
002180 A000-MAIN.
002190
002200     PERFORM B100-INITIALIZE
002210        THRU B100-EXIT.
002220
002230     IF RTN-NO-ERROR
002240        PERFORM B200-EDIT-LINKAGE
002250           THRU B200-EXIT
002260     END-IF.
002270
002280     IF RTN-NO-ERROR
002290        PERFORM B300-SETUP-REPORT
002300           THRU B300-EXIT
002310     END-IF.
002320*    SET WS-BALANCE-DT HERE BECAUSE IT ONLY NEEDS TO BE DONE ONCE FLM0001
002330     IF RTN-NO-ERROR
002340        PERFORM C100-SUBTRACT-A-MONTH
002350           THRU C100-EXIT
002360     END-IF.
002370
002380     IF RTN-NO-ERROR
002390        PERFORM C200-GET-LAST-DAY-OF-MONTH
002400           THRU C200-EXIT
002410     END-IF.
002420
002430     IF RTN-NO-ERROR                                              FLM0001
002440*       CHECK FOR MULTI-CURRENCY                                  FLM0001
002450        PERFORM A120-CHECK-ISO-CURR                               FLM0001
002460           THRU A120-EXIT                                         FLM0001
002470     END-IF.                                                      FLM0001
002480
002490     IF RTN-NO-ERROR
002500        PERFORM B400-BUILD-REPORT-HEADER
002510           THRU B400-EXIT
002520     END-IF.
002530
002540     PERFORM UNTIL VJLD1-END-FLAG = 'Y'
002550        OR RTN-ERROR
002560
002570**   THIS FETCH GETS ALL ENTRIES FROM JOURNAL_LOGS,
002580**   LEDGER_ACCOUNTS FOR THE BATCH CYCLE DATE
002590        MOVE WS-RUN-DT             TO VJLD1-RUN-DT
002600        CALL 'CF2VJLLD' USING RMESSAGE,
002610                              VJLD1-DATA,
002620                              RTN-OUTPUT-FIELDS
002630
002640**      INITIALIZE HOLD VARIABLE FOR ACCT-KEY
002650        MOVE ZEROS TO WS-HOLD-ACCT-KEY
002660
002670        PERFORM VARYING SUB0 FROM 1 BY 1 UNTIL
002680           SUB0 > VJLD1-ROWS-FOUND OR RTN-ERROR
002690
002700           PERFORM B500-GET-ACCT-KEY
002710              THRU B500-EXIT
002720
002730           IF RTN-NO-ERROR
002740              IF ACCT1-ACCT-KEY <> WS-HOLD-ACCT-KEY
002750
002760                 MOVE ACCT1-ACCT-KEY TO WS-HOLD-ACCT-KEY
002770
002780                 PERFORM B600-GET-ACCOUNT-BALANCES
002790                    THRU B600-EXIT
002800
002810              END-IF
002820
002830              PERFORM B700-PRINT-DETAIL-LINE
002840                 THRU B700-EXIT
002850           END-IF
002860
002870        END-PERFORM
002880
002890     END-PERFORM.
002900
002910     GOBACK.
002920
002930 A000-EXIT.
002940     EXIT.
002950
002960**********************************************************        FLM0001
002970*  CHECK FOR MULTIPLE CURRENCY CODES ON JLOGS TO BALANCE          FLM0001
002980**********************************************************        FLM0001
002990
003000 A120-CHECK-ISO-CURR.                                             FLM0001
003010
003020     INITIALIZE JLOG1-DATA.                                       FLM0001
003030     MOVE WS-BALANCE-DT     TO  JLOG1-BEGIN-DT.                   FLM0001
003040     MOVE WS-RUN-DT         TO  JLOG1-END-DT.                     FLM0001
003050     CALL 'CS6JLOGD' USING RMESSAGE,                              FLM0001
003060                           JLOG1-DATA,                            FLM0001
003070                           RTN-OUTPUT-FIELDS.                     FLM0001
003080
003090     IF RTN-NO-ERROR                                              FLM0001
003100        IF JLOG1-ROWS-FOUND > 1                                   FLM0001
003110           MOVE 'UT'    TO RTN-ERR-PREFIX                         FLM0001
003120           MOVE 317     TO RTN-ERROR-CODE                         FLM0001
003130           MOVE 'CACDBALB' TO RTN-PROG-NAME                       FLM0001
003140           MOVE 'A120'  TO RTN-PARA-NAME                          FLM0001
003150           MOVE WS-BALANCE-DT        TO WS-ERR-BEG-BAL-DT         FLM0001
003160           MOVE WS-RUN-DT            TO WS-ERR-END-BAL-DT         FLM0001
003170           MOVE WS-ERR-MSG1          TO RTN-VAR-TEXT   (1)        FLM0001                             000167300
003180           MOVE 1                    TO RTN-VAR-SEQ-NO (1)        FLM0001                           000167400
003190           MOVE WS-ERR-MSG2          TO RTN-VAR-TEXT   (2)        FLM0001                             000167500
003200           MOVE 2                    TO RTN-VAR-SEQ-NO (2)        FLM0001                           000167600
003210           MOVE 2                    TO RTN-VAR-DETL-CNT          FLM0001
003220           CALL 'CUTSVARB' USING RMESSAGE,                        FLM0001
003230                                 RTN-OUTPUT-FIELDS                FLM0001
003240        END-IF                                                    FLM0001
003250     END-IF.                                                      FLM0001
003260
003270 A120-EXIT.                                                       FLM0001
003280     EXIT.
003290**********************************************************
003300** INITIALIZE                                                  **
003310*****************************************************************
003320
003330 B100-INITIALIZE.
003340
003350     INITIALIZE WS-HOST-VARIABLES.
003360     INITIALIZE VJLD1-DATA.
003370     INITIALIZE BJRL1-DATA.                                       FDM3514
003380
003390     MOVE CHR34      TO  FILLER1 FILLER2 FILLER3 FILLER4.
003400
003410 B100-EXIT.
003420
003430*****************************************************************
003440** EDIT LINKAGE FIELDS                                         **
003450*****************************************************************
003460
003470 B200-EDIT-LINKAGE.
003480*
003490     IF FDP-CYC-DT = SPACES AND RTN-NO-ERROR
003500        MOVE 'UT'            TO RTN-ERR-PREFIX
003510        MOVE 36              TO RTN-ERROR-CODE
003520        MOVE 'CACDBALB'      TO RTN-PROG-NAME
003530        MOVE 'B200'          TO RTN-PARA-NAME
003540        CALL 'CUTSVARB' USING RMESSAGE,
003550                              RTN-OUTPUT-FIELDS
003560     END-IF.
003570*
003580     IF FDP-USER = SPACES AND RTN-NO-ERROR
003590        MOVE 'UT'            TO RTN-ERR-PREFIX
003600        MOVE 39              TO RTN-ERROR-CODE
003610        MOVE 'CACDBALB'      TO RTN-PROG-NAME
003620        MOVE 'B200'          TO RTN-PARA-NAME
003630        CALL 'CUTSVARB' USING RMESSAGE,
003640                              RTN-OUTPUT-FIELDS
003650     END-IF.
003660*
003670 B200-EXIT.
003680
003690*****************************************************************
003700** SETUP REPORT                                                **
003710*****************************************************************
003720
003730 B300-SETUP-REPORT.
003740
003750*    SELECT RPTREFID
003760     INITIALIZE     VRPD1-DATA.
003770     MOVE '9011' TO VRPD1-TRANID.
003780
003790     CALL 'CS1VRPDD' USING RMESSAGE,
003800                           VRPD1-DATA,
003810                           RTN-OUTPUT-FIELDS.
003820
003830     IF VRPD1-ROWS-FOUND = 0 AND RTN-NO-ERROR                     FLM0001
003840        MOVE 'BT'                  TO RTN-ERR-PREFIX              FLM0001
003850        MOVE 1216                  TO RTN-ERROR-CODE              FLM0001
003860        MOVE 'CACDBALB'            TO RTN-PROG-NAME               FLM0001
003870        MOVE 'B300'                TO RTN-PARA-NAME               FLM0001
003880        MOVE '9011'                  TO WS-ERR-TR-CD              FLM0001
003890        MOVE WS-ERR-MSG3             TO RTN-VAR-TEXT   (1)        FLM0001                             000167300
003900        MOVE 1                       TO RTN-VAR-SEQ-NO (1)        FLM0001                           000167400
003910        MOVE 1                       TO RTN-VAR-DETL-CNT          FLM0001
003920        CALL 'CUTSVARB' USING RMESSAGE,                           FLM0001
003930                              RTN-OUTPUT-FIELDS                   FLM0001
003940     END-IF                                                       FLM0001
003950     IF VRPD1-ROWS-FOUND > 0 AND RTN-NO-ERROR
003960
003970**      SEE IF THIS IS A BATCH RUN OR AN AD HOC RUN
003980        IF FDP-LAUNCHED-BY-CD = 'B'                               FTS0279
003990
004000           IF FDP-BJLG-KEY <= ZEROS
004010              MOVE 'BT'            TO RTN-ERR-PREFIX
004020              MOVE 1004            TO RTN-ERROR-CODE
004030              MOVE 'CACDBALB'      TO RTN-PROG-NAME
004040              MOVE 'B300'          TO RTN-PARA-NAME               MLV0916
004050              CALL 'CUTSVARB' USING RMESSAGE,
004060                                    RTN-OUTPUT-FIELDS
004070           END-IF
004080
004090**         GET NEXT-RUN-DT FROM BATCH JOB LOG
004100           IF RTN-NO-ERROR
004110              INITIALIZE VBJB1-DATA
004120              MOVE FDP-BJLG-KEY       TO VBJB1-BJLG-KEY
004130
004140              CALL 'CS1VBJBD' USING RMESSAGE,
004150                                    VBJB1-DATA,
004160                                    BJRL1-DATA,                   FDM3514
004170                                    RTN-OUTPUT-FIELDS
004180           END-IF
004190
004200***     BATCH RULES FORM NO LONGER REQUIRES NEXT-RUN-DT           MLV0916
004210***     IF SPACES, ERROR OUT                                      MLV0916
004220           IF RTN-NO-ERROR                                        MLV0916
004230              IF VBJB1-NEXT-RUN-DT = SPACES                       MLV0916
004240                 MOVE 'BT'               TO RTN-ERR-PREFIX        MLV0916
004250                 MOVE 1217               TO RTN-ERROR-CODE        MLV0916
004260                 MOVE 'CACDBALB'         TO RTN-PROG-NAME         MLV0916
004270                 MOVE 'B300'             TO RTN-PARA-NAME         MLV0916
004280                 CALL 'CUTSVARB' USING RMESSAGE,                  MLV0916
004290                                       RTN-OUTPUT-FIELDS          MLV0916
004300              END-IF                                              MLV0916
004310           END-IF                                                 MLV0916
004320
004330           IF RTN-NO-ERROR
004340**            INSERT TO RPTREQUEST ONLY IF RUNNING IN BATCH
004350**            IF RUNNING FROM FORMS, RPTREQUESTID WILL BE IN
004360**            FDP-REF-KEY
004370              MOVE VBJB1-NEXT-RUN-DT  TO WS-RUN-DT
004380              INITIALIZE RPRQ1-DATA
004390
004400              MOVE  VRPD1-RPTREFID    TO    RPRQ1-RPTREFID
004410              MOVE  WS-RUN-DT         TO    RPRQ1-SCHEDDT
004420              MOVE  '3'               TO    RPRQ1-STATCD
004430              MOVE  ZEROES            TO    RPRQ1-AUDITID
004440              MOVE  ZEROES            TO    RPRQ1-RPTREQUESTID
004450
004460              CALL 'CI1RPRQD' USING RMESSAGE,
004470                                    RPRQ1-DATA,
004480                                    RTN-OUTPUT-FIELDS
004490           END-IF
004500
004510           IF RTN-NO-ERROR
004520              MOVE RPRQ1-RPTREQUESTID  TO  FDP-REF-KEY
004530           END-IF
004540
004550        ELSE
004560**         USE FDP-REF-KEY TO GET SCHED-DT FOR REPORT
004570           INITIALIZE RPRQ1-DATA
004580           MOVE FDP-REF-KEY        TO RPRQ1-RPTREQUESTID
004590
004600           CALL 'CS1RPRQD' USING RMESSAGE,
004610                           RPRQ1-DATA,
004620                           RTN-OUTPUT-FIELDS
004630
004640           IF RTN-NO-ERROR
004650              MOVE RPRQ1-SCHEDDT   TO WS-RUN-DT
004660           END-IF
004670        END-IF
004680
004690     END-IF.
004700
004710 B300-EXIT.
004720
004730*****************************************************************
004740** BUILD REPORT HEADER                                         **
004750*****************************************************************
004760
004770 B400-BUILD-REPORT-HEADER.
004780
004790     IF RTN-NO-ERROR
004800
004810        INITIALIZE PRDT1-DATA
004820*       HEADER
004830        MOVE ZEROES         TO  WS-SEQ-NUM
004840        ADD +1              TO  WS-SEQ-NUM
004850        MOVE WS-RUN-DT      TO  HEAD-DT
004860
004870        MOVE FDP-REF-KEY             TO   PRDT1-RPTREQUESTID
004880        MOVE WS-SEQ-NUM              TO   PRDT1-SEQNUM
004890        MOVE HEAD-REC                TO   PRDT1-CDL
004900        MOVE 'N'                     TO   PRDT1-CONTINUECD
004910        MOVE ZEROES                  TO   PRDT1-AUDITID
004920        MOVE 'HEAD'                  TO   PRDT1-RECORDTYPE
004930        MOVE SPACES                  TO   PRDT1-SORTDATA
004940
004950        CALL 'CI1PRDTD'      USING RMESSAGE,
004960                                   PRDT1-DATA,
004970                                   RTN-OUTPUT-FIELDS
004980
004990     END-IF.
005000
005010 B400-EXIT.
005020
005030*****************************************************************
005040** GET THE ACCT_KEY FROM LEDGER_ACCOUNTS                       **
005050*****************************************************************
005060
005070 B500-GET-ACCT-KEY.
005080
005090     INITIALIZE ACCT1-DATA.
005100     MOVE VJLD1-ACCT-NO(SUB0)     TO ACCT1-ACCT-NO.
005110     MOVE VJLD1-SUB-ACCT-NO(SUB0) TO ACCT1-SUB-ACCT-NO.
005120
005130     CALL 'CS4ACCTD' USING RMESSAGE,
005140                           ACCT1-DATA,
005150                           RTN-OUTPUT-FIELDS.
005160
005170 B500-EXIT.
005180
005190*****************************************************************
005200** GET BEGINNING AND ENDING LEDGER ACCOUNT BALANCES            **
005210*****************************************************************
005220
005230 B600-GET-ACCOUNT-BALANCES.
005240** GET BEGINNING DT MOVED OUTSIDE LOOP - ONLY NEED TO DO ONCE  ** FLM0001
005250     PERFORM C300-GET-MONTH-END-BALANCE
005260        THRU C300-EXIT
005270
005280     IF RTN-NO-ERROR
005290        PERFORM C400-GET-BEGINNING-BALANCE
005300           THRU C400-EXIT
005310     END-IF.
005320
005330     IF RTN-NO-ERROR
005340        PERFORM C500-GET-ENDING-BALANCE
005350           THRU C500-EXIT
005360     END-IF.
005370
005380 B600-EXIT.
005390     EXIT.
005400
005410*****************************************************************
005420** PRINT DETAIL LINE                                           **
005430*****************************************************************
005440
005450 B700-PRINT-DETAIL-LINE.
005460
005470     INITIALIZE PRDT1-DATA.
005480     MOVE VJLD1-MKEY-FD-NUM (SUB0) TO DETL-FUND.
005490     MOVE VJLD1-ACCT-NO (SUB0)     TO DETL-ACCT-NO.
005500     MOVE VJLD1-SUB-ACCT-NO(SUB0)  TO DETL-SUB-ACCT-NO.
005510     MOVE WS-BEG-BAL               TO DETL-BEG-BAL.
005520     MOVE WS-END-BAL               TO DETL-END-BAL.
005530     MOVE VJLD1-CONT-NO (SUB0)     TO DETL-SCHEME-NO.
005540     MOVE VJLD1-EFF-DT (SUB0)      TO DETL-EFF-DT.
005550     MOVE VJLD1-BOOK-DT (SUB0)     TO DETL-BOOK-DT.
005560     MOVE VJLD1-AMOUNT-DB (SUB0)   TO DETL-AMOUNT-DB.
005570     MOVE VJLD1-AMOUNT-CR (SUB0)   TO DETL-AMOUNT-CR.
005580
005590     ADD +1                        TO  WS-SEQ-NUM
005600
005610     MOVE FDP-REF-KEY              TO   PRDT1-RPTREQUESTID
005620     MOVE WS-SEQ-NUM               TO   PRDT1-SEQNUM
005630     MOVE DETL-REC                 TO   PRDT1-CDL
005640     MOVE 'N'                      TO   PRDT1-CONTINUECD
005650     MOVE ZEROES                   TO   PRDT1-AUDITID
005660     MOVE 'DETL'                   TO   PRDT1-RECORDTYPE
005670     MOVE SPACES                   TO   PRDT1-SORTDATA
005680
005690     CALL 'CI1PRDTD'   USING RMESSAGE,
005700                             PRDT1-DATA,
005710                             RTN-OUTPUT-FIELDS.
005720
005730
005740 B700-EXIT.
005750     EXIT.
005760
005770*****************************************************************
005780** SUBTRACT ONE MONTH FROM REPORT RUN DATE                     **
005790*****************************************************************
005800
005810 C100-SUBTRACT-A-MONTH.
005820
005830**   SUBTRACT ONE MONTH FROM THE RUN DATE
005840     INITIALIZE DATEPARMS.
005850     MOVE WS-RUN-DT              TO INDATE.
005860**   REQUEST-TYPE = 110 -> MONTH
005870     MOVE 110                    TO REQUEST-TYPE.
005880     MOVE -1                     TO MONTHS-DAY-CHG.
005890
005900     CALL 'CUTDTCAB' USING RMESSAGE,                              LSM5259
005910                           DATEPARMS,                             LSM5259
005920                           RTN-OUTPUT-FIELDS.                     LSM5259
005930
005940     IF RTN-NO-ERROR
005950        MOVE ODATE               TO WS-BALANCE-DT
005960     END-IF.
005970
005980 C100-EXIT.
005990     EXIT.
006000
006010*****************************************************************
006020** GET THE LAST DAY OF THE MONTH                               **
006030*****************************************************************
006040
006050 C200-GET-LAST-DAY-OF-MONTH.
006060
006070**   GET THE LAST DAY OF THE MONTH
006080     INITIALIZE DATEPARMS.
006090     MOVE WS-BALANCE-DT          TO INDATE.
006100**   REQUEST-TYPE = 200 -> LAST DAY OF MONTH
006110     MOVE 200                    TO REQUEST-TYPE.
006120
006130     CALL 'CUTDTCAB' USING RMESSAGE,                              LSM5259
006140                           DATEPARMS,                             LSM5259
006150                           RTN-OUTPUT-FIELDS.                     LSM5259
006160
006170     IF RTN-NO-ERROR
006180        MOVE ODATE               TO WS-BALANCE-DT
006190     END-IF.
006200
006210 C200-EXIT.
006220     EXIT.
006230
006240*****************************************************************
006250** GET THE ACCOUNT'S MONTH END BALANCE                         **
006260*****************************************************************
006270
006280 C300-GET-MONTH-END-BALANCE.
006290
006300**   GET THIS ACCOUNT'S MONTH END BALANCE
006310     INITIALIZE COAC1-DATA.
006320     MOVE WS-BALANCE-DT          TO COAC1-ACCT-BAL-DT.
006330     MOVE ACCT1-ACCT-KEY         TO COAC1-ACCT-KEY.
006340
006350     CALL 'CS3COACD' USING RMESSAGE,
006360                           COAC1-DATA,
006370                           RTN-OUTPUT-FIELDS.
006380
006390     IF RTN-NO-ERROR
006400        IF COAC1-ROWS-FOUND > ZERO
006410           MOVE COAC1-END-BAL-AMT  TO WS-BEG-BAL,
006420                                      WS-END-BAL
006430        ELSE
006440           MOVE ZERO               TO WS-BEG-BAL,
006450                                      WS-END-BAL
006460           MOVE 'TX'               TO RTN-ERR-PREFIX
006470           MOVE 1010               TO RTN-ERROR-CODE
006480           MOVE 'CACDBALB'         TO RTN-PROG-NAME
006490           MOVE 'C300'             TO RTN-PARA-NAME
006500           CALL 'CUTSVARB' USING RMESSAGE,
006510                                 RTN-OUTPUT-FIELDS
006520        END-IF
006530     END-IF.
006540
006550 C300-EXIT.
006560     EXIT.
006570
006580*****************************************************************
006590** SUM THE JOURNAL LOGS FOR THIS MONTH                         **
006600*****************************************************************
006610
006620 C400-GET-BEGINNING-BALANCE.
006630
006640**   SUM THE JOURNAL LOGS FROM THE FIRST OF THE MONTH TO
006650**   THE DAY PRIOR TO THE RUN DATE GIVING THE BEGINNING BALANCE.
006660     INITIALIZE JLOG4-DATA.
006670
006680**   SUBTRACT ONE DAY FROM THE RUN DATE
006690     INITIALIZE DATEPARMS.
006700     MOVE WS-RUN-DT              TO INDATE.
006710**   REQUEST-TYPE = 100 -> DAYS
006720     MOVE 100                    TO REQUEST-TYPE.
006730     MOVE -1                     TO MONTHS-DAY-CHG.
006740
006750     CALL 'CUTDTCAB' USING RMESSAGE,                              LSM5259
006760                           DATEPARMS,                             LSM5259
006770                           RTN-OUTPUT-FIELDS.                     LSM5259
006780
006790     IF RTN-NO-ERROR
006800        MOVE ODATE               TO WS-END-DT
006810     END-IF.
006820
006830        MOVE WS-BALANCE-DT        TO WS-BEGIN-DT.
006840
006850        PERFORM D100-CALL-CF3JLOGD
006860           THRU D100-EXIT.
006870
006880        ADD WS-BAL TO WS-BEG-BAL.
006890 C400-EXIT.
006900     EXIT.
006910
006920*****************************************************************
006930** SUM THE JOURNAL LOGS FOR THIS MONTH                         **
006940*****************************************************************
006950
006960 C500-GET-ENDING-BALANCE.
006970
006980**   SUM THE JOURNAL LOGS FROM THE FIRST OF THE MONTH TO
006990**   THE RUN DATE TO GET THE ENDING BALANCE.
007000     INITIALIZE JLOG4-DATA.
007010
007020
007030     MOVE WS-BALANCE-DT        TO WS-BEGIN-DT.
007040
007050     MOVE WS-RUN-DT            TO WS-END-DT.
007060
007070     PERFORM D100-CALL-CF3JLOGD
007080        THRU D100-EXIT.
007090
007100     ADD WS-BAL TO WS-END-BAL.
007110
007120 C500-EXIT.
007130     EXIT.
007140
007150*****************************************************************
007160** CALL CF3JLOGB TO GET JOURNAL LOGS BETWEEN BEG AND END DATES **
007170*****************************************************************
007180
007190 D100-CALL-CF3JLOGD.
007200
007210*    INITIALIZE WS-BAL.
007220     MOVE ZEROS TO WS-BAL.
007230     INITIALIZE JLOG4-DATA.
007240
007250     PERFORM UNTIL JLOG4-END-FLAG = 'Y'
007260        OR RTN-ERROR
007270
007280        MOVE ACCT1-ACCT-KEY       TO JLOG4-ACCT-KEY(1)
007290**      BAL-LVL = '01' -> LEDGER LEVEL
007300        MOVE '01'                 TO JLOG4-BAL-LVL
007310        MOVE WS-BEGIN-DT          TO JLOG4-BEGIN-DT
007320        MOVE WS-END-DT            TO JLOG4-END-DT
007330
007340        CALL 'CF3JLOGD' USING RMESSAGE,
007350                              JLOG4-DATA,
007360                              RTN-OUTPUT-FIELDS
007370
007380        PERFORM VARYING IJLOG FROM 1 BY 1 UNTIL
007390           IJLOG > JLOG4-ROWS-FOUND OR RTN-ERROR
007400
007410**         ADD THE DEBITS AND SUBTRACT THE CREDITS
007420           ADD JLOG4-TOT-DEBITS(IJLOG) TO WS-BAL
007430           SUBTRACT JLOG4-TOT-CREDITS(IJLOG) FROM WS-BAL
007440
007450        END-PERFORM
007460
007470     END-PERFORM.
007480
007490 D100-EXIT.
007500     EXIT.
007510
007520
007530
