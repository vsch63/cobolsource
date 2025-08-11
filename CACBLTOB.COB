000010*Property of FIS©. or its affiliates
000020*all rights reserved. FIS Confidential
000030 IDENTIFICATION DIVISION.
000040 PROGRAM-ID.           CACBLTOB.
000050 AUTHOR.               DANIEL WILLIAMS
000060 DATE-WRITTEN.         11-19-1997.
000070****************************************************************
000080****************************************************************
000090*   $Modtime::   10 Oct 2018 10:15:16  $
000100*   $Log::   P:/pvcsproj/DONOTUSE.prj/source/CACBLTOB.COV  $
000110*
000120*   Rev 1.0.1.11   10 Oct 2018 11:45:22   alex
000130*FDM4167
000140*fis copyright
000150*
000160*   Rev 1.0.1.10   19 Apr 2011 17:27:24   tom
000170*LSM5259
000180*Replace call to CUTDTCAL
000190*
000200*   Rev 1.0.1.9   28 Apr 2006 14:19:34   katrina
000210*LPM0066
000220*Contract Number Size Change
000230*
000240*   Rev 1.0.1.8   23 Sep 2005 11:41:18   robin
000250*FDM3514
000260*Module Impact Analysis Tool - WS-REVISION awk script fix.
000270*
000280*   Rev 1.0.1.7   19 Sep 2005 18:58:14   robin
000290*FDM3514
000300*Module Impact Analysis Tool
000310*
000320*   Rev 1.0.1.6   22 Nov 2004 18:27:26   gabriela
000330*FLM0001
000340*scheme level currency
000350*
000360*   Rev 1.0.1.5   20 Dec 2002 10:56:00   rieko
000370*FDM3175
000380*COBOL modifications to support the Oracle 8 pre-complier.
000390*
000400*   Rev 1.0.1.4   09 Jun 2000 15:38:46   barry
000410*FDM2160
000420*fix erf# in wrong column (72), should be 73
000430*
000440*   Rev 1.0.1.3   09 Jun 2000 14:48:54   barry
000450*FDM2160
000460*Expand CASE-KEY
000470*Also, Display Compiled date/time
000480*
000490*   Rev 1.0.1.2   Mar 24 2000 17:42:34   miker
000500*FDM1959
000510*Expanded Keys project
000520*
000530*   Rev 1.0.1.1   13 Jul 1999 02:13:48   michelle
000540*AFY2246 Initialize RPRQ1-DATA
*******
*******   Rev 1.0.1.0   15 Dec 1997 17:07:34   jihee
*******Initial Check In to FDP Branch
*******
*******
*******   Rev 1.0   15 Dec 1997 16:57:20   jihee
*******Initial Check In
*******
*******   Rev 1.0.1.0   15 Dec 1997 14:57:04   jihee
*******Initial Check In to FDP Branch
*******
*******   Rev 1.0   15 Dec 1997 14:29:20   jihee
*******Initial Check In
000680****************************************************************
000690****************************************************************
000700*
000710*
000720****************************************************************
000730*
000740* ADHOC ACCOUNT BALANCING.  THREE LEVELS OF BALANCING ARE
000750* SUPPORTED IN THIS PROGRAM COMPANY, PRODUCT, AND SCHEME.
000760****************************************************************
000770* THIS PROGRAM CALCULATES BEGINNING AND ENDING ACCOUNT BALANCES.
000780* THREE LEVELS OF BALANCING ARE AVAILABLE COMPANY, PRODUCT
000790* AND CASE.  THE PROGRAM WILL UPDATE/INSERT RECORDS INTO COMPANY  FLM0001
000800* ACCOUNTS TABLE.  IT ALSO UPDATES LAST ACCOUNT BALANCE DATE ON   FLM0001
000810* SYSTEM_DATA.  THIS PROGRAM WILL BE RUN ON A MONTHLY BASIS IN
000820* BATCH.  THIS IS A STEP PROGRAM DEFINED BY BARL-KEY.             FLM0001
000830*
000840****************************************************************
000850*    THE FOLLOWING ERROR CODES MAY BE RETURNED IN RTN-ERROR-CODE
000860*    FROM THIS PROGRAM...
000870*
000880*
000890****************************************************************
000900*                     CHANGES LOG
000910****************************************************************
000920*
000930*   DATE     PRG   PROJ#      DESCRIPTION
000940*  --------  --- ---------  --------------------------------------
000950* 11-19-1997 DEW AFA0314   ORIGINAL REQUEST ERF #
000960*07-JUL-1999 ML  AFY2246   INITIALIZE REPORT REQUEST
000970* 24-MAR-00  MR  FDM1959   Expanded Keys project
000980* 31-MAY-00  BST FDM2160   Expand CASE-KEY from PIC S9(05) to (15)
000990*                          Also, Display Compiled date/time
001000* 09-JUN-00  BST FDM2160   fix erf# in wrong column (72)
001010* 17-NOV-04  GVC FLM0001   ADD EDIT FOR NEW SCHEME CURRENCY CD
001020*
001030******************************************************************
001040 ENVIRONMENT DIVISION.
001050 CONFIGURATION SECTION.
001060*
001070 DATA DIVISION.
001080 WORKING-STORAGE SECTION.
001090
001100 01  WS-PROG-NAME                         PIC X(8)
001110                                          VALUE 'CACBLTOB'.
001120
001130 01 WS-COMPILED                           PIC X(20).              FDM2160
001140
001150 01  WS-SUBSCRIPTS.
001160     05  IJLOG                            PIC S9(4) COMP.
001170     05  ICOAC                            PIC S9(4) COMP.
001180
001190 01  WS-CODES.
001200     05  WS-WARNING-FLAG                  PIC 9(01) VALUE ZERO.
001210         88 WARNING-NOT-ISSUED            VALUE 0.
001220
001230 01  WS-HOLD-VARIABLES.
001240     05  WS-HOLD-ACCT-KEY                 PIC S9(15) COMP-3.      FDM1959
001250     05  WS-HOLD-CO-NUM                   PIC S9(05) COMP-3.
001260     05  WS-HOLD-PROD-NUM                 PIC S9(05) COMP-3.
001270     05  WS-HOLD-CASE-KEY                 PIC S9(15) COMP-3.      FDM2160
001280
001290 01  WS-IN-VARIABLES.
001300     05  WS-IN-CO-NUM                     PIC S9(05) COMP-3.
001310     05  WS-IN-PROD-NUM                   PIC S9(05) COMP-3.
001320     05  WS-IN-CASE-KEY                   PIC S9(15) COMP-3.      FDM2160
001330     05  WS-BALANCE-LEVEL                 PIC X(02) VALUE SPACES.
001340         88 NOT-APPLICABLE                VALUE '00'.
001350         88 LEDGER-LEVEL                  VALUE '01'.
001360         88 COMPANY-LEVEL                 VALUE '02'.
001370         88 PRODUCT-LEVEL                 VALUE '03'.
001380         88 CASE-LEVEL                    VALUE '04'.
001390     05  WS-BYPASS-FLAG                   PIC 9(01) VALUE ZERO.
001400         88 BYPASS-FALSE                  VALUE 0.
001410         88 BYPASS-TRUE                   VALUE 1.
001420
001430 01  WS-HOLD-VARIABLES2.
001440*    THESE ARE FOR BATCH REPORTING   ACCT-NO TRUNCATED
001450     05  WS-HOLD-ACCT-NO                  PIC X(20).
001460     05  WS-HOLD-SUB-ACCT-NO              PIC X(12).
001470
001480 01  WS-ACCUMULATORS.
001490     05  WS-TOTAL-DEBITS                  PIC S9(13)V99 COMP-3.
001500     05  WS-TOTAL-CREDITS                 PIC S9(13)V99 COMP-3.
001510     05  WS-DB-CR-TOTAL                   PIC S9(13)V99 COMP-3.
001520
001530 01  WS-MISC-VARIABLES.
001540     05  WS-BEG-ACCT-BAL-DT               PIC X(10).
001550     05  WS-END-ACCT-BAL-DT.
001560         10  WS-ISO-YYYY2                 PIC 9999.
001570         10  FILLER                       PIC X.
001580         10  WS-ISO-MM2                   PIC 99.
001590         10  FILLER                       PIC X.
001600         10  WS-ISO-DD2                   PIC 99.
001610     05  WS-YRS-DIFF                      PIC S9(4)V9(5) COMP-3.
001620
001630     05  WS-RECTYPE                       PIC X(04).
001640     05  WS-RPTREQUEST                    PIC 9(01) VALUE 0.
001650*    FOR REPORT
001660     05  WS-SEQ-NUM                       PIC 9(08) VALUE 0.
001670*    FOR REF CODE LOOKUP
001680     05  WS-REF-NUM                       PIC ZZZZ9.
001690
001700 01  WS-ERROR-MESSAGES.
001710     05  WS-ERR-MSG1.
001720         10  FILLER                       PIC X(20)
001730             VALUE 'LEDGER ACCOUNT KEY: '.
001740         10  WS-ERR-ACCT-KEY              PIC S9(15).             FDM1959
001750     05  WS-ERR-MSG2.
001760         10  FILLER                       PIC X(20)
001770             VALUE 'LEDGER ACCOUNT NO: '.
001780         10  WS-ERR-ACCT-NO               PIC X(20).
001790     05  WS-ERR-MSG3.
001800         10  FILLER                       PIC X(20)
001810             VALUE 'SUB ACCOUNT NO: '.
001820         10  WS-ERR-SUB-ACCT-NO           PIC X(12).
001830     05  WS-ERR-MSG4.                                             FLM0001
001840         10  FILLER                       PIC X(15)               FLM0001
001850             VALUE 'BEG BAL DATE: '.                              FLM0001
001860         10  WS-ERR-BEG-BAL-DT            PIC X(10).              FLM0001
001870
001880     05  WS-ERR-MSG5.                                             FLM0001
001890         10  FILLER                       PIC X(15)               FLM0001
001900             VALUE 'END BAL DATE: '.                              FLM0001
001910         10  WS-ERR-END-BAL-DT            PIC X(10).              FLM0001
001920
001930     05  WS-ERR-MSG6.                                             FLM0001
001940         10  FILLER                       PIC X(15)               FLM0001
001950             VALUE 'TRANSACTION : '.                              FLM0001
001960         10  WS-ERR-TR-CD                 PIC X(04).              FLM0001
001970
001980* WS FOR THE REPORT
001990 01  CHR34-NUM PIC 9(4) BINARY VALUE 34.
002000 01  CHR34-NUM-REDEF REDEFINES CHR34-NUM.
002010     05  FILLER PIC X.
002020     05  CHR34 PIC X.
002030
002040**   PUTS IN A DOUBLE QUOTE
002050 01  HEAD-REC.
002060     05  FILLER1                          PIC X(01).
002070     05  HEAD-LAST-ACCT-BAL-DT            PIC X(10).
002080     05  FILLER                           PIC X(03) VALUE '","'.
002090     05  HEAD-NEW-ACCT-BAL-DT             PIC X(10).
002100     05  FILLER                           PIC X(03) VALUE '","'.
002110     05  HEAD-CYC-DT                      PIC X(10).
002120     05  FILLER2                          PIC X(01).
002130
002140 01  DETL-REC.
002150     05  FILLER3                          PIC X(01).
002160     05  DETL-ACCT-NO                     PIC X(12).
002170     05  FILLER                           PIC X(03) VALUE '","'.
002180     05  DETL-SUB-ACCT-NO                 PIC X(12).
002190     05  FILLER                           PIC X(03) VALUE '","'.
002200     05  DETL-ACCT-NM                     PIC X(30).
002210     05  FILLER                           PIC X(03) VALUE '","'.
002220* TRUNCATED FROM 80 TO 50
002230     05  DETL-COMPANY-DESCRIPT            PIC X(50).
002240     05  FILLER                           PIC X(03) VALUE '","'.
002250* TRUNCATED FROM 80 TO 50
002260     05  DETL-PRODUCT-DESCRIPT            PIC X(50).
002270     05  FILLER                           PIC X(03) VALUE '","'.
002280     05  DETL-BAL-LVL-CD                  PIC X(02).
002290     05  FILLER                           PIC X(03) VALUE '","'.
002300     05  DETL-CONT-NO                     PIC X(15).              LPM0066
002310     05  FILLER                           PIC X(03) VALUE '","'.
002320     05  DETL-CAT-NO                      PIC X(10).
002330     05  FILLER                           PIC X(03) VALUE '","'.
002340* TRUNCATED FROM 80 TO 50
002350     05  DETL-PLAN-NM                     PIC X(50).
002360     05  FILLER4                          PIC X(01).
002370
002380 01  DETL-REC2.
002390     05  FILLER                           PIC X(01) VALUE ",".
002400     05  FILLER5                          PIC X(01).
002410     05  DETL2-BEG-BAL-AMT                PIC -ZZZZZZZZZZZZ9.99.          8200
002420     05  FILLER                           PIC X(03) VALUE '","'.
002430     05  DETL2-TOT-CREDITS                PIC -ZZZZZZZZZZZZ9.99.          8400
002440     05  FILLER                           PIC X(03) VALUE '","'.
002450     05  DETL2-TOT-DEBITS                 PIC -ZZZZZZZZZZZZ9.99.
002460     05  FILLER                           PIC X(03) VALUE '","'.
002470     05  DETL2-END-BAL-AMT                PIC -ZZZZZZZZZZZZ9.99.
002480     05  FILLER                           PIC X(03) VALUE '","'.
002490     05  DETL2-TR-REF-NO                  PIC X(12).
002500     05  FILLER6                          PIC X(01).
002510
002520 01  SORT-REC.
002530     05  SORT-BAL-LVL-CD                  PIC X(02).
002540* TRUNCATED FROM 80 TO 50
002550     05  SORT-COMPANY-DESCRIPT            PIC X(50).
002560* TRUNCATED FROM 80 TO 50
002570     05  SORT-PRODUCT-DESCRIPT            PIC X(50).
002580     05  SORT-CONT-NO                     PIC X(15).              LPM0066
002590     05  SORT-CAT-NO                      PIC X(10).
002600     05  SORT-ACCT-NO                     PIC X(12).
002610     05  SORT-SUB-ACCT-NO                 PIC X(12).
002620
002630*    LEDGER ACCOUNT COPYBOOK
002640     COPY CL1ACCTD.
002650*    COMPANY ACCOUNTS COPYBOOK
002660     COPY CL1COACD.
002670*    COMPANY ACCOUNTS COPYBOOK BLOCK FETCH
002680     COPY CL2COACD.
002690**   JOURNAL_LOGS                                                 FLM0001
002700     COPY CL1JLOGD.                                               FLM0001
002710*    JOURNAL LOG COPYBOOK BLOCK FETCH
002720     COPY CL4JLOGD.
002730*    JOURNAL LOG COPYBOOK SUMMARY
002740     COPY CL3JLOGD.
002750*    JOURNAL LOG COPYBOOK SUMMARY
002760     COPY CL5JLOGD.
002770*    DATE CALCULATION COPYBOOK
002780     COPY CWSDTCAL.
002790*    SCHED_ACCT_BAL COPYBOOK
002800     COPY CL1SCABD.
002810**   COPYBOOK FOR RPTDEF,RPTVERS,RPTREFLNK JOIN
002820     COPY CL1VRPDD.
002830**   COPYBOOK FOR RPTREQUEST
002840     COPY CL1RPRQD.
002850*    PRINTDATA COPYBOOK
002860     COPY CL1PRDTD.
002870*    REF_CODES COPYBOOK
002880     COPY CL1RFCDD.
002890*    CASE_DATA COPYBOOK
002900     COPY CL3CASDD.
002910*    BAL_ACCT_RULES
002920     COPY CL1BARLD.
002930
002940**  REVISION AREA **                                              FDM3514
002950 01  WS-REVISION           PIC X(35)  VALUE                       FDM3514
002960     ' $Revision::   1.0.1.11       $'.                           
002970 LINKAGE SECTION.
002980*    RMESSAGE AREA
002990     COPY CLKFDPNC.
003000*    CASE  DATA COPYBOOK
003010     COPY CL1CASDD.
003020*    STANDARD ERROR BLOCK
003030     COPY CLKERRBC.
003040
003050*********************************************************
003060**   P R O C E D U R E    D I V I S I O N              **
003070*********************************************************
003080
003090******************************************************************
003100**                MAIN ROUTINE                                  **
003110******************************************************************
003120 PROCEDURE DIVISION USING RMESSAGE,
003130                          CASD1-DATA,
003140                          RTN-OUTPUT-FIELDS.
003150
003160
003170     IF FDP-DEBUG-RUNSTAT > '00'                                  FDM3514
003180        ADD +1  TO RTN-DIMT-CNT                                   FDM3514
003190        MOVE WHEN-COMPILED TO                                     FDM3514
003200             RTN-DIMT-COMPILE-DATE (RTN-DIMT-CNT)                 FDM3514
003210        MOVE WS-REVISION (16:10) TO                               FDM3514
003220             RTN-DIMT-REVISION (RTN-DIMT-CNT)                     FDM3514
003230        MOVE 'CACBLTOB' TO                                        FDM3514
003240           RTN-DIMT-PROGRAM-NAME (RTN-DIMT-CNT)                   FDM3514
003250        CALL 'CUTDIMTD' USING RMESSAGE,                           FDM3514
003260                              RTN-OUTPUT-FIELDS                   FDM3514
003270     END-IF.                                                      FDM3514
003280
003290     PERFORM A000-INITIALIZE
003300        THRU A000-EXIT.
003310
003320     PERFORM B000-MAIN
003330        THRU B000-EXIT.
003340
003350*    IF A ERROR OCCURRED CALL THE CD2COACD DAL
003360*    TO ERASE ALL OF THE COAC'S THAT WERE ROLLED FORWARD
003370*    BY THE A200- PARAGRAPH
003380*    SET ALL PROCESSED SCHED ACTIVITY RECORDS TO UNPROCESSED
003390     IF RTN-ERROR
003400        PERFORM Z300-ROLLBACK-COMPANY-ACCOUNTS THRU Z300-EXIT
003410        CALL 'CCOMMITD'
003420     END-IF.
003430     IF RTN-ERROR   AND  WS-RPTREQUEST = 1
003440        PERFORM Z320-DELETE-PRINTDATA          THRU Z320-EXIT
003450        PERFORM Z340-DELETE-REPORT-REQUEST     THRU Z340-EXIT
003460        CALL 'CCOMMITD'
003470     END-IF.
003480
003490     GOBACK.
003500
003510 A000-INITIALIZE.
003520*********************************************************
003530**                                                     **
003540*********************************************************
003550
003560     INITIALIZE WS-CODES.
003570     INITIALIZE WS-IN-VARIABLES.
003580     INITIALIZE WS-HOLD-VARIABLES.
003590     MOVE  ZERO           TO  WS-ERR-ACCT-KEY.
003600     MOVE  SPACES         TO  WS-ERR-ACCT-NO.
003610     MOVE  SPACES         TO  WS-ERR-SUB-ACCT-NO.
003620
003630** EDIT INCOMING PARAMETERS
003640     PERFORM A010-EDIT-LINKAGE                                    FLM0001
003650        THRU A010-EXIT.                                           FLM0001
003660
003670** GET THE BARL RULE
003680     IF RTN-NO-ERROR
003690        PERFORM A050-GET-BARL-RULE  THRU  A050-EXIT
003700     END-IF.
003710
003720** CALCULATE MONTH END DATE
003730     IF RTN-NO-ERROR
003740        PERFORM A100-CALC-PRIOR-MONTHEND-DATE  THRU  A100-EXIT
003750     END-IF.
003760
003770** verify dates
003780     IF RTN-NO-ERROR
003790        PERFORM A110-VERIFY-DATES  THRU  A110-EXIT
003800     END-IF.
003810
003820     IF RTN-NO-ERROR AND NOT CASE-LEVEL                           FLM0001
003830*       CHECK FOR MULTI-CURRENCY IF NOT CASE-LEVEL BALANCING      FLM0001
003840        PERFORM A120-CHECK-ISO-CURR                               FLM0001
003850           THRU A120-EXIT                                         FLM0001
003860     END-IF.                                                      FLM0001
003870
003880** SELECT COMPANY ACCOUNTS FROM LAST BALANCE RUN
003890     IF RTN-NO-ERROR
003900        PERFORM A200-FETCH-COMPANY-ACCOUNTS  THRU  A200-EXIT
003910     END-IF.
003920
003930     IF RTN-NO-ERROR
003940        PERFORM A300-SELECT-RPTREFID  THRU  A300-EXIT
003950     END-IF.
003960
003970     IF RTN-NO-ERROR
003980        PERFORM A400-INS-RPTREQUEST   THRU  A400-EXIT
003990     END-IF.
004000
004010     IF RTN-NO-ERROR
004020        PERFORM A500-CREATE-REPORT-HEADING THRU A500-EXIT
004030     END-IF.
004040
004050 A000-EXIT.
004060     EXIT.
004070**********************************************************
004080*  EDIT LINKAGE                                                   FLM0001
004090**********************************************************
004100
004110  A010-EDIT-LINKAGE.                                              FLM0001
004120
004130     IF FDP-USER = SPACES
004140        MOVE 'UT'            TO RTN-ERR-PREFIX
004150        MOVE 39              TO RTN-ERROR-CODE
004160        MOVE 'CACBLTOB'      TO RTN-PROG-NAME
004170        MOVE 'A010-'         TO RTN-PARA-NAME                     FLM0001
004180        CALL 'CUTSVARB' USING RMESSAGE,
004190                              RTN-OUTPUT-FIELDS
004200     END-IF.
004210
004220     IF FDP-TR-REF-NO = SPACES AND RTN-NO-ERROR                   FLM0001
004230        MOVE 'UT'            TO RTN-ERR-PREFIX
004240        MOVE 34              TO RTN-ERROR-CODE
004250        MOVE 'CACBLTOB'      TO RTN-PROG-NAME
004260        MOVE 'A010-'         TO RTN-PARA-NAME                     FLM0001
004270        CALL 'CUTSVARB' USING RMESSAGE,
004280                              RTN-OUTPUT-FIELDS
004290     END-IF.
004300
004310     IF FDP-CYC-DT    = SPACES AND RTN-NO-ERROR                   FLM0001
004320        MOVE 'UT'            TO RTN-ERR-PREFIX
004330        MOVE 36              TO RTN-ERROR-CODE
004340        MOVE 'CACBLTOB'      TO RTN-PROG-NAME
004350        MOVE 'A010-'         TO RTN-PARA-NAME                     FLM0001
004360        CALL 'CUTSVARB' USING RMESSAGE,
004370                              RTN-OUTPUT-FIELDS
004380     END-IF.
004390
004400     IF FDP-REF-KEY  <= ZERO AND RTN-NO-ERROR                     FLM0001
004410        MOVE 'UT'            TO RTN-ERR-PREFIX
004420        MOVE 32              TO RTN-ERROR-CODE
004430        MOVE 'CACBLTOB'      TO RTN-PROG-NAME
004440        MOVE 'A010-'         TO RTN-PARA-NAME                     FLM0001
004450        CALL 'CUTSVARB' USING RMESSAGE,
004460                              RTN-OUTPUT-FIELDS
004470     END-IF.
004480
004490  A010-EXIT.                                                      FLM0001
004500   EXIT.                                                          FLM0001
004510
004520
004530 A050-GET-BARL-RULE.
004540*********************************************************
004550*
004560*********************************************************
004570     INITIALIZE BARL1-DATA.
004580     MOVE FDP-REF-KEY TO BARL1-BARL-KEY.
004590     CALL 'CS1BARLD' USING RMESSAGE,
004600                           BARL1-DATA,
004610                           RTN-OUTPUT-FIELDS.
004620
004630* VALIDATE THE NEXT BALANCE DATE FROM BARL
004640     IF RTN-NO-ERROR
004650        INITIALIZE DATEPARMS
004660        MOVE 200 TO REQUEST-TYPE
004670        MOVE BARL1-NXT-BAL-DT TO INDATE
004680        CALL 'CUTDTCAB' USING RMESSAGE,                           LSM5259
004690                              DATEPARMS,                          LSM5259
004700                              RTN-OUTPUT-FIELDS                   LSM5259
004710        IF LK-ERROR-CD NOT = ZERO
004720           MOVE 'UT'        TO RTN-ERR-PREFIX
004730           MOVE 1100        TO RTN-ERROR-CODE
004740           MOVE 'CACBLTOB'  TO RTN-PROG-NAME
004750           MOVE 'A050-GET-' TO RTN-PARA-NAME
004760           CALL 'CUTSVARB' USING RMESSAGE,
004770                                RTN-OUTPUT-FIELDS
004780           DISPLAY 'ERROR IN DATE CALCULATION PROGRAM'
004790           DISPLAY 'CALLED FROM CACBLTOB'
004800        END-IF
004810        IF BARL1-NXT-BAL-DT NOT = ODATE
004820           MOVE 'TX'        TO RTN-ERR-PREFIX
004830           MOVE 1011        TO RTN-ERROR-CODE
004840           MOVE 'CACBLTOB'  TO RTN-PROG-NAME
004850           MOVE 'A050-GET-' TO RTN-PARA-NAME
004860           CALL 'CUTSVARB' USING RMESSAGE,
004870                                RTN-OUTPUT-FIELDS
004880           DISPLAY 'NXT DT MUST BE MONTH END DATE'
004890        END-IF
004900     END-IF.
004910
004920     IF BARL1-CASE-KEY > 0 AND RTN-NO-ERROR                                               00031900
004930        MOVE BARL1-CASE-KEY TO WS-HOLD-CASE-KEY
004940        MOVE BARL1-CASE-KEY TO WS-IN-CASE-KEY
004950        SET CASE-LEVEL TO TRUE
004960     END-IF.
004970
004980     IF BARL1-MKEY-CO-NUM > 0 AND RTN-NO-ERROR                                               00031900
004990        MOVE BARL1-MKEY-CO-NUM TO WS-HOLD-CO-NUM
005000        MOVE BARL1-MKEY-CO-NUM TO WS-IN-CO-NUM
005010        SET COMPANY-LEVEL TO TRUE
005020     END-IF.
005030
005040     IF BARL1-MKEY-PROD-NUM > 0 AND RTN-NO-ERROR                                               00031900
005050        MOVE BARL1-MKEY-PROD-NUM TO WS-HOLD-PROD-NUM
005060        MOVE BARL1-MKEY-PROD-NUM TO WS-IN-PROD-NUM
005070        SET PRODUCT-LEVEL TO TRUE
005080     END-IF.
005090
005100 A050-EXIT.
005110     EXIT.
005120
005130 A100-CALC-PRIOR-MONTHEND-DATE.
005140*********************************************************
005150* DATE RANGE = END OF LAST MONTH THRU NXT-BAL-DT
005160*********************************************************
005170*    CALCULATE LAST MONTH END DATE, SAVE IN WORKING STORAGE
005180     IF RTN-NO-ERROR
005190        INITIALIZE DATEPARMS
005200        MOVE 110 TO REQUEST-TYPE
005210        MOVE BARL1-NXT-BAL-DT TO INDATE
005220        MOVE BARL1-NXT-BAL-DT TO WS-END-ACCT-BAL-DT
005230        MOVE -1 TO MONTHS-DAY-CHG
005240
005250        CALL 'CUTDTCAB' USING RMESSAGE,                           LSM5259
005260                              DATEPARMS,                          LSM5259
005270                              RTN-OUTPUT-FIELDS                   LSM5259
005280
005290        IF LK-ERROR-CD NOT = ZERO
005300           MOVE 'UT'        TO RTN-ERR-PREFIX
005310           MOVE 1100        TO RTN-ERROR-CODE
005320           MOVE 'CACBLTOB'  TO RTN-PROG-NAME
005330           MOVE 'A100-CALC' TO RTN-PARA-NAME
005340           CALL 'CUTSVARB' USING RMESSAGE,
005350                                RTN-OUTPUT-FIELDS
005360           DISPLAY 'ERROR IN DATE CALCULATION PROGRAM'
005370           DISPLAY 'CALLED FROM CACBLTOB'
005380        ELSE
005390           MOVE ODATE TO WS-BEG-ACCT-BAL-DT
005400        END-IF
005410     END-IF.
005420
005430 A100-EXIT.
005440     EXIT.
005450
005460 A110-VERIFY-DATES.
005470*********************************************************
005480* VERIFY DATES
005490*********************************************************
005500*    ISSUE A WARNING IF CURRENT DATE < MONTH END DATE
005510     IF FDP-CYC-DT  <    WS-END-ACCT-BAL-DT
005520        MOVE 'TX'          TO RTN-ERR-PREFIX
005530        MOVE 1007          TO RTN-ERROR-CODE
005540        MOVE 'CACBLTOB'    TO RTN-PROG-NAME
005550        MOVE 'A110-VERIFY' TO RTN-PARA-NAME
005560        CALL 'CUTSVARB' USING RMESSAGE,
005570                              RTN-OUTPUT-FIELDS
005580     END-IF.
005590
005600 A110-EXIT.
005610     EXIT.
005620
005630**********************************************************        FLM0001
005640*  CHECK FOR MULTIPLE CURRENCY CODES ON JLOGS TO BALANCE          FLM0001
005650**********************************************************        FLM0001
005660
005670 A120-CHECK-ISO-CURR.                                             FLM0001
005680
005690     INITIALIZE JLOG1-DATA.                                       FLM0001
005700     MOVE WS-BEG-ACCT-BAL-DT     TO  JLOG1-BEGIN-DT.              FLM0001
005710     MOVE WS-END-ACCT-BAL-DT     TO  JLOG1-END-DT.                FLM0001
005720     CALL 'CS6JLOGD' USING RMESSAGE,                              FLM0001
005730                           JLOG1-DATA,                            FLM0001
005740                           RTN-OUTPUT-FIELDS.                     FLM0001
005750
005760     IF RTN-NO-ERROR                                              FLM0001
005770        IF JLOG1-ROWS-FOUND > 1                                   FLM0001
005780           MOVE 'UT'    TO RTN-ERR-PREFIX                         FLM0001
005790           MOVE 317     TO RTN-ERROR-CODE                         FLM0001
005800           MOVE 'CACBLTOB' TO RTN-PROG-NAME                       FLM0001
005810           MOVE 'A120'  TO RTN-PARA-NAME                          FLM0001
005820           MOVE WS-BEG-ACCT-BAL-DT   TO WS-ERR-BEG-BAL-DT         FLM0001
005830           MOVE WS-END-ACCT-BAL-DT   TO WS-ERR-END-BAL-DT         FLM0001
005840           MOVE WS-ERR-MSG4          TO RTN-VAR-TEXT   (1)        FLM0001                             000167300
005850           MOVE 1                    TO RTN-VAR-SEQ-NO (1)        FLM0001                           000167400
005860           MOVE WS-ERR-MSG5          TO RTN-VAR-TEXT   (2)        FLM0001                             000167500
005870           MOVE 2                    TO RTN-VAR-SEQ-NO (2)        FLM0001                           000167600
005880           MOVE 2                    TO RTN-VAR-DETL-CNT          FLM0001
005890           CALL 'CUTSVARB' USING RMESSAGE,                        FLM0001
005900                                 RTN-OUTPUT-FIELDS                FLM0001
005910        END-IF                                                    FLM0001
005920     END-IF.                                                      FLM0001
005930
005940 A120-EXIT.                                                       FLM0001
005950     EXIT.
005960**********************************************************
005970
005980 A200-FETCH-COMPANY-ACCOUNTS.
005990*********************************************************
006000* SELECT ALL COMPANY ACCOUNTS FROM LAST MONTH
006010*********************************************************
006020     INITIALIZE COAC2-DATA.
006030     PERFORM UNTIL COAC2-END-FLAG = 'Y'  OR  RTN-ERROR
006040       MOVE WS-BEG-ACCT-BAL-DT TO COAC2-ACCT-BAL-DT
006050       EVALUATE TRUE
006060         WHEN COMPANY-LEVEL
006070              MOVE WS-IN-CO-NUM   TO  COAC2-MKEY-CO-NUM (1)
006080         WHEN PRODUCT-LEVEL
006090              MOVE WS-IN-PROD-NUM TO  COAC2-MKEY-PROD-NUM (1)
006100         WHEN CASE-LEVEL
006110              MOVE WS-IN-CASE-KEY TO  COAC2-CASE-KEY (1)
006120       END-EVALUATE
006130*
006140       CALL 'CF2COACD' USING RMESSAGE,
006150                             COAC2-DATA,
006160                             RTN-OUTPUT-FIELDS
006170
006180       IF RTN-NO-ERROR
006190          IF COAC2-ROWS-FOUND > 0
006200             PERFORM A210-PROCESS-COMPANY-ACCTS THRU A210-EXIT
006210          END-IF
006220       END-IF
006230
006240     END-PERFORM.
006250
006260 A200-EXIT.
006270     EXIT.
006280
006290 A210-PROCESS-COMPANY-ACCTS.
006300*********************************************************
006310* LOOP THRU ALL COMPANY ACCOUNTS FROM LAST MONTH
006320* INSERT NEW ONES FOR THIS RUN
006330*********************************************************
006340     PERFORM VARYING ICOAC FROM 1 BY 1
006350             UNTIL ICOAC > COAC2-ROWS-FOUND OR
006360                           RTN-ERROR
006370
006380        INITIALIZE COAC1-DATA
006390        MOVE COAC2-ACCT-KEY (ICOAC)      TO  COAC1-ACCT-KEY
006400        MOVE COAC2-END-BAL-AMT (ICOAC)   TO  COAC1-BEG-BAL-AMT
006410        MOVE WS-END-ACCT-BAL-DT          TO  COAC1-EFF-DT
006420        MOVE COAC2-END-BAL-AMT (ICOAC)   TO  COAC1-END-BAL-AMT
006430        MOVE COAC2-CASE-KEY (ICOAC)      TO  COAC1-CASE-KEY
006440        MOVE COAC2-MKEY-CO-NUM (ICOAC)   TO  COAC1-MKEY-CO-NUM
006450        MOVE COAC2-MKEY-PROD-NUM (ICOAC) TO  COAC1-MKEY-PROD-NUM
006460        MOVE COAC2-PROD-TYP-CD (ICOAC)   TO  COAC1-PROD-TYP-CD
006470        MOVE FDP-TR-REF-NO               TO  COAC1-TR-REF-NO
006480
006490        CALL 'CI3COACD' USING RMESSAGE,
006500                              COAC1-DATA,
006510                              RTN-OUTPUT-FIELDS
006520
006530     END-PERFORM.
006540
006550 A210-EXIT.
006560     EXIT.
006570
006580 A300-SELECT-RPTREFID.
006590*********************************************************
006600*
006610*********************************************************
006620
006630**   SELECT RPTREFID  FROM  RPTDEF,RPTVERS,RPTREFLNK JOIN
006640
006650     MOVE '9001' TO VRPD1-TRANID.
006660
006670     CALL 'CS1VRPDD' USING RMESSAGE,
006680                           VRPD1-DATA,
006690                           RTN-OUTPUT-FIELDS.
006700
006710     IF VRPD1-ROWS-FOUND  =  ZERO
006720        MOVE 'BT'              TO RTN-ERR-PREFIX
006730        MOVE 1216              TO RTN-ERROR-CODE
006740        MOVE 'CACBLTOB'        TO RTN-PROG-NAME
006750        MOVE 'A300'            TO RTN-PARA-NAME
006760        MOVE '9001'                  TO WS-ERR-TR-CD              FLM0001
006770        MOVE WS-ERR-MSG6             TO RTN-VAR-TEXT   (1)        FLM0001                             000167300
006780        MOVE 1                       TO RTN-VAR-SEQ-NO (1)        FLM0001                           000167400
006790        MOVE 1                       TO RTN-VAR-DETL-CNT          FLM0001
006800        CALL 'CUTSVARB' USING RMESSAGE,
006810                              RTN-OUTPUT-FIELDS
006820     END-IF.
006830
006840 A300-EXIT.
006850      EXIT.
006860
006870 A400-INS-RPTREQUEST.
006880*********************************************************
006890*
006900*********************************************************
006910     INITIALIZE RPRQ1-DATA                                        AFY2246
006920     MOVE  VRPD1-RPTREFID      TO     RPRQ1-RPTREFID.
006930     MOVE  FDP-CYC-DT          TO     RPRQ1-SCHEDDT.
006940* 3 = SET REPORT REQUEST TO PROCESSED STATE
006950     MOVE  '3'                 TO     RPRQ1-STATCD.
006960     MOVE  ZEROES              TO     RPRQ1-AUDITID.
006970     MOVE  ZEROES              TO     RPRQ1-RPTREQUESTID.
006980
006990**
007000**   INSERT INTO RPTREQUEST
007010**
007020     CALL 'CI1RPRQD' USING RMESSAGE,
007030                           RPRQ1-DATA,
007040                           RTN-OUTPUT-FIELDS.
007050
007060     IF RTN-NO-ERROR
007070        MOVE 1 TO WS-RPTREQUEST
007080     END-IF.
007090
007100 A400-EXIT.
007110      EXIT.
007120
007130 A500-CREATE-REPORT-HEADING.
007140*********************************************************
007150*
007160*********************************************************
007170     MOVE CHR34   TO  FILLER1  FILLER2  FILLER3
007180                      FILLER4  FILLER5  FILLER6.
007190*    INSERT A PRINTDATA RECORD FOR THE REPORT HEADING
007200     MOVE WS-BEG-ACCT-BAL-DT         TO  HEAD-LAST-ACCT-BAL-DT.
007210     MOVE WS-END-ACCT-BAL-DT         TO  HEAD-NEW-ACCT-BAL-DT.
007220     MOVE FDP-CYC-DT                 TO  HEAD-CYC-DT.
007230
007240     ADD    +1    TO    WS-SEQ-NUM.
007250
007260     MOVE  RPRQ1-RPTREQUESTID        TO  PRDT1-RPTREQUESTID.
007270     MOVE  WS-SEQ-NUM                TO  PRDT1-SEQNUM.
007280     MOVE  HEAD-REC                  TO  PRDT1-CDL.
007290     MOVE  'N'                       TO  PRDT1-CONTINUECD.
007300     MOVE  ZEROES                    TO  PRDT1-AUDITID.
007310     MOVE 'HEAD'                     TO  PRDT1-RECORDTYPE.
007320** CHANGED FROM SPACES TO 00 THIS WILL SORT HEADER RECORD TO TOP
007330     MOVE '00'                       TO  PRDT1-SORTDATA.
007340
007350     CALL 'CI1PRDTD' USING RMESSAGE,
007360                           PRDT1-DATA,
007370                           RTN-OUTPUT-FIELDS.
007380*
007390 A500-EXIT.
007400      EXIT.
007410
007420 B000-MAIN.
007430*********************************************************
007440* MAIN LOOP TO PROCESS JOURNAL LOG RECORDS
007450* ORDERED BY ACCT-KEY,MKEY-CO-NUM,MKEY-PROD-NUM,CASE-KEY
007460* MAJOR BREAK ON ACCOUNT KEY
007470* MINOR BREAK ON COMPANY, PRODUCT AND CASE WITHIN A ACCT-KEY
007480*********************************************************
007490     INITIALIZE JLOG4-DATA.
007500     INITIALIZE WS-ACCUMULATORS.
007510
007520     PERFORM UNTIL JLOG4-END-FLAG = 'Y'  OR  RTN-ERROR
007530
007540*       FETCH NEXT BLOCK OF JOURNAL LOGS
007550        MOVE WS-BEG-ACCT-BAL-DT  TO  JLOG4-BEGIN-DT
007560        MOVE WS-END-ACCT-BAL-DT  TO  JLOG4-END-DT
007570        EVALUATE TRUE
007580          WHEN COMPANY-LEVEL
007590               MOVE WS-IN-CO-NUM   TO  JLOG4-MKEY-CO-NUM (1)
007600          WHEN PRODUCT-LEVEL
007610               MOVE WS-IN-PROD-NUM TO  JLOG4-MKEY-PROD-NUM (1)
007620          WHEN CASE-LEVEL
007630               MOVE WS-IN-CASE-KEY TO  JLOG4-CASE-KEY (1)
007640        END-EVALUATE
007650
007660        CALL 'CF3VJLLD'    USING RMESSAGE,
007670                                 JLOG4-DATA,
007680                                 RTN-OUTPUT-FIELDS
007690
007700**      SETUP FOR FIRST TIME THRU
007710        IF JLOG4-CUMUL-ROWS >  0  AND
007720           JLOG4-CUMUL-ROWS <= 50
007730*          SETUP HOLD AREAS PREVENT INITIAL BREAK
007740           MOVE JLOG4-ACCT-KEY (1)      TO WS-HOLD-ACCT-KEY
007750           MOVE JLOG4-MKEY-CO-NUM (1)   TO WS-HOLD-CO-NUM
007760           MOVE JLOG4-MKEY-PROD-NUM (1) TO WS-HOLD-PROD-NUM
007770           MOVE JLOG4-CASE-KEY (1)      TO WS-HOLD-CASE-KEY
007780           MOVE 1 TO IJLOG
007790           PERFORM B100-GET-LEDGER-ACCOUNT THRU B100-EXIT
007800           IF BYPASS-TRUE
007810              PERFORM B150-DELETE-COMPANY-ACCTS THRU B150-EXIT            7600
007820           END-IF
007830        END-IF
007840
007850**      PROCESS EACH JOURNAL LOG RECORD
007860**
007870        PERFORM VARYING IJLOG FROM 1 BY 1
007880                UNTIL IJLOG > JLOG4-ROWS-FOUND OR  RTN-ERROR
007890
007900*          LEDGER_ACCOUNT KEY BREAK
007910           IF JLOG4-ACCT-KEY (IJLOG) = WS-HOLD-ACCT-KEY
007920              CONTINUE
007930           ELSE
007940**           LEDGER ACCT CHANGED, PROCESS DATA FROM PRIOR ACCT
007950             IF BYPASS-FALSE
007960                PERFORM B700-WRITE-COAC-TOTAL THRU B700-EXIT                                     00049000
007970             END-IF
007980             IF RTN-NO-ERROR
007990               PERFORM B100-GET-LEDGER-ACCOUNT THRU B100-EXIT
008000               INITIALIZE WS-HOLD-VARIABLES
008010               INITIALIZE WS-CODES
008020               INITIALIZE WS-ACCUMULATORS
008030*              SETUP HOLD AREAS PREVENT INITIAL BREAK
008040               MOVE JLOG4-ACCT-KEY (IJLOG)     TO WS-HOLD-ACCT-KEY
008050               MOVE JLOG4-MKEY-CO-NUM (IJLOG)  TO WS-HOLD-CO-NUM
008060               MOVE JLOG4-MKEY-PROD-NUM(IJLOG) TO WS-HOLD-PROD-NUM
008070               MOVE JLOG4-CASE-KEY (IJLOG)     TO WS-HOLD-CASE-KEY
008080               IF BYPASS-TRUE
008090                  PERFORM B150-DELETE-COMPANY-ACCTS THRU B150-EXIT         500
008100               END-IF
008110             END-IF
008120           END-IF
008130
008140**      CHECK THE BALANCING LEVEL & PROCESS JLOG RECORD
008150
008160**         ISSUE ONE WARNING IF THE BALANCE LEVEL IS NOT SET
008170           IF NOT-APPLICABLE  AND  RTN-NO-ERROR
008180                              AND  WARNING-NOT-ISSUED
008190              DISPLAY 'ERROR BAL-LVL-CD SHOULD BE SET'
008200                      ' ACCT1-ACCT-KEY = ' ACCT1-ACCT-KEY
008210              MOVE 'TX'            TO RTN-ERR-PREFIX
008220              MOVE 1002            TO RTN-ERROR-CODE
008230              MOVE 'CACBLTOB'      TO RTN-PROG-NAME
008240              MOVE 'B000'          TO RTN-PARA-NAME
008250              MOVE ACCT1-ACCT-KEY  TO WS-ERR-ACCT-KEY
008260              MOVE 1               TO RTN-VAR-SEQ-NO (1)
008270              MOVE WS-ERR-MSG1     TO RTN-VAR-TEXT (1)
008280              MOVE 1               TO RTN-VAR-DETL-CNT
008290              CALL 'CUTSVARB' USING RMESSAGE,
008300                           RTN-OUTPUT-FIELDS
008310              MOVE 1               TO WS-WARNING-FLAG
008320           END-IF
008330
008340           IF COMPANY-LEVEL AND RTN-NO-ERROR AND BYPASS-FALSE
008350              PERFORM B300-COMPANY-LEVEL THRU B300-EXIT
008360           END-IF
008370
008380           IF PRODUCT-LEVEL AND RTN-NO-ERROR AND BYPASS-FALSE
008390              PERFORM B400-PRODUCT-LEVEL THRU B400-EXIT
008400           END-IF
008410
008420           IF CASE-LEVEL    AND RTN-NO-ERROR AND BYPASS-FALSE
008430              PERFORM B500-CASE-LEVEL THRU B500-EXIT
008440           END-IF
008450
008460        END-PERFORM
008470     END-PERFORM.
008480
008490*    FLUSH OUT TOTALS FOR LAST RECORD
008500     IF RTN-NO-ERROR AND JLOG4-CUMUL-ROWS > 0 AND BYPASS-FALSE
008510        PERFORM B700-WRITE-COAC-TOTAL THRU B700-EXIT                                          00055200
008520     END-IF.
008530
008540
008550*    ISSUE A WARNING IF NO JOURNAL LOGS WERE PROCESSED
008560     IF RTN-NO-ERROR  AND  JLOG4-CUMUL-ROWS = 0
008570        MOVE 'TX'            TO RTN-ERR-PREFIX                            36200
008580        MOVE 1013            TO RTN-ERROR-CODE                            36300
008590        MOVE 'CACBLTOB'      TO RTN-PROG-NAME                             36400
008600        MOVE 'B000'          TO RTN-PARA-NAME                             36500
008610        CALL 'CUTSVARB' USING RMESSAGE,
008620                             RTN-OUTPUT-FIELDS
008630     END-IF.
008640
008650     IF RTN-NO-ERROR  AND  JLOG4-CUMUL-ROWS > 0
008660*       UPDATE BLOCK OF JOURNAL_LOGS WITH HOLD_CD = 1 ADHOC BAL
008670        INITIALIZE JLOG5-DATA
008680        MOVE WS-BEG-ACCT-BAL-DT  TO  JLOG5-LAST-ACCT-BAL-DT1
008690        MOVE WS-END-ACCT-BAL-DT  TO  JLOG5-NEW-ACCT-BAL-DT
008700        MOVE WS-IN-CO-NUM        TO  JLOG5-MKEY-CO-NUM
008710        MOVE WS-IN-PROD-NUM      TO  JLOG5-MKEY-PROD-NUM
008720        MOVE WS-IN-CASE-KEY      TO  JLOG5-CASE-KEY
008730
008740        MOVE '1'                 TO  JLOG5-HOLD-CD
008750        MOVE '0'                 TO  JLOG5-FROM-HOLD-CD
008760
008770        CALL 'CU3JLOGD' USING RMESSAGE,
008780                              JLOG5-DATA,
008790                              RTN-OUTPUT-FIELDS
008800     END-IF.
008810
008820*    UPDATE THE BARL RULE TO PROCESSED
008830     IF RTN-NO-ERROR
008840         INITIALIZE BARL1-DATA
008850         MOVE FDP-REF-KEY TO BARL1-BARL-KEY
008860         MOVE '01'        TO BARL1-STAT-CD
008870         CALL 'CU2BARLD' USING RMESSAGE,                                   0
008880                               BARL1-DATA,                                 0
008890                               RTN-OUTPUT-FIELDS                           0
008900     END-IF.
008910
008920*    RECORD THAT ACCOUNT BALANCING RAN SUCCESSFULLY IN
008930*    THE SCHED_ACCT_BAL TABLE
008940     IF RTN-NO-ERROR
008950         PERFORM B800-INSERT-SCHED-ACCT-BAL
008960            THRU B800-EXIT
008970     END-IF.
008980
008990  B000-EXIT.
009000     EXIT.
009010
009020 B100-GET-LEDGER-ACCOUNT.
009030*********************************************************
009040** GET THE LEDGER ACCOUNT
009050** CREATE A SCHED ACTIVITY RECORD FOR THIS ACCOUNT
009060*********************************************************
009070     INITIALIZE ACCT1-DATA.
009080     SET BYPASS-FALSE TO TRUE.
009090     MOVE JLOG4-ACCT-KEY (IJLOG) TO ACCT1-ACCT-KEY.
009100
009110     CALL 'CS2ACCTD'    USING RMESSAGE,
009120                              ACCT1-DATA,
009130                              RTN-OUTPUT-FIELDS.
009140
009150*    HOLD THIS INFORMATION FOR BATCH REPORTING
009160     MOVE ACCT1-ACCT-NO         TO WS-HOLD-ACCT-NO.
009170     MOVE ACCT1-SUB-ACCT-NO     TO WS-HOLD-SUB-ACCT-NO.
009180
009190* CHECK THE BALANCING LEVEL.  IF IT DOES NOT MATCH THE
009200* LEVEL WE ARE PROCESSING ISSUE A WARNING AND BYPASS
009210* ALL JOURNAL LOGS FOR THIS ACCOUNT
009220     IF COMPANY-LEVEL AND RTN-NO-ERROR
009230        IF ACCT1-BAL-LVL-CD = '02'
009240           CONTINUE
009250        ELSE                                                              9400
009260           SET BYPASS-TRUE  TO TRUE
009270           DISPLAY 'WARNING BALANCE LEVEL FOR '
009280                    ACCT1-ACCT-KEY
009290                   ' IS NOT SET TO COMPANY, BYPASSING JLOGS'
009300        END-IF
009310     END-IF.
009320
009330     IF PRODUCT-LEVEL AND RTN-NO-ERROR
009340        IF ACCT1-BAL-LVL-CD = '03'
009350           CONTINUE
009360        ELSE                                                              9400
009370           SET BYPASS-TRUE  TO TRUE
009380           DISPLAY 'WARNING BALANCE LEVEL FOR '
009390                    ACCT1-ACCT-KEY
009400                   ' IS NOT SET TO PRODUCT, BYPASSING JLOGS'
009410        END-IF
009420     END-IF.
009430
009440     IF CASE-LEVEL AND RTN-NO-ERROR
009450        IF ACCT1-BAL-LVL-CD = '04'
009460           CONTINUE
009470        ELSE                                                              9400
009480           SET BYPASS-TRUE  TO TRUE
009490           DISPLAY 'WARNING BALANCE LEVEL FOR '
009500                    ACCT1-ACCT-KEY
009510                   ' IS NOT SET TO CASE, BYPASSING JLOGS'
009520        END-IF
009530     END-IF.
009540
009550     IF BYPASS-TRUE
009560           MOVE 'TX'            TO RTN-ERR-PREFIX                          0036200
009570           MOVE 1012            TO RTN-ERROR-CODE                          0036300
009580           MOVE 'CACBLTOB'      TO RTN-PROG-NAME                           0036400
009590           MOVE 'B100'          TO RTN-PARA-NAME                           0036500
009600           MOVE ACCT1-ACCT-KEY  TO WS-ERR-ACCT-KEY
009610           MOVE ACCT1-ACCT-NO   TO WS-ERR-ACCT-NO
009620           MOVE ACCT1-SUB-ACCT-NO TO WS-ERR-SUB-ACCT-NO
009630           MOVE 1               TO RTN-VAR-SEQ-NO (1)
009640           MOVE WS-ERR-MSG1     TO RTN-VAR-TEXT   (1)
009650           MOVE 2               TO RTN-VAR-SEQ-NO (2)
009660           MOVE WS-ERR-MSG2     TO RTN-VAR-TEXT   (2)
009670           MOVE 3               TO RTN-VAR-SEQ-NO (3)
009680           MOVE WS-ERR-MSG3     TO RTN-VAR-TEXT   (3)
009690           MOVE 3               TO RTN-VAR-DETL-CNT
009700           CALL 'CUTSVARB' USING RMESSAGE,                                6600
009710                                RTN-OUTPUT-FIELDS                         6700
009720     END-IF.
009730
009740 B100-EXIT.
009750     EXIT.
009760
009770 B150-DELETE-COMPANY-ACCTS.
009780*********************************************************
009790* DELETE SELECTED COMPANY ACCOUNTS THAT WERE ROLLED FORWARD BY
009800* A200-INSERT-COMPANY-ACCOUNTS.  ALL COMPANY ACCOUNTS
009810* BROUGHT FORWARD THAT DO NOT MATCH THE ACCOUNTS LEVEL OF
009820* BALANCING WILL BE DELETED. ALL JLOGS WILL BE BYPASSED.
009830* COMPANY, PRODUCT, AND CASE VALUES ARE NOT USED FOR A
009840* MATCH CONDITION.  THEY ARE USED BY THE DAL TO DETERMINE THE
009850* LEVEL OF BALANCING.
009860*********************************************************
009870     INITIALIZE COAC1-DATA.
009880     IF ACCT1-BAL-LVL-CD = '02'
009890        MOVE  1  TO  COAC1-MKEY-CO-NUM
009900     END-IF.
009910     IF ACCT1-BAL-LVL-CD = '03'
009920        MOVE  1  TO  COAC1-MKEY-PROD-NUM
009930     END-IF.
009940     IF ACCT1-BAL-LVL-CD = '04'
009950        MOVE  1  TO  COAC1-CASE-KEY
009960     END-IF.
009970
009980     MOVE WS-END-ACCT-BAL-DT TO COAC1-ACCT-END-BAL-DT.
009990     MOVE ACCT1-ACCT-KEY     TO COAC1-ACCT-KEY.
010000
010010     CALL 'CD1COACD' USING RMESSAGE,
010020                           COAC1-DATA,
010030                           RTN-OUTPUT-FIELDS.
010040
010050 B150-EXIT.
010060    EXIT.
010070
010080 B300-COMPANY-LEVEL.
010090*********************************************************
010100** TWO BREAK CONDITIONS
010110** LEDGER ACCOUNT NUMBER CHANGED  ( HANDLED IN B600 )
010120** COMPANY NUM CHANGED WITHIN A LEDGER ACCOUNT
010130*********************************************************
010140     IF JLOG4-MKEY-CO-NUM (IJLOG) = WS-HOLD-CO-NUM
010150        PERFORM B600-PROCESS-DEBIT-CREDIT THRU B600-EXIT
010160     ELSE
010170*       WRITE OUT TOTALS FOR PREVIOUS ACCOUNT/COMPANY
010180        PERFORM B700-WRITE-COAC-TOTAL THRU B700-EXIT
010190*       CLEAR ACCUMULATORS, GET READY FOR NEXT COMPANY
010200        INITIALIZE WS-ACCUMULATORS
010210        MOVE JLOG4-MKEY-CO-NUM (IJLOG) TO WS-HOLD-CO-NUM
010220*       ADD TO ACCUMULATORS DATA FROM CURRENT RECORD NEW COMPANY
010230        PERFORM B600-PROCESS-DEBIT-CREDIT THRU B600-EXIT
010240     END-IF.
010250
010260 B300-EXIT.
010270     EXIT.
010280
010290 B400-PRODUCT-LEVEL.
010300*********************************************************
010310** TWO BREAK CONDITIONS
010320** LEDGER ACCOUNT NUMBER CHANGED  ( HANDLED IN B600 )
010330** PRODUCT NUM CHANGED WITHIN A LEDGER ACCOUNT
010340*********************************************************
010350     IF JLOG4-MKEY-PROD-NUM (IJLOG) = WS-HOLD-PROD-NUM
010360        PERFORM B600-PROCESS-DEBIT-CREDIT THRU B600-EXIT
010370     ELSE
010380*       WRITE OUT TOTALS FOR PREVIOUS ACCOUNT/PRODUCT
010390        PERFORM B700-WRITE-COAC-TOTAL THRU B700-EXIT
010400*       CLEAR ACCUMULATORS, GET READY FOR NEXT PRODUCT
010410        INITIALIZE WS-ACCUMULATORS
010420        MOVE JLOG4-MKEY-PROD-NUM (IJLOG) TO WS-HOLD-PROD-NUM
010430*       ADD TO ACCUMULATORS DATA FROM CURRENT RECORD NEW PRODUCT
010440        PERFORM B600-PROCESS-DEBIT-CREDIT THRU B600-EXIT
010450     END-IF.
010460 B400-EXIT.
010470     EXIT.
010480
010490 B500-CASE-LEVEL.
010500*********************************************************
010510** TWO BREAK CONDITIONS
010520** LEDGER ACCOUNT NUMBER CHANGED  ( HANDLED IN B600 )
010530** CASE KEY CHANGED WITHIN A LEDGER ACCOUNT
010540*********************************************************
010550     IF JLOG4-CASE-KEY (IJLOG) = WS-HOLD-CASE-KEY
010560        PERFORM B600-PROCESS-DEBIT-CREDIT THRU B600-EXIT
010570     ELSE
010580*       WRITE OUT TOTALS FOR PREVIOUS ACCOUNT/CASE
010590        PERFORM B700-WRITE-COAC-TOTAL THRU B700-EXIT
010600*       CLEAR ACCUMULATORS, GET READY FOR NEXT CASE
010610        INITIALIZE WS-ACCUMULATORS
010620        MOVE JLOG4-CASE-KEY (IJLOG) TO WS-HOLD-CASE-KEY
010630*       ADD TO ACCUMULATORS DATA FROM CURRENT RECORD NEW CASE
010640        PERFORM B600-PROCESS-DEBIT-CREDIT THRU B600-EXIT
010650     END-IF.
010660
010670 B500-EXIT.
010680     EXIT.
010690
010700 B600-PROCESS-DEBIT-CREDIT.
010710*********************************************************
010720** PROCESS JOURNAL LOG AMOUNT
010730*********************************************************
010740     IF JLOG4-DB-CR-CD (IJLOG) = 'DB' OR
010750        JLOG4-DB-CR-CD (IJLOG) = 'DX'
010760           ADD JLOG4-JRNL-AMT (IJLOG) TO WS-TOTAL-DEBITS
010770     END-IF.
010780
010790     IF JLOG4-DB-CR-CD (IJLOG) = 'CR' OR
010800        JLOG4-DB-CR-CD (IJLOG) = 'CX'
010810           ADD JLOG4-JRNL-AMT (IJLOG) TO WS-TOTAL-CREDITS
010820     END-IF.
010830
010840 B600-EXIT.
010850     EXIT.
010860
010870 B700-WRITE-COAC-TOTAL.
010880*********************************************************
010890** USE WS-HOLD-VARIABLE KEYS
010900*********************************************************
010910     IF NOT-APPLICABLE
010920        GO TO B700-EXIT
010930     END-IF.
010940
010950* CALCULATE TOTAL DEBITS AND CREDITS
010960     COMPUTE WS-DB-CR-TOTAL =
010970             WS-TOTAL-CREDITS - WS-TOTAL-DEBITS.
010980
010990* ATTEMPT TO UPDATE COMPANY ACCOUNT BY ACCT KEY/LEVEL
011000* OF BALANCE FIELD
011010* IF 1403 ERROR  (COAC DOES NOT EXIST  ROWS-FOUND=ZERO)
011020* CALL DAL PROGRAM TO SUMMARIZE ALL JOURNAL LOGS <= BEGIN BAL DT
011030* INSERT A COMPANY ACCOUNT WITH ACCT KEY/LEVEL OF BALANCE FIELD
011040* AND BALANCE AMOUNT
011050
011060* ATTEMPT UPDATE COMPANY ACCOUNT WITH CLOSING BALANCE
011070     IF RTN-NO-ERROR
011080        PERFORM B720-UPDATE-COMPANY-ACCT THRU B720-EXIT
011090     END-IF.
011100
011110     IF COAC1-ROWS-FOUND = ZEROES  AND  RTN-NO-ERROR
011120*       SUMMARIZE JOURNAL LOGS
011130        PERFORM B740-SUM-JLOGS           THRU B740-EXIT
011140        IF RTN-NO-ERROR
011150*          INSERT NEW COMPANY ACCOUNT
011160           PERFORM B760-INSERT-COMPANY-ACCT THRU B760-EXIT
011170        END-IF
011180        IF RTN-NO-ERROR
011190*          BUILD PRINT DATA BASED ON A INSERTED COMPANY ACCOUNT
011200           PERFORM B910-BUILD-PRINTDATA-INS
011210              THRU B910-EXIT
011220        END-IF
011230     ELSE
011240        IF RTN-NO-ERROR
011250*          BUILD PRINTDATA BASED ON A UPDATED COMPANY ACCOUNT             4900
011260           PERFORM B900-BUILD-PRINTDATA-UPD THRU B900-EXIT
011270        END-IF
011280     END-IF.
011290
011300     IF RTN-NO-ERROR
011310        PERFORM B920-BUILD-WRITE-PRINTDATA THRU B920-EXIT
011320     END-IF.
011330
011340 B700-EXIT.
011350     EXIT.
011360
011370 B720-UPDATE-COMPANY-ACCT.
011380*********************************************************
011390* UPDATE BY ACCT, ACCT/CO, ACCT/PROD, OR ACCT/CASE
011400*********************************************************
011410     INITIALIZE COAC1-DATA.
011420     IF COMPANY-LEVEL
011430        MOVE WS-HOLD-CO-NUM   TO  COAC1-MKEY-CO-NUM
011440     END-IF.
011450     IF PRODUCT-LEVEL
011460        MOVE WS-HOLD-PROD-NUM  TO  COAC1-MKEY-PROD-NUM
011470     END-IF.
011480     IF CASE-LEVEL
011490        MOVE WS-HOLD-CASE-KEY  TO  COAC1-CASE-KEY
011500     END-IF.
011510
011520*    UPDATE COMPANY_ACCTS WITH THE ENDING BALANCE AMOUNT
011530*
011540     MOVE WS-HOLD-ACCT-KEY              TO  COAC1-ACCT-KEY.
011550     MOVE WS-END-ACCT-BAL-DT            TO  COAC1-EFF-DT.
011560     MOVE WS-DB-CR-TOTAL                TO  COAC1-TOT-END-BAL-AMT.
011570*    THE ACTUAL END BALANCE AMOUNT IS CALCULATED IN THE
011580*    DAL PROGRAM USING THE FOLLOWING FORMULA
011590*    COMPUTE COAC1-TOT-END-BAL-AMT =  COAC1-BEG-BAL-AMT PLUS
011600*                                     WS-DB-CR-TOTAL (PASSED IN)
011610
011620     CALL 'CU2COACD' USING RMESSAGE,
011630                           COAC1-DATA,
011640                           RTN-OUTPUT-FIELDS.
011650
011660 B720-EXIT.
011670     EXIT.
011680
011690 B740-SUM-JLOGS.
011700*********************************************************
011710* SUM BY ACCT, ACCT/CO, ACCT/PROD, OR ACCT/CASE
011720*********************************************************
011730     INITIALIZE JLOG3-DATA.
011740     IF COMPANY-LEVEL
011750        MOVE WS-HOLD-CO-NUM    TO  JLOG3-MKEY-CO-NUM                       0
011760     END-IF.
011770     IF PRODUCT-LEVEL
011780        MOVE WS-HOLD-PROD-NUM  TO  JLOG3-MKEY-PROD-NUM
011790     END-IF.
011800     IF CASE-LEVEL
011810        MOVE WS-HOLD-CASE-KEY  TO  JLOG3-CASE-KEY
011820     END-IF.
011830*
011840     MOVE WS-HOLD-ACCT-KEY        TO  JLOG3-ACCT-KEY.
011850     MOVE WS-BEG-ACCT-BAL-DT      TO  JLOG3-LAST-ACCT-BAL-DT1.
011860
011870     CALL 'CS2JLOGD' USING RMESSAGE,
011880                           JLOG3-DATA,
011890                           RTN-OUTPUT-FIELDS.
011900
011910 B740-EXIT.
011920     EXIT.
011930
011940 B760-INSERT-COMPANY-ACCT.
011950*********************************************************
011960* INSERT NEW COAC RECORD POPULATE FIELDS BASED ON LEVEL
011970* OF BALANCING
011980*********************************************************
011990     INITIALIZE COAC1-DATA.
012000     IF COMPANY-LEVEL
012010        MOVE WS-HOLD-CO-NUM    TO  COAC1-MKEY-CO-NUM
012020     END-IF.
012030     IF PRODUCT-LEVEL
012040        MOVE WS-HOLD-PROD-NUM  TO  COAC1-MKEY-PROD-NUM
012050     END-IF.
012060     IF CASE-LEVEL
012070        MOVE WS-HOLD-CASE-KEY  TO  COAC1-CASE-KEY
012080     END-IF.
012090
012100     MOVE WS-HOLD-ACCT-KEY              TO  COAC1-ACCT-KEY.
012110     MOVE WS-END-ACCT-BAL-DT            TO  COAC1-EFF-DT.
012120     MOVE FDP-TR-REF-NO                 TO  COAC1-TR-REF-NO.
012130*    SUM OF ALL JLOGS <= BEGIN-BAL-DATE
012140*    PLUS THE SUM OF JLOGS >BEGIN-BAL-DT AND <= END-BAL-DT
012150     ADD  JLOG3-DB-CR-TOTAL             TO  WS-DB-CR-TOTAL.
012160     MOVE WS-DB-CR-TOTAL                TO  COAC1-END-BAL-AMT.
012170
012180     CALL 'CI1COACD' USING RMESSAGE,
012190                           COAC1-DATA,
012200                           RTN-OUTPUT-FIELDS.
012210
012220 B760-EXIT.
012230     EXIT.
012240
012250 B800-INSERT-SCHED-ACCT-BAL.
012260*********************************************************
012270**   INSERT ONE RECORD IN THE SCHED_ACCT_BAL TABLE FOR THE
012280**   ACCOUNT BALANCING JOB
012290*********************************************************
012300     INITIALIZE SCAB1-DATA.
012310*    '03' ADHOC BALANCE
012320     MOVE '03'                 TO SCAB1-BAL-TYP-CD.
012330     MOVE WS-END-ACCT-BAL-DT   TO SCAB1-EFF-DT.
012340     MOVE 0                    TO SCAB1-REAR-KEY.
012350     MOVE '01'                 TO SCAB1-STAT-CD.
012360     MOVE FDP-TR-REF-NO        TO SCAB1-TR-REF-NO.
012370     MOVE FDP-REF-KEY          TO SCAB1-BARL-KEY.
012380
012390     CALL 'CI1SCABD' USING RMESSAGE,
012400                           SCAB1-DATA,
012410                           RTN-OUTPUT-FIELDS.
012420
012430 B800-EXIT.
012440     EXIT.
012450
012460 B900-BUILD-PRINTDATA-UPD.
012470*********************************************************
012480**   BUILD PRINTDATA FOR A UPDATED COMPANY ACCOUNT
012490**   SELECT THE COMPANY ACCOUNT WE JUST UPDATED FOR REPORT
012500**   INFORMATION
012510*********************************************************
012520     MOVE WS-TOTAL-CREDITS TO DETL2-TOT-CREDITS.
012530     MOVE WS-TOTAL-DEBITS  TO DETL2-TOT-DEBITS.
012540*    USE THE CS2COACD DAL TO READ THIS INFORMATION
012550     PERFORM B940-READ-COMPANY-ACCOUNTS THRU B940-EXIT.                                      00085200
012560     IF COAC1-ROWS-FOUND > 0
012570        MOVE COAC1-BEG-BAL-AMT TO DETL2-BEG-BAL-AMT
012580        MOVE COAC1-END-BAL-AMT TO DETL2-END-BAL-AMT
012590     ELSE
012600*    DAL WILL CALL CUTSVARB FOR A NOT FOUND CONDITION
012610        MOVE 0                TO DETL2-BEG-BAL-AMT
012620        MOVE 0                TO DETL2-END-BAL-AMT
012630     END-IF.
012640
012650 B900-EXIT.
012660     EXIT.
012670
012680 B910-BUILD-PRINTDATA-INS.
012690*********************************************************
012700**   BUILD PRINTDATA FOR A INSERTED COMPANY ACCOUNT
012710*********************************************************
012720     ADD  WS-TOTAL-CREDITS TO JLOG3-TOT-CREDITS
012730          GIVING DETL2-TOT-CREDITS.
012740     ADD  WS-TOTAL-DEBITS  TO JLOG3-TOT-DEBITS
012750          GIVING DETL2-TOT-DEBITS.
012760     MOVE 0                TO DETL2-BEG-BAL-AMT.
012770     MOVE WS-DB-CR-TOTAL   TO DETL2-END-BAL-AMT.
012780
012790 B910-EXIT.
012800     EXIT.
012810
012820 B920-BUILD-WRITE-PRINTDATA.
012830*********************************************************
012840**   BUILD PRINTDATA FOR REST OF THE FIELDS
012850**   BUILD THE SORT DATA
012860**   CALL PARA TO INSERT THE PRINTDATA RECORD
012870*********************************************************
012880     MOVE SPACES TO DETL-BAL-LVL-CD.
012890     MOVE SPACES TO DETL-ACCT-NM.
012900     MOVE SPACES TO DETL-ACCT-NO.
012910     MOVE SPACES TO DETL-SUB-ACCT-NO.
012920     MOVE SPACES TO DETL-COMPANY-DESCRIPT.
012930     MOVE SPACES TO DETL-PRODUCT-DESCRIPT.
012940     MOVE SPACES TO DETL-CONT-NO.
012950     MOVE SPACES TO DETL-CAT-NO.
012960     MOVE SPACES TO DETL-PLAN-NM.
012970
012980     IF COMPANY-LEVEL
012990*       LOOK UP COMPANY REF CODE
013000        PERFORM Z600-GET-COMPANY-DESCRIPT  THRU Z600-EXIT
013010     END-IF.
013020     IF PRODUCT-LEVEL
013030*       LOOK UP PRODUCT REF CODE
013040        PERFORM Z700-GET-PRODUCT-DESCRIPT  THRU Z700-EXIT
013050     END-IF.
013060     IF CASE-LEVEL
013070*       LOOK UP SCHEME INFORMATION
013080        PERFORM Z800-GET-CASD-DATA  THRU Z800-EXIT
013090     END-IF.
013100
013110     MOVE WS-BALANCE-LEVEL   TO  DETL-BAL-LVL-CD.
013120     MOVE ACCT1-ACCT-NM      TO  DETL-ACCT-NM.
013130     MOVE ACCT1-ACCT-NO      TO  DETL-ACCT-NO.
013140     MOVE ACCT1-SUB-ACCT-NO  TO  DETL-SUB-ACCT-NO.
013150*
013160* BUILD SORT-DATA
013170     INITIALIZE SORT-REC.
013180     MOVE WS-BALANCE-LEVEL      TO  SORT-BAL-LVL-CD.
013190     MOVE ACCT1-ACCT-NO         TO  SORT-ACCT-NO.
013200     MOVE ACCT1-SUB-ACCT-NO     TO  SORT-SUB-ACCT-NO.
013210     MOVE DETL-COMPANY-DESCRIPT TO  SORT-COMPANY-DESCRIPT.
013220     MOVE DETL-PRODUCT-DESCRIPT TO  SORT-PRODUCT-DESCRIPT.
013230     MOVE DETL-CONT-NO          TO  SORT-CONT-NO.
013240     MOVE DETL-CAT-NO           TO  SORT-CAT-NO.
013250*
013260     PERFORM B930-INSERT-PRINTDATA  THRU  B930-EXIT.                                            00100400
013270
013280 B920-EXIT.
013290     EXIT.
013300
013310 B930-INSERT-PRINTDATA.
013320*********************************************************
013330*
013340*********************************************************
013350     IF LEDGER-LEVEL                                                      200
013360        MOVE 'LEDG' TO WS-RECTYPE
013370     END-IF.
013380     IF COMPANY-LEVEL
013390        MOVE 'COMP' TO WS-RECTYPE
013400     END-IF.
013410     IF PRODUCT-LEVEL
013420        MOVE 'PROD' TO WS-RECTYPE
013430     END-IF.
013440     IF CASE-LEVEL
013450        MOVE 'SCHE' TO WS-RECTYPE
013460     END-IF.
013470
013480*    INCREMENT COUNTER FOR PRINT LINES
013490     ADD    +1    TO    WS-SEQ-NUM.
013500*    INSERT A NEW ROW INTO PRINTDATA FOR THE DETAIL LINE
013510*    CONTINUE CODE = Y
013520     MOVE  RPRQ1-RPTREQUESTID        TO   PRDT1-RPTREQUESTID.
013530     MOVE  WS-SEQ-NUM                TO   PRDT1-SEQNUM.
013540     MOVE  DETL-REC                  TO   PRDT1-CDL.
013550     MOVE  'Y'                       TO   PRDT1-CONTINUECD.
013560     MOVE  ZEROES                    TO   PRDT1-AUDITID.
013570     MOVE WS-RECTYPE                 TO   PRDT1-RECORDTYPE.
013580     MOVE SORT-REC                   TO   PRDT1-SORTDATA.
013590
013600     CALL 'CI1PRDTD' USING RMESSAGE,
013610                           PRDT1-DATA,
013620                           RTN-OUTPUT-FIELDS.
013630
013640
013650*    INCREMENT COUNTER FOR PRINT LINES
013660     ADD    +1    TO    WS-SEQ-NUM.
013670*    INSERT A NEW ROW INTO PRINTDATA FOR THE DETAIL LINE
013680*    CONTINUE CODE = N
013690     MOVE  RPRQ1-RPTREQUESTID        TO   PRDT1-RPTREQUESTID.
013700     MOVE  WS-SEQ-NUM                TO   PRDT1-SEQNUM.
013710     MOVE  DETL-REC2                 TO   PRDT1-CDL.
013720     MOVE  'N'                       TO   PRDT1-CONTINUECD.
013730     MOVE  ZEROES                    TO   PRDT1-AUDITID.
013740     MOVE WS-RECTYPE                 TO   PRDT1-RECORDTYPE.
013750     MOVE SORT-REC                   TO   PRDT1-SORTDATA.
013760
013770     CALL 'CI1PRDTD' USING RMESSAGE,
013780                           PRDT1-DATA,
013790                           RTN-OUTPUT-FIELDS.
013800 B930-EXIT.
013810     EXIT.
013820
013830 B940-READ-COMPANY-ACCOUNTS.
013840*********************************************************
013850**   READ A SINGLE COMPANY ACCOUNT TO GET DATA FOR REPORT
013860* SELECT BY ACCT, ACCT/CO, ACCT/PROD, OR ACCT/CASE
013870*********************************************************
013880     INITIALIZE COAC1-DATA.
013890     IF COMPANY-LEVEL
013900        MOVE WS-HOLD-CO-NUM   TO  COAC1-MKEY-CO-NUM
013910     END-IF.
013920     IF PRODUCT-LEVEL
013930        MOVE WS-HOLD-PROD-NUM  TO  COAC1-MKEY-PROD-NUM
013940     END-IF.
013950     IF CASE-LEVEL
013960        MOVE WS-HOLD-CASE-KEY  TO  COAC1-CASE-KEY
013970     END-IF.
013980
013990     MOVE WS-HOLD-ACCT-KEY              TO  COAC1-ACCT-KEY.
014000     MOVE WS-END-ACCT-BAL-DT            TO  COAC1-EFF-DT.
014010
014020     CALL 'CS2COACD' USING RMESSAGE,
014030                           COAC1-DATA,
014040                           RTN-OUTPUT-FIELDS.
014050
014060 B940-EXIT.
014070     EXIT.
014080
014090 Z300-ROLLBACK-COMPANY-ACCOUNTS.
014100*********************************************************
014110* DELETE ALL COMPANY ACCOUNTS THAT WERE ROLLED FORWARD
014120*********************************************************
014130     INITIALIZE COAC1-DATA.
014140     MOVE FDP-TR-REF-NO TO COAC1-TR-REF-NO.
014150
014160     CALL 'CD2COACD' USING RMESSAGE,
014170                           COAC1-DATA,
014180                           RTN-OUTPUT-FIELDS.
014190
014200 Z300-EXIT.
014210     EXIT.
014220
014230 Z320-DELETE-PRINTDATA.
014240*********************************************************
014250*
014260*********************************************************
014270     INITIALIZE PRDT1-DATA.
014280     MOVE RPRQ1-RPTREQUESTID    TO  PRDT1-RPTREQUESTID.
014290
014300     CALL 'CD1PRDTD' USING RMESSAGE,
014310                           PRDT1-DATA,
014320                           RTN-OUTPUT-FIELDS.
014330
014340 Z320-EXIT.
014350     EXIT.
014360
014370 Z340-DELETE-REPORT-REQUEST.
014380*********************************************************
014390*
014400*********************************************************
014410*    RPRQ1-RPTREQUESTID ALREADY POPULATED
014420     CALL 'CD1RPRQD' USING RMESSAGE,
014430                           RPRQ1-DATA,
014440                           RTN-OUTPUT-FIELDS.
014450
014460 Z340-EXIT.
014470     EXIT.
014480
014490 Z600-GET-COMPANY-DESCRIPT.
014500*********************************************************
014510*
014520*********************************************************
014530     INITIALIZE RFCD1-DATA.
014540*    CONVERT PIC S9 TO PIC Z9
014550     MOVE WS-HOLD-CO-NUM        TO WS-REF-NUM.
014560     MOVE WS-REF-NUM            TO RFCD1-REF-CD.
014570     MOVE 'TMAP MKEY CO NUM'    TO RFCD1-DOMAIN-NAME.
014580
014590     CALL 'CS1RFCDD' USING RMESSAGE,
014600                           RFCD1-DATA,
014610                           RTN-OUTPUT-FIELDS.
014620     IF RTN-NO-ERROR
014630        MOVE RFCD1-DESCRIPT        TO DETL-COMPANY-DESCRIPT
014640     ELSE
014650        MOVE SPACES                TO DETL-COMPANY-DESCRIPT
014660     END-IF.
014670
014680 Z600-EXIT.
014690     EXIT.
014700
014710 Z700-GET-PRODUCT-DESCRIPT.
014720*********************************************************
014730*
014740*********************************************************
014750     INITIALIZE RFCD1-DATA.
014760*    CONVERT PIC S9 TO PIC Z9
014770     MOVE WS-HOLD-PROD-NUM        TO WS-REF-NUM.
014780     MOVE WS-REF-NUM              TO RFCD1-REF-CD.
014790     MOVE 'TMAP MKEY PROD NUM'    TO RFCD1-DOMAIN-NAME.
014800
014810     CALL 'CS1RFCDD' USING RMESSAGE,
014820                           RFCD1-DATA,
014830                           RTN-OUTPUT-FIELDS.
014840     IF RTN-NO-ERROR
014850        MOVE RFCD1-DESCRIPT        TO DETL-PRODUCT-DESCRIPT
014860     ELSE
014870        MOVE SPACES                TO DETL-PRODUCT-DESCRIPT
014880     END-IF.
014890
014900 Z700-EXIT.
014910     EXIT.
014920
014930 Z800-GET-CASD-DATA.
014940*********************************************************
014950*
014960*********************************************************
014970     INITIALIZE CASD3-DATA.
014980     MOVE WS-HOLD-CASE-KEY        TO CASD3-CASE-KEY.
014990
015000     CALL 'CS5CASDD' USING RMESSAGE,
015010                           CASD3-DATA,
015020                           RTN-OUTPUT-FIELDS.
015030     IF RTN-NO-ERROR
015040        MOVE CASD3-CONT-NO         TO DETL-CONT-NO
015050        MOVE CASD3-CAT-NO          TO DETL-CAT-NO
015060        MOVE CASD3-PLAN-NM         TO DETL-PLAN-NM
015070     ELSE
015080        MOVE SPACES                TO DETL-CONT-NO
015090        MOVE SPACES                TO DETL-CAT-NO
015100        MOVE SPACES                TO DETL-PLAN-NM
015110     END-IF.
015120
015130 Z800-EXIT.
015140     EXIT.
015150
015160**
015170**   E N D     O F    P R O G R A M
015180**
015190**
015200
015210
