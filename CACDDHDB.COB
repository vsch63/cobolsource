000010*Property of FIS©. or its affiliates
000020*all rights reserved. FIS Confidential
000030 IDENTIFICATION DIVISION.
000040 PROGRAM-ID.                CACDDHDB.
000050 AUTHOR.                    BOB BOOHER.
000060 DATE-WRITTEN.              NOV 11, 2004.
000070*****************************************************************
000080*   $Modtime::   10 Oct 2018 10:15:18  $
000090*   $Log::   P:/pvcsproj/DONOTUSE.prj/source/CACDDHDB.COV       $
000100*
000110*   Rev 1.0.1.5   10 Oct 2018 11:45:28   alex
000120*FDM4167
000130*fis copyright
000140*
000150*   Rev 1.0.1.4   16 Jan 2006 08:46:10   robin
000160*FDM3518
000170*Module Impact Analysis - Runstat logic
000180*
000190*   Rev 1.0.1.3   23 Sep 2005 11:41:20   robin
000200*FDM3514
000210*Module Impact Analysis Tool - WS-REVISION awk script fix.
000220*
000230*   Rev 1.0.1.2   19 Sep 2005 18:58:20   robin
000240*FDM3514
000250*Module Impact Analysis Tool
000260*
000270*   Rev 1.0.1.1   11 Nov 2004 15:34:06   bob
000280*OMM0742
000290*new program for real money movements
000300*
000310*****************************************************************
000320**
000330**  PRODUCE NOTIONAL TRANSACT_DETAILS TO ACCOUNT FOR REAL MONEY
000340**  MOVEMENTS DURING THE BILLING PROCESS, THEN CALL
000350**  ACCOUNT FOR REAL MONEY MOVEMENTS PROCESS DEF, ALIAS ACRM
000360**
000370**  DEFINED BY : NOTHING
000380**
000390**  1.) Look for system level tr_code in the new tr_code group.
000400**      If none exists, exit program.  If multiple exists, error.
000410**      If one exists, continue. Use TR_CD from Billing PRDF_KEY
000420**      to retrieve TR_CD (tranch activity 933). If this exists,
000430**      use this as TR_CD for notional TDTL, else use TR_CD from
000440**      TR_CD_GRP
000450**  2.) Block fetch joining DD_HIST and SCED_ACT_KEYS
000460**      (def by num = 19 for CSBL_KEY)
000470**  3.) Loop entries from (2)
000480**        a.) Insert one and only one sched_activity (set flag so
000490**            only one gets inserted in the loop)
000500**              Generate new tr_ref_no
000510**              PRDF_KEY = PRDF of new subprocess.  Use alias
000520**              'ACRM' to retrieve PRDF_KEY
000530**              STAT_CD = 'unprocessed'
000540**              group_scat_key = FDP-SCAT-KEY (this will be used
000550**              in reversals )
000560**              All other  columns should be set to FDP-??? values
000570**        b.) Insert notional TDTL
000580**              SCAT_KEY from (b)
000590**              TR_REF_NO from (b)
000600**              DEF_BY_NUM ==> DDHS
000610**              REF_KEY ==> Primary key from DDHS
000620**              CASE_KEY = FDP-CASE-KEY
000630**              UNALLOC_ACCT_CD : expense account
000640**              No elements of balance
000650**              AMT from DDHS
000660**        End loop
000670**  4.) Call superstub one time to launch new PRDF.
000680**      make sure scat1-data is properly loaded with data from
000690**      3b.
000700**      set up scat1-orig-scat-key for error handling
000710**
000720**  Note:
000730**
000740**  New Process definition will simply have the accounting and
000750**  forward processing expense steps.  These steps should pick up
000760**  the TDTLs inserted in CACDDHSB because you inserted them with
000770**  the appropriate scat_key and tr_ref_no.
000780**
000790**  The reversal program for this is CACDDRVB
000800**
000810*****************************************************************
000820**              M A I N T E N A N C E   L O G                  **
000830**-------------------------------------------------------------**
000840**  PGMMER :   DATE    : PROJECT :  DESCRIPTION                **
000850*****************************************************************
000860**  BOB       11-NOV-04 OMM0742     NEW PROGRAM
000870**  ROBIN     16-JAN-05 FDM3518     ADD LOGIC FOR RUNSTAT
000880*****************************************************************
000890 ENVIRONMENT DIVISION.
000900 CONFIGURATION SECTION.
000910 INPUT-OUTPUT SECTION.

000930 DATA DIVISION.
000940 WORKING-STORAGE SECTION.

000960 01  WS-COMPILED                       PIC X(20).
000970 01  WS-PROG-NM                        PIC X(8) VALUE 'CACDDHDB'.

000990 01  WS-VARIABLES.
001000     05  VDDHX                         PIC S9(4)  COMP.
001010     05  WS-TR-REF-NO                  PIC X(12).
001020     05  WS-SCAT-KEY                   PIC S9(15) COMP-3.
001030     05  WS-PRDF-KEY                   PIC S9(15) COMP-3.
001040     05  WS-TR-CD                      PIC X(4).

001060 01  WS-LOCAL-FLAGS.
001070     05  PROCESS-NOTIONALS-FLAG        PIC X VALUE SPACE.
001080         88 PROCESS-NOTIONALS             VALUE 'Y'.
001090         88 DO-NOT-PROCESS-NOTIONALS      VALUE 'N'.

001110     05 WS-WARN-FLAG                   PIC X VALUE SPACE.
001120        88  WARN-FLAG-YES                VALUE '1'.
001130        88  WARN-FLAG-NO                 VALUE '0'.

001150    05  SUPERSTUB-SETUP-FLAG           PIC X VALUE SPACE.
001160        88  SUPERSTUB-SETUP              VALUE '1'.
001170        88  SUPERSTUB-NOT-SETUP          VALUE '0'.

001190 01 WS-ERROR-MESSAGES.
001200    05 WS-ERR-MSG3.
001210       10 FILLER                        PIC X(24) VALUE
001220          'PROCESS DEFINITION KEY: '.
001230       10 WS-ERR-PRDF-KEY               PIC S9(15).
001240    05 WS-ERR-MSG4.
001250       10 FILLER                        PIC X(27) VALUE
001260          'SCHED ACTIVITY KEY (SCAT): '.
001270       10 WS-ERR-SCAT-KEY               PIC S9(15).
001280    05 WS-ERR-MSG5.
001290       10 FILLER                        PIC X(25) VALUE
001300          'PROCESS DEFINITION NAME: '.
001310       10 WS-ERR-PRDF-NM                PIC X(80).
001320    05 WS-ERR-MSG6.
001330       10 FILLER                        PIC X(11) VALUE
001340          'TR-REF-NO: '.
001350       10 WS-ERR-TR-REF-NO              PIC X(12).

001370**   COPYBOOKS

001390** TR_CODE_GRP/TR_CODE_GRP_DETL
001400     COPY CL8TRGPD.
001410** TRANSACT_DETAILS
001420     COPY CL1TDTLD.
001430** TR_CD_TRNCH
001440     COPY CL1TRTRD.
001450** DD_HIST/DD_MSTR/SCHED_ACT_KEYS
001460     COPY CL4VDDHD.
001470** CASE_UNALLOC_ACCT
001480     COPY CL1CSUAD.
001490** SCHED_ACTIVITY
001500     COPY CL1SCATD.
001510** SCHED_ACT_KEYS
001520     COPY CL1SCAKD.
001530** PROCESS_DEF
001540     COPY CL6PRDFD.
001550** PROCESS FORMS
001560     COPY CLKFORMC.
001570** PROCESS STEPS
001580     COPY CLKSTEPC.
001590** TRANS-SCHED-KEYS
001600     COPY CL2TSCKD.

001620**
001630** LINKAGE SECTION PARAMETERS
001640**
001650**  REVISION AREA **                                              FDM3514
001660 01  WS-REVISION           PIC X(35)  VALUE                       FDM3514
001670     ' $Revision::   1.0.1.5        $'.
001680 LINKAGE SECTION.
001690     COPY CLKFDPNC.
001700     COPY CL1CASDD.
001710     COPY CLKERRBC.

001730******************************************************************
001740**              PROCEDURE DIVISION                              **
001750******************************************************************
001760 PROCEDURE DIVISION USING RMESSAGE,
001770                          CASD1-DATA,
001780                          RTN-OUTPUT-FIELDS.
001790     IF FDP-DEBUG-RUNSTAT > '00'                                  FDM3514
001800        ADD +1  TO RTN-DIMT-CNT                                   FDM3514
001810        MOVE WHEN-COMPILED TO                                     FDM3514
001820             RTN-DIMT-COMPILE-DATE (RTN-DIMT-CNT)                 FDM3514
001830        MOVE WS-REVISION (16:10) TO                               FDM3514
001840             RTN-DIMT-REVISION (RTN-DIMT-CNT)                     FDM3514
001850        MOVE 'CACDDHDB' TO                                        FDM3514
001860           RTN-DIMT-PROGRAM-NAME (RTN-DIMT-CNT)                   FDM3514
001870        CALL 'CUTDIMTD' USING RMESSAGE,                           FDM3514
001880                              RTN-OUTPUT-FIELDS                   FDM3514
001890     END-IF.                                                      FDM3514

001910******************************************************************
001920**                MAIN ROUTINE                                  **
001930******************************************************************
001940 MAIN-ROUTINE.

001960**   INITIALIZE NECESSARY WORKING STORAGE FIELDS

001980     PERFORM A100-INITIALIZE
001990        THRU A100-EXIT.

002010**   EDIT LINKAGE PARAMETERS

002030     PERFORM A200-EDIT-LK-PARMS
002040        THRU A200-EXIT.

002060**   VALIDATE/RETRIEVE SYSTEM NOTIONAL TR-CD

002080     IF RTN-NO-ERROR
002090        PERFORM B220-VALIDATE-REAL-MONEY-TR-CD
002100           THRU B220-EXIT
002110     END-IF.

002130**   PROCESS NOTIONAL TDTL'S AND CALL SUPERSTUB

002150     IF RTN-NO-ERROR AND PROCESS-NOTIONALS
002160        PERFORM B300-PROCESS-NOTIONALS
002170           THRU B300-EXIT
002180     END-IF.
002190**
002200**   END OF PROGRAM
002210**
002220     GOBACK.


002250 MAIN-ROUTINE-EXIT.
002260     EXIT.
002270******************************************************************
002280*   INITIALIZE VARIABLES                                         *
002290******************************************************************
002300 A100-INITIALIZE.

002320     INITIALIZE WS-VARIABLES.
002330     SET SUPERSTUB-NOT-SETUP TO TRUE.

002350 A100-EXIT.
002360     EXIT.
002370******************************************************************
002380*    EDIT LINKAGE FIELDS                                         *
002390******************************************************************
002400 A200-EDIT-LK-PARMS.

002420     IF FDP-CYC-DT = SPACES
002430        MOVE 'UT'                      TO RTN-ERR-PREFIX
002440        MOVE 36                        TO RTN-ERROR-CODE
002450        MOVE WS-PROG-NM                TO RTN-PROG-NAME
002460        MOVE 'A200-EDIT-LK-PARMS'      TO RTN-PARA-NAME
002470        CALL 'CUTSVARB' USING RMESSAGE,
002480                              RTN-OUTPUT-FIELDS
002490     END-IF.
002500*
002510     IF FDP-EFF-DT = SPACES AND RTN-NO-ERROR
002520        MOVE 'UT'                      TO RTN-ERR-PREFIX
002530        MOVE 35                        TO RTN-ERROR-CODE
002540        MOVE WS-PROG-NM                TO RTN-PROG-NAME
002550        MOVE 'A200-EDIT-LK-PARMS'      TO RTN-PARA-NAME
002560        CALL 'CUTSVARB' USING RMESSAGE,
002570                              RTN-OUTPUT-FIELDS
002580     END-IF.
002590*
002600     IF FDP-USER = SPACES AND RTN-NO-ERROR
002610        MOVE 'UT'                      TO RTN-ERR-PREFIX
002620        MOVE 39                        TO RTN-ERROR-CODE
002630        MOVE WS-PROG-NM                TO RTN-PROG-NAME
002640        MOVE 'A200-EDIT-LK-PARMS'      TO RTN-PARA-NAME
002650        CALL 'CUTSVARB' USING RMESSAGE,
002660                              RTN-OUTPUT-FIELDS
002670     END-IF.

002690     IF FDP-SCAT-KEY <= ZERO AND RTN-NO-ERROR
002700        MOVE 'UT'                TO RTN-ERR-PREFIX
002710        MOVE 42                  TO RTN-ERROR-CODE
002720        MOVE WS-PROG-NM          TO RTN-PROG-NAME
002730        MOVE 'A200'              TO RTN-PARA-NAME
002740        CALL 'CUTSVARB' USING    RMESSAGE,
002750                                 RTN-OUTPUT-FIELDS
002760     END-IF.

002780     IF FDP-TR-REF-NO = SPACES AND RTN-NO-ERROR
002790        MOVE 'UT'                TO RTN-ERR-PREFIX
002800        MOVE 34                  TO RTN-ERROR-CODE
002810        MOVE WS-PROG-NM          TO RTN-PROG-NAME
002820        MOVE 'A200'              TO RTN-PARA-NAME
002830        CALL 'CUTSVARB' USING    RMESSAGE,
002840                                 RTN-OUTPUT-FIELDS
002850     END-IF.

002870 A200-EXIT.
002880     EXIT.


002910 B220-VALIDATE-REAL-MONEY-TR-CD.

002930*    RETRIEVE TR-CD FOR REAL MONEY MOVEMENTS ACCTG DEFAULT

002950     SET DO-NOT-PROCESS-NOTIONALS TO TRUE.
002960     INITIALIZE TRGP8-DATA.
002970     MOVE 'I2'  TO TRGP8-GRP-TYP-CD (1).
002980     CALL 'CB2TRGDD' USING RMESSAGE,
002990                           TRGP8-DATA,
003000                           RTN-OUTPUT-FIELDS.
003010     IF RTN-NO-ERROR
003020        IF TRGP8-ROWS-FOUND = 0
003030*          NO ERROR, NOTIONALS WILL NOT BE PROCESSED
003040           CONTINUE
003050        ELSE
003060           IF TRGP8-ROWS-FOUND = 1
003070*             SET FLAG AND TR-CD FOR NOTIONAL PROCESSING
003080              SET PROCESS-NOTIONALS TO TRUE
003090           ELSE
003100*             ERROR >1 REAL MONEY MOVEMENT ACCTG DEFAULT
003110              MOVE WS-PROG-NM     TO RTN-PROG-NAME
003120              MOVE 'B220'         TO RTN-PARA-NAME
003130              MOVE 'TX'           TO RTN-ERR-PREFIX
003140              MOVE 3033           TO RTN-ERROR-CODE
003150              CALL 'CUTSVARB' USING RMESSAGE,
003160                                    RTN-OUTPUT-FIELDS
003170           END-IF
003180        END-IF
003190     END-IF.

003210 B220-EXIT.
003220     EXIT.

003240 B300-PROCESS-NOTIONALS.

003260*    SAVE FDP-TR-REF-NO AND FDP-SCAT-KEY, THEY WILL BE
003270*    CHANGED BELOW.  ALSO PRDF-KEY

003290     MOVE FDP-TR-REF-NO     TO WS-TR-REF-NO.
003300     MOVE FDP-SCAT-KEY      TO WS-SCAT-KEY.
003310     MOVE FDP-PRDF-KEY      TO WS-PRDF-KEY.

003330     INITIALIZE VDDH4-DATA.
003340     MOVE FDP-SCAT-KEY      TO VDDH4-SCAT-KEY   (1).
003350*    19 CSBL_KEY
003360     MOVE +19               TO VDDH4-DEF-BY-NUM (1).
003370     MOVE FDP-EFF-DT        TO VDDH4-EFF-DT     (1).
003380     CALL 'CB3VDDHD' USING  RMESSAGE,
003390                            VDDH4-DATA,
003400                            RTN-OUTPUT-FIELDS.

003420*    NOTE : MOST OF THE TIME S/B ONLY ONE RECORD

003440     PERFORM VARYING VDDHX FROM 1 BY 1
003450         UNTIL VDDHX > VDDH4-ROWS-FOUND OR RTN-ERROR
003460         PERFORM B400-INSERT-TDTL-CALL-PROCESS
003470            THRU B400-EXIT
003480     END-PERFORM.

003500*    PUT BACK FDP-VARS

003520     MOVE WS-TR-REF-NO      TO FDP-TR-REF-NO.
003530     MOVE WS-SCAT-KEY       TO FDP-SCAT-KEY.
003540     MOVE WS-PRDF-KEY       TO FDP-PRDF-KEY.

003560 B300-EXIT.
003570     EXIT.

003590 B400-INSERT-TDTL-CALL-PROCESS.

003610*    FIRST VALIDATE THAT WE HAVE TR_CD TRNCH FROM THE BGRL LINKED
003620*    TO THE CASE BILL/DIRECT DEBIT READ.  ERROR IF NONE FOUND.
003630*    OK IF NO REAL MONEY ACTIVITY TRANCHE LINKED TO IT, BUT THE
003640*    TR_CD MUST BE THERE

003660     PERFORM C010-VALIDATE-BGRL-TR-CD
003670        THRU C010-EXIT

003690     IF RTN-NO-ERROR
003700        INITIALIZE TDTL1-DATA

003720*       TR-CD DERIVED IN C010
003730        MOVE WS-TR-CD      TO TDTL1-TR-CD

003750*       GET UA CODE OF TYPE EXP ACCT (WILL BE 0 OR 1 FOUND)

003770        PERFORM C020-GET-UA-CD
003780           THRU C020-EXIT
003790     END-IF.

003810*    GENERATE NEW FDP-TR-REF-NO (ORIGINAL ALREADY SAVED)

003830     IF RTN-NO-ERROR
003840        PERFORM C030-FDP-TR-REF-NO
003850           THRU C030-EXIT
003860     END-IF.

003880*    CREATE SCHED-ACTIVITY FOR SUPERSTUB CALL AND USE
003890*    NEW SCAT-KEY IN TDTL INSERT

003910     IF RTN-NO-ERROR
003920        PERFORM C040-INSERT-SCHED-ACTIVITY
003930           THRU C040-EXIT
003940     END-IF.

003960*    INSERT NOTIONAL TRANSACT_DETAIL

003980     IF RTN-NO-ERROR
003990        PERFORM C050-INSERT-NOTIONAL-TDTL
004000           THRU C050-EXIT
004010     END-IF.

004030*    TDTL INSERTED, NOW PREPARE SUPERSTUB CALL

004050     IF RTN-NO-ERROR AND SUPERSTUB-NOT-SETUP
004060        PERFORM C500-SETUP-SUPERSTUB-CALL
004070           THRU C500-EXIT
004080     END-IF.

004100*    CALL SUPERSTUB

004120     IF RTN-NO-ERROR
004130        PERFORM C600-CALL-SUPERSTUB
004140           THRU C600-EXIT
004150     END-IF.

004170 B400-EXIT.
004180     EXIT.

004200 C010-VALIDATE-BGRL-TR-CD.

004220     IF VDDH4-TR-CD (VDDHX) = SPACES
004230**      TX 3035 NO TR-CD ON BGRL,REAL MONEY MOVMT CAN'T PROCESS
004240**      THIS IS A HARD ERROR
004250        MOVE WS-PROG-NM                 TO RTN-PROG-NAME
004260        MOVE 'C010-VALIDATE-BGRL-TR-CD' TO RTN-PARA-NAME
004270        MOVE 'TX'                       TO RTN-ERR-PREFIX
004280        MOVE 3035                       TO RTN-ERROR-CODE
004290        CALL 'CUTSVARB' USING RMESSAGE,
004300                              RTN-OUTPUT-FIELDS
004310     ELSE
004320**      USE TR-CD TO LOOKUP ACITIVTY NUM/TRANCHE, IF FOUND, THE
004330**      LINKED TR-CD WILL BE USED FOR THE TDTL, ELSE USE THE DFLT

004350        INITIALIZE TRTR1-DATA
004360        MOVE VDDH4-TR-CD (VDDHX)   TO TRTR1-TR-CD
004370*       933' REAL MONEY MOVEMENT ACCOUNTING
004380        MOVE 933           TO TRTR1-ACTIVITY-NUM
004390        CALL 'CS1TRTRD' USING RMESSAGE,
004400                              TRTR1-DATA,
004410                              RTN-OUTPUT-FIELDS
004420        IF RTN-NO-ERROR
004430           IF TRTR1-ROWS-FOUND = 1
004440              MOVE TRTR1-TDTL-TR-CD  TO WS-TR-CD
004450           ELSE
004460*             USE DEFAULT VALUE AND PRINT INFORMATIONAL MSG
004470              MOVE TRGP8-TR-CD (1)   TO WS-TR-CD
004480**            TX 3034 DEFAULT REAL MONEY TR-CD USED
004490**            THIS IS INFORMATIONAL
004500              MOVE WS-PROG-NM                 TO RTN-PROG-NAME
004510              MOVE 'C010-VALIDATE-BGRL-TR-CD' TO RTN-PARA-NAME
004520              MOVE 'TX'                       TO RTN-ERR-PREFIX
004530              MOVE 3034                       TO RTN-ERROR-CODE
004540              CALL 'CUTSVARB' USING RMESSAGE,
004550                                    RTN-OUTPUT-FIELDS
004560           END-IF
004570        END-IF
004580     END-IF.

004600 C010-EXIT.
004610     EXIT.

004630 C020-GET-UA-CD.

004650     INITIALIZE CSUA1-DATA.
004660**   '1' USED FOR REALLOCATION/SHORTFALL OF MEMBER'S MONEY
004670     MOVE '1'           TO CSUA1-EXP-ACCT-CD.
004680     CALL 'CS1CSUAD' USING RMESSAGE,
004690                           CSUA1-DATA,
004700                           RTN-OUTPUT-FIELDS.
004710     IF RTN-NO-ERROR
004720*       IF NONE WAS FOUND, WILL RETAIN BLANK/NULL
004730        MOVE CSUA1-UNALLOC-ACCT-CD TO TDTL1-UNALLOC-ACCT-CD
004740     END-IF.

004760 C020-EXIT.
004770     EXIT.

004790 C030-FDP-TR-REF-NO.

004810*   GENERATING NEW FDP-TR-REF-NO WILL KEEP EXPENSE/ACCTING DONE
004820*   HERE SEPARATE FROM BILLING PROCESS

004840*   PASS IN ORIGINAL TR-REF-NO, WHICH DAL WILL USE AS GRP_TR_REF_N
004850*   AND WILL RETURN NEW FDP-TR-REF-NO

004870    MOVE WS-TR-REF-NO  TO FDP-TR-REF-NO.

004890    CALL 'CS3TREFD' USING RMESSAGE,
004900                          RTN-OUTPUT-FIELDS.

004920    IF RTN-NO-ERROR
004930       MOVE FDP-TR-REF-NO TO TDTL1-TR-REF-NO
004940    END-IF.

004960 C030-EXIT.
004970     EXIT.

004990 C040-INSERT-SCHED-ACTIVITY.

005010     INITIALIZE SCAT1-DATA
005020                SCAK1-DATA
005030                PRDF6-DATA.

005050*    GET PRDF-KEY FOR ACCOUNT FOR REAL MONEY MOVEMENTS ACRM

005070     MOVE 'ACRM'        TO PRDF6-ALIAS-NM.

005090     CALL 'CS6PRDFD'    USING RMESSAGE,
005100                              PRDF6-DATA,
005110                              RTN-OUTPUT-FIELDS.
005120     IF RTN-NO-ERROR
005130        MOVE PRDF6-PRDF-KEY         TO SCAT1-PRDF-KEY
005140*       NEW FDP-TR-REF-NO GENERATED ALREADY
005150        MOVE FDP-TR-REF-NO          TO SCAT1-TR-REF-NO
005160        MOVE FDP-EFF-DT             TO SCAT1-EFF-DT,
005170                                       SCAT1-DUE-DT,
005180                                       SCAT1-TX-DT
005190        MOVE VDDH4-CASE-KEY(VDDHX)  TO SCAT1-CASE-KEY
005200        MOVE ZEROES                 TO SCAT1-CASE-MBR-KEY,
005210                                       SCAT1-TSCH-KEY
005220        MOVE ZERO                   TO SCAT1-BJLG-KEY
005230        MOVE FDP-CYC-DT             TO SCAT1-CYC-DT
005240        MOVE '0'                    TO SCAT1-STAT-CD
005250*       USE THE ORIGINAL SCAT-KEY SAVED IN WS-SCAT-KEY
005260        MOVE WS-SCAT-KEY            TO SCAT1-ORIG-SCAT-KEY,
005270                                       SCAT1-GRP-SCAT-KEY
005280        MOVE FDP-BOOK-DT            TO SCAT1-BOOK-DT
005290        MOVE ZEROES                 TO SCAT1-CSRL-KEY,
005300                                       SCAT1-BUNDLE-ID
005310        MOVE FDP-CYC-DT             TO SCAT1-PROC-DT
005320        MOVE SPACES                 TO SCAT1-UNALLOC-ACCT-CD,
005330                                       SCAT1-BEN-CAT-NO

005350        CALL 'CI3SCATD' USING RMESSAGE,
005360                              SCAT1-DATA,
005370                              RTN-OUTPUT-FIELDS
005380     END-IF.

005400     IF RTN-NO-ERROR
005410        MOVE SCAT1-SCAT-KEY           TO SCAK1-SCAT-KEY
005420        MOVE 1                        TO SCAK1-SEQ-NUM
005430*       248 RE-PRESENTED DIRECT DEBIT DDHS_KEY
005440        MOVE +248                     TO SCAK1-DEF-BY-NUM
005450        MOVE VDDH4-DDHS-KEY (VDDHX)   TO SCAK1-REF-KEY
005460        CALL 'CI1SCAKD' USING RMESSAGE,
005470                              SCAK1-DATA,
005480                              RTN-OUTPUT-FIELDS
005490     END-IF.

005510 C040-EXIT.
005520     EXIT.

005540 C050-INSERT-NOTIONAL-TDTL.
005550*    INITIALIZE AND SETTING OF SOME VARS HAS ALREADY OCCURRED

005570     MOVE VDDH4-AMT(VDDHX)           TO TDTL1-AMT.
005580     MOVE ZEROES                     TO TDTL1-PRNC-AMT.
005590     MOVE FDP-EFF-DT                 TO TDTL1-DUE-DT.
005600     MOVE FDP-EFF-DT                 TO TDTL1-EFF-DT.
005610     MOVE '0'                        TO TDTL1-INT-INCL-CD.
005620*    2 = NOTIONAL
005630     MOVE '2'                        TO TDTL1-STAT-CD.
005640*    LEAVE CASE-MBR-KEY EMPTY
005650     MOVE ZEROES                     TO TDTL1-CASE-MBR-KEY.
005660*    NO ELEMENTS OF BALANCE
005670     MOVE ZEROES                     TO TDTL1-UNIT-CT,
005680                                        TDTL1-BKT-NO,
005690                                        TDTL1-FD-NO,
005700                                        TDTL1-CHRL-KEY,
005710                                        TDTL1-FDRT-KEY,
005720                                        TDTL1-TRNCH-KEY,
005730                                        TDTL1-ORIG-SCAT-KEY,
005740                                        TDTL1-MNY-TYP-NO.
005750     MOVE SPACES                     TO TDTL1-RVSL-CYC-DT,
005760                                        TDTL1-MNY-CAT-NUM,
005770                                        TDTL1-FD-DESC-ID.
005780*    USE SCAT-KEY JUST CREATED SO THIS WILL BE TREATED SEPARATE
005790*    FROM BILLING SCAT-KEY
005800     MOVE SCAT1-SCAT-KEY             TO TDTL1-SCAT-KEY.
005810     MOVE VDDH4-CASE-KEY(VDDHX)      TO TDTL1-CASE-KEY.
005820     MOVE FDP-EFF-DT                 TO TDTL1-TX-DT.
005830     MOVE FDP-CYC-DT                 TO TDTL1-BOOK-DT,
005840                                        TDTL1-PROC-DT.
005850*    248 RE-PRESENTED DIRECT DEBIT (DDHS_KEY)
005860     MOVE VDDH4-DDHS-KEY(VDDHX)      TO TDTL1-REF-KEY.
005870     MOVE +248                       TO TDTL1-DEF-BY-NUM.

005890     CALL 'CI2TDTLD' USING RMESSAGE,
005900                           TDTL1-DATA,
005910                           RTN-OUTPUT-FIELDS.

005930 C050-EXIT.
005940     EXIT.

005960**--------------------------------------------------------------**
005970**  Call CS1GNSTD to validate that the steps in the process are **
005980**  COBOL programs.                                             **
005990**--------------------------------------------------------------**
006000 C500-SETUP-SUPERSTUB-CALL.

006020     INITIALIZE FORMMSG.

006040     MOVE PRDF6-PRDF-KEY   TO FRM-PRDF-KEY,
006050                              FDP-PRDF-KEY.
006060     MOVE FDP-USER         TO FRM-USER.

006080     MOVE SCAT1-SCAT-KEY   TO FRM-SCAT-KEY.
006090     IF FDP-DEBUG > 0
006100        MOVE '1' TO FRM-DEBUG (2:1)                               FDM3518
006110     ELSE
006120         MOVE '0' TO FRM-DEBUG (2:1)                              FDM3518
006130     END-IF.
006140
006150     IF FDP-RUNSTAT > 0                                           FDM3518
006160        MOVE '1' TO FRM-DEBUG (1:1)                               FDM3518
006170     ELSE                                                         FDM3518
006180        MOVE '0' TO FRM-DEBUG (1:1)                               FDM3518
006190     END-IF.                                                      FDM3518

006210     CALL 'CS1GNSTD' USING RMESSAGE,
006220                           FORMMSG,
006230                           RTN-OUTPUT-FIELDS.
006240     IF RTN-NO-ERROR
006250*       SET FLAG SO THIS PARAGRAPH WON'T BE CALLED AGAIN
006260        SET SUPERSTUB-SETUP TO TRUE
006270     END-IF.

006290 C500-EXIT.
006300     EXIT.

006320 C600-CALL-SUPERSTUB.

006340     INITIALIZE STEP-DATA,
006350                WS-ERR-PRDF-KEY,
006360                WS-ERR-PRDF-NM,
006370                WS-ERR-TR-REF-NO.


006400**   DO NOT COMMIT DATA
006410     MOVE 'NC' TO STEP-COMMIT-FLAG OF STEP-DATA.

006430* SCAT1-LAUNCHED-BY-CD INDICATES WHERE COMPASS WAS ORIGINALLY
006440* KICKED OFF FROM. FDP-LAUNCHED-BY-CD HAS THIS INFO. MOVE THE CD
006450* SO THAT THE CD GETS PASSED THRU
006460     MOVE FDP-LAUNCHED-BY-CD     TO SCAT1-LAUNCHED-BY-CD.
006470     MOVE 'Y'                    TO SCAT1-DB-VALIDATED-CD.

006490     CALL 'CSTCMPST' USING FORMMSG,
006500                           SCAT1-DATA,
006510                           STEP-DATA.

006530**   FRM-ERR-CT = 2  04/FATAL ERROR
006540     IF FRM-ERR-CT = 2
006550**      FORCE HARD ERROR
006560        SET RTN-PROG-ERROR       TO TRUE
006570        MOVE 3                   TO FDP-ERR-CT
006580        MOVE PRDF6-PRDF-KEY      TO WS-ERR-PRDF-KEY
006590        MOVE PRDF6-NM            TO WS-ERR-PRDF-NM
006600**      SCAT1-TR-REF-NO HOLDS THE SUB PROCESS TR-REF-NO
006610        MOVE SCAT1-TR-REF-NO     TO WS-ERR-TR-REF-NO
006620**      804 FATAL SQL ERROR IN CALLED PROGRAM
006630        MOVE 804                 TO RTN-ERROR-CODE
006640        MOVE 'TX'                TO RTN-ERR-PREFIX
006650        MOVE WS-PROG-NM          TO RTN-PROG-NAME
006660        MOVE 'C600-CALL-SUPERSTUB'
006670                                 TO RTN-PARA-NAME
006680        MOVE WS-ERR-MSG3         TO RTN-VAR-TEXT   (1)
006690        MOVE 1                   TO RTN-VAR-SEQ-NO (1)
006700        MOVE WS-ERR-MSG5         TO RTN-VAR-TEXT   (2)
006710        MOVE 2                   TO RTN-VAR-SEQ-NO (2)
006720        MOVE WS-ERR-MSG6         TO RTN-VAR-TEXT   (3)
006730        MOVE 3                   TO RTN-VAR-SEQ-NO (3)
006740        MOVE 3                   TO RTN-VAR-DETL-CNT
006750        CALL 'CUTSVARB'       USING RMESSAGE,
006760                                    RTN-OUTPUT-FIELDS
006770*       exit paragraph
006780        GO TO C600-EXIT
006790     END-IF.

006810**   FRM-ERR-CT = 1  02/ERROR PROCESS HALTED
006820**   TX #805  MSG WILL INFORM THE USER THAT AN ERROR
006830**   OCCURED DURING THE PROCESSING OF THE 'SUB PROCESS'.
006840**   SYS VAR DETLS DISPLAYS THE SUB PROCESS NAME AND TR REF NO

006860     IF FRM-ERR-CT  = 1
006870        MOVE PRDF6-PRDF-KEY    TO WS-ERR-PRDF-KEY
006880        MOVE PRDF6-NM          TO WS-ERR-PRDF-NM
006890**      SCAT1-TR-REF-NO HOLDS THE SUB PROCESS TR REF-NO
006900        MOVE SCAT1-TR-REF-NO   TO WS-ERR-TR-REF-NO
006910**      FORCE HARD ERROR
006920        SET RTN-PROG-ERROR     TO TRUE
006930        MOVE 805               TO RTN-ERROR-CODE
006940        MOVE 'TX'              TO RTN-ERR-PREFIX
006950        MOVE WS-PROG-NM        TO RTN-PROG-NAME
006960        MOVE 'C600-CALL-SUPERSTUB'
006970                               TO RTN-PARA-NAME
006980        MOVE WS-ERR-MSG3       TO RTN-VAR-TEXT   (1)
006990        MOVE 1                 TO RTN-VAR-SEQ-NO (1)
007000        MOVE WS-ERR-MSG5       TO RTN-VAR-TEXT   (2)
007010        MOVE 2                 TO RTN-VAR-SEQ-NO (2)
007020        MOVE WS-ERR-MSG6       TO RTN-VAR-TEXT   (3)
007030        MOVE 3                 TO RTN-VAR-SEQ-NO (3)
007040        MOVE 3                 TO RTN-VAR-DETL-CNT
007050        CALL 'CUTSVARB'     USING RMESSAGE,
007060                                  RTN-OUTPUT-FIELDS
007070**      exit paragraph
007080        GO TO C600-EXIT
007090     END-IF.

007110**   IF FRM-WARN-CT > 0 INFORMATION OR WARNING
007120**   MESSAGE(S) WERE INSERTED.  CONTINUE PROCESSING
007130     IF FRM-WARN-CT > 0

007150        SET RTN-NO-ERROR TO TRUE
007160        MOVE ZEROS        TO  FDP-ERR-CT
007170        MOVE FRM-WARN-CT  TO  FDP-WARN-CT

007190        IF FDP-DEBUG > 0
007200           DISPLAY 'CACDDHDB - INFORMATION OR WARNING ',
007210           DISPLAY 'MESSAGE(S) RETURNED FROM CALL TO SUPERSTUB'
007220        END-IF
007230*       EXIT PARAGRAPH
007240        GO TO C600-EXIT
007250     END-IF.

007270*    CLEAN RETURN FROM SUPERSTUB
007280     IF FRM-WARN-CT = 0  AND
007290        FRM-ERR-CT  = 0
007300        SET RTN-NO-ERROR TO TRUE
007310        MOVE ZEROS TO FDP-ERR-CT
007320        MOVE ZEROS TO FDP-WARN-CT
007330     END-IF.

007350 C600-EXIT.
007360     EXIT.


