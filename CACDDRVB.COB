000010*Property of FIS©. or its affiliates
000020*all rights reserved. FIS Confidential
000030 IDENTIFICATION DIVISION.
000040 PROGRAM-ID.                CACDDRVB.
000050 AUTHOR.                    BOB BOOHER.
000060 DATE-WRITTEN.              NOV 11, 2004.
000070*****************************************************************
000080*   $Modtime::   10 Oct 2018 10:15:18  $
000090*   $Log::   P:/pvcsproj/DONOTUSE.prj/source/CACDDRVB.COV       $
000100*
000110*   Rev 1.0.1.6   10 Oct 2018 11:45:28   alex
000120*FDM4167
000130*fis copyright
000140*
000150*   Rev 1.0.1.5   16 Jan 2006 08:46:12   robin
000160*FDM3518
000170*Module Impact Analysis - Runstat logic
000180*
000190*   Rev 1.0.1.4   23 Sep 2005 11:41:22   robin
000200*FDM3514
000210*Module Impact Analysis Tool - WS-REVISION awk script fix.
000220*
000230*   Rev 1.0.1.3   19 Sep 2005 18:58:20   robin
000240*FDM3514
000250*Module Impact Analysis Tool
000260*
000270*   Rev 1.0.1.2   15 Nov 2004 11:57:42   bob
000280*OMM0742
000290*rows found for scat1-data
000300*
000310*   Rev 1.0.1.1   11 Nov 2004 15:33:58   bob
000320*OMM0742
000330*new program for real money movements
000340*
000350*****************************************************************
000360**
000370**  REVERSE TRANSACT_DETAILS CREATED BY BILLING STEP CACDDHDB,
000380**  WHICH HAVE DIFFERENT SCAT_KEYS/TR_REF_NO'S FROM THOSE IN
000390**  THE REST OF THE BILLING PROCESS
000400**
000410**
000420**  DEFINED BY : CSBL_KEY
000430**
000440**  1.)  Look for tr_code in the new tr_code group.  If none
000450**       exists, exit program.  If multiple exists, error.  If one
000460**       exists, continue
000470**  2.)  Select sched_activity created in CACDDHDB
000480**       Select * from sched_activity where grp_scat_key =
000490**       SCAT_KEY from CASE_BILLS (original scat-key)
000500**  3.)  Block fetch TDTLs (from 1)
000510**       Select * from transact_details where scat_key = scat_key
000520**  4.)  Loop TDTLs
000530**          Update stat_cd = 'reversed'
000540**          rvsl_cyc_dt    =  fdp-cyc-dt
000550**          orig_scat_key  = scat_key of the tdtl you are updating
000560**          scat_key       = scat_key from loop
000570**       End loop
000580**  5.)  Update the prdf_key on sched_activity from (2) to the new
000590**       reversal prdf_key.  Again, use alias to get prdf_key
000600**       Also update the status to 'unprocessed'.  Make sure you
000610**       overlay scat1- linkage loaded in (2) with these new values.
000620**  6.)  Call superstub to launch the the new reversal process
000630**       definition make sure scat1-data is properly loaded with
000640**       data from 3b make sure you pass in "NC" flag no no commit
000650**       hapens in the sub process.  also make sure you set up
000660**       scat1-orig-scat-key for error handling
000670**
000680**  Note:
000690**
000700**       New reversal process definition will simply have the
000710**       accounting step andthe reversal expense steps.  These
000720**       steps should pick up the TDTLs updated in  CACDDRVB.
000730**
000740**
000750*****************************************************************
000760**              M A I N T E N A N C E   L O G                  **
000770**-------------------------------------------------------------**
000780**  PGMMER :   DATE    : PROJECT :  DESCRIPTION                **
000790*****************************************************************
000800**  BOB       11-NOV-04 OMM0742     NEW PROGRAM
000810**  BOB       15-NOV-04 OMM0742     CHANGE IF/END-IF FOR SCAT1
000820**  ROBIN     16-JAN-06 FDM3518     ADDED RUNSTAT LOGIC.
000830*****************************************************************
000840 ENVIRONMENT DIVISION.
000850 CONFIGURATION SECTION.
000860 INPUT-OUTPUT SECTION.
000870
000880 DATA DIVISION.
000890 WORKING-STORAGE SECTION.
000900
000910 01  WS-COMPILED                       PIC X(20).
000920 01  WS-PROG-NM                        PIC X(8) VALUE 'CACDDRVB'.
000930
000940 01  WS-VARIABLES.
000950     05  WS-SYSTEM-DEFAULT-TR-CD       PIC X(4).
000960     05  VDDHX                         PIC S9(4)  COMP.
000970     05  WS-TR-REF-NO                  PIC X(12).
000980     05  WS-SCAT-KEY                   PIC S9(15) COMP-3.
000990     05  WS-PRDF-KEY                   PIC S9(15) COMP-3.
001000
001010 01  WS-LOCAL-FLAGS.
001020     05  SYSTEM-TR-CD-FLAG        PIC X VALUE SPACE.
001030         88 SYSTEM-TR-CD-FOUND            VALUE 'Y'.
001040         88 SYSTEM-TR-CD-NOT-FOUND        VALUE 'N'.
001050
001060     05 WS-WARN-FLAG                   PIC X VALUE SPACE.
001070        88  WARN-FLAG-YES                 VALUE '1'.
001080        88  WARN-FLAG-NO                  VALUE '0'.
001090
001100     05 TDTLS-FOUND-FLAG               PIC X VALUE SPACE.
001110        88  TDTLS-FOUND                   VALUE '1'.
001120        88  TDTLS-NOT-FOUND               VALUE '0'.
001130
001140    05  SUPERSTUB-SETUP-FLAG           PIC X VALUE SPACE.
001150        88  SUPERSTUB-SETUP              VALUE '1'.
001160        88  SUPERSTUB-NOT-SETUP          VALUE '0'.
001170
001180 01 WS-ERROR-MESSAGES.
001190    05 WS-ERR-MSG3.
001200       10 FILLER                        PIC X(24) VALUE
001210          'PROCESS DEFINITION KEY: '.
001220       10 WS-ERR-PRDF-KEY               PIC S9(15).
001230    05 WS-ERR-MSG4.
001240       10 FILLER                        PIC X(27) VALUE
001250          'SCHED ACTIVITY KEY (SCAT): '.
001260       10 WS-ERR-SCAT-KEY               PIC S9(15).
001270    05 WS-ERR-MSG5.
001280       10 FILLER                        PIC X(25) VALUE
001290          'PROCESS DEFINITION NAME: '.
001300       10 WS-ERR-PRDF-NM                PIC X(80).
001310    05 WS-ERR-MSG6.
001320       10 FILLER                        PIC X(11) VALUE
001330          'TR-REF-NO: '.
001340       10 WS-ERR-TR-REF-NO              PIC X(12).
001350
001360**   COPYBOOKS
001370
001380** TR_CODE_GRP/TR_CODE_GRP_DETL
001390     COPY CL8TRGPD.
001400** TRANSACT_DETAILS
001410     COPY CL1TDTLD.
001420** TR_CD_TRNCH
001430     COPY CL1TRTRD.
001440** SCHED_ACTIVITY
001450     COPY CL1SCATD.
001460** CASE_BILLS
001470     COPY CL7CSBLD.
001480     COPY CL1CSBLD.
001490** PROCESS_DEF
001500     COPY CL6PRDFD.
001510** PROCESS FORMS
001520     COPY CLKFORMC.
001530** PROCESS STEPS
001540     COPY CLKSTEPC.
001550
001560**
001570** LINKAGE SECTION PARAMETERS
001580**
001590**  REVISION AREA **                                              FDM3514
001600 01  WS-REVISION           PIC X(35)  VALUE                       FDM3514
001610     ' $Revision::   1.0.1.6        $'.
001620 LINKAGE SECTION.
001630     COPY CLKFDPNC.
001640     COPY CL1CASDD.
001650     COPY CLKERRBC.
001660
001670******************************************************************
001680**              PROCEDURE DIVISION                              **
001690******************************************************************
001700 PROCEDURE DIVISION USING RMESSAGE,
001710                          CASD1-DATA,
001720                          RTN-OUTPUT-FIELDS.
001730     IF FDP-DEBUG-RUNSTAT > '00'                                  FDM3514
001740        ADD +1  TO RTN-DIMT-CNT                                   FDM3514
001750        MOVE WHEN-COMPILED TO                                     FDM3514
001760             RTN-DIMT-COMPILE-DATE (RTN-DIMT-CNT)                 FDM3514
001770        MOVE WS-REVISION (16:10) TO                               FDM3514
001780             RTN-DIMT-REVISION (RTN-DIMT-CNT)                     FDM3514
001790        MOVE 'CACDDRVB' TO                                        FDM3514
001800           RTN-DIMT-PROGRAM-NAME (RTN-DIMT-CNT)                   FDM3514
001810        CALL 'CUTDIMTD' USING RMESSAGE,                           FDM3514
001820                              RTN-OUTPUT-FIELDS                   FDM3514
001830     END-IF.                                                      FDM3514
001840
001850******************************************************************
001860**                MAIN ROUTINE                                  **
001870******************************************************************
001880 MAIN-ROUTINE.
001890
001900**   INITIALIZE NECESSARY WORKING STORAGE FIELDS
001910
001920     PERFORM A100-INITIALIZE
001930        THRU A100-EXIT.
001940
001950**   EDIT LINKAGE PARAMETERS
001960
001970     PERFORM A200-EDIT-LK-PARMS
001980        THRU A200-EXIT.
001990
002000**   VALIDATE/RETRIEVE SYSTEM NOTIONAL TR-CD
002010
002020     IF RTN-NO-ERROR
002030        PERFORM B320-VALIDATE-REAL-MONEY-TR-CD
002040           THRU B320-EXIT
002050     END-IF.
002060
002070**   PROCEED ONLY IF WE FOUND THE TR-CD IN B320
002080
002090     IF RTN-NO-ERROR AND SYSTEM-TR-CD-FOUND
002100        PERFORM B300-MAIN-ROUTINE
002110           THRU B300-EXIT
002120     END-IF.
002130
002140**
002150**   END OF PROGRAM
002160**
002170     GOBACK.
002180
002190
002200 MAIN-ROUTINE-EXIT.
002210     EXIT.
002220******************************************************************
002230*   INITIALIZE VARIABLES                                         *
002240******************************************************************
002250 A100-INITIALIZE.
002260
002270     INITIALIZE WS-VARIABLES.
002280     SET SUPERSTUB-NOT-SETUP TO TRUE.
002290
002300 A100-EXIT.
002310     EXIT.
002320******************************************************************
002330*    EDIT LINKAGE FIELDS                                         *
002340******************************************************************
002350 A200-EDIT-LK-PARMS.
002360
002370     IF FDP-CYC-DT = SPACES
002380        MOVE 'UT'                      TO RTN-ERR-PREFIX
002390        MOVE 36                        TO RTN-ERROR-CODE
002400        MOVE WS-PROG-NM                TO RTN-PROG-NAME
002410        MOVE 'A200-EDIT-LK-PARMS'      TO RTN-PARA-NAME
002420        CALL 'CUTSVARB' USING RMESSAGE,
002430                              RTN-OUTPUT-FIELDS
002440     END-IF.
002450*
002460     IF FDP-EFF-DT = SPACES AND RTN-NO-ERROR
002470        MOVE 'UT'                      TO RTN-ERR-PREFIX
002480        MOVE 35                        TO RTN-ERROR-CODE
002490        MOVE WS-PROG-NM                TO RTN-PROG-NAME
002500        MOVE 'A200-EDIT-LK-PARMS'      TO RTN-PARA-NAME
002510        CALL 'CUTSVARB' USING RMESSAGE,
002520                              RTN-OUTPUT-FIELDS
002530     END-IF.
002540*
002550     IF FDP-USER = SPACES AND RTN-NO-ERROR
002560        MOVE 'UT'                      TO RTN-ERR-PREFIX
002570        MOVE 39                        TO RTN-ERROR-CODE
002580        MOVE WS-PROG-NM                TO RTN-PROG-NAME
002590        MOVE 'A200-EDIT-LK-PARMS'      TO RTN-PARA-NAME
002600        CALL 'CUTSVARB' USING RMESSAGE,
002610                              RTN-OUTPUT-FIELDS
002620     END-IF.
002630
002640     IF FDP-REF-KEY <= ZERO  AND RTN-NO-ERROR
002650        MOVE 'UT'                      TO RTN-ERR-PREFIX
002660        MOVE 32                        TO RTN-ERROR-CODE
002670        MOVE WS-PROG-NM                TO RTN-PROG-NAME
002680        MOVE 'A200-EDIT-LK-PARMS'      TO RTN-PARA-NAME
002690        CALL 'CUTSVARB' USING RMESSAGE,
002700                              RTN-OUTPUT-FIELDS
002710     END-IF.
002720
002730     IF FDP-SCAT-KEY <= ZERO AND RTN-NO-ERROR
002740        MOVE 'UT'                TO RTN-ERR-PREFIX
002750        MOVE 42                  TO RTN-ERROR-CODE
002760        MOVE WS-PROG-NM          TO RTN-PROG-NAME
002770        MOVE 'A200'              TO RTN-PARA-NAME
002780        CALL 'CUTSVARB' USING    RMESSAGE,
002790                                 RTN-OUTPUT-FIELDS
002800     END-IF.
002810
002820     IF FDP-TR-REF-NO = SPACES AND RTN-NO-ERROR
002830        MOVE 'UT'                TO RTN-ERR-PREFIX
002840        MOVE 34                  TO RTN-ERROR-CODE
002850        MOVE WS-PROG-NM          TO RTN-PROG-NAME
002860        MOVE 'A200'              TO RTN-PARA-NAME
002870        CALL 'CUTSVARB' USING    RMESSAGE,
002880                                 RTN-OUTPUT-FIELDS
002890     END-IF.
002900
002910 A200-EXIT.
002920     EXIT.
002930
002940 B300-MAIN-ROUTINE.
002950
002960
002970*    SAVE FDP-TR-REF-NO AND FDP-SCAT-KEY, THEY WILL BE
002980*    CHANGED BELOW.  ALSO PRDF-KEY
002990
003000     MOVE FDP-TR-REF-NO     TO WS-TR-REF-NO.
003010     MOVE FDP-SCAT-KEY      TO WS-SCAT-KEY.
003020     MOVE FDP-PRDF-KEY      TO WS-PRDF-KEY.
003030
003040     IF RTN-NO-ERROR AND SCAT1-ROWS-FOUND > 0                     OMM0742
003050        PERFORM C200-UPDATE-TDTLS
003060           THRU C200-EXIT
003070
003080*       DON'T CALL SUBPROCESS IF NO TDTL'S WERE UPDATED
003090
003100        IF RTN-NO-ERROR AND TDTL1-ROWS-FOUND > 0
003110           PERFORM C300-ADJUST-SCHED-ACTIVITY
003120              THRU C300-EXIT
003130
003140           IF RTN-NO-ERROR AND SUPERSTUB-NOT-SETUP
003150              PERFORM C500-SETUP-SUPERSTUB-CALL
003160                 THRU C500-EXIT
003170           END-IF
003180
003190*          CALL SUPERSTUB
003200
003210           IF RTN-NO-ERROR
003220              PERFORM C600-CALL-SUPERSTUB
003230                 THRU C600-EXIT
003240           END-IF
003250        END-IF
003260     END-IF.                                                      OMM0742
003270
003280*    PUT BACK FDP-VARS
003290
003300     MOVE WS-TR-REF-NO      TO FDP-TR-REF-NO.
003310     MOVE WS-SCAT-KEY       TO FDP-SCAT-KEY.
003320     MOVE WS-PRDF-KEY       TO FDP-PRDF-KEY.
003330
003340 B300-EXIT.
003350     EXIT.
003360
003370 B320-VALIDATE-REAL-MONEY-TR-CD.
003380
003390*    RETRIEVE TR-CD FOR REAL MONEY MOVEMENTS ACCTG DEFAULT
003400
003410     SET SYSTEM-TR-CD-NOT-FOUND TO TRUE.
003420     INITIALIZE TRGP8-DATA.
003430     MOVE 'I2'  TO TRGP8-GRP-TYP-CD (1).
003440     CALL 'CB2TRGDD' USING RMESSAGE,
003450                           TRGP8-DATA,
003460                           RTN-OUTPUT-FIELDS.
003470     IF RTN-NO-ERROR
003480        IF TRGP8-ROWS-FOUND = 0
003490*          NO ERROR, PROGRAM WILL EXIT UPON RETURN
003500           CONTINUE
003510        ELSE
003520           IF TRGP8-ROWS-FOUND = 1
003530*             SET FLAG AND TR-CD FOR CONTINUED PROCESSING
003540              SET SYSTEM-TR-CD-FOUND TO TRUE
003550              MOVE TRGP8-TR-CD (1) TO WS-SYSTEM-DEFAULT-TR-CD
003560           ELSE
003570*             ERROR >1 REAL MONEY MOVEMENT ACCTG DEFAULT
003580              MOVE WS-PROG-NM     TO RTN-PROG-NAME
003590              MOVE 'B320'         TO RTN-PARA-NAME
003600              MOVE 'TX'           TO RTN-ERR-PREFIX
003610              MOVE 3033           TO RTN-ERROR-CODE
003620              CALL 'CUTSVARB' USING RMESSAGE,
003630                                    RTN-OUTPUT-FIELDS
003640           END-IF
003650        END-IF
003660     END-IF.
003670
003680 B320-EXIT.
003690     EXIT.
003700
003710*****************************************************************
003720**   READ SCHED-ACTIVITY THAT PRODUCED THE TRANSACT_DETAILS
003730**   FROM THE FORWARD BILLING PROCESS
003740*****************************************************************
003750 C100-READ-SCHED-ACTIVITY.
003760
003770     INITIALIZE SCAT1-DATA
003780                CSBL7-DATA,
003790                CSBL1-DATA.
003800
003810     MOVE FDP-REF-KEY        TO CSBL1-CSBL-KEY.
003820
003830*    GET ORIGINAL SCAT KEY FROM CASE_BILLS,
003840*    IT'S NOT IN RTN-ORIG-SCAT-KEY
003850
003860     CALL 'CS1CSBLD' USING RMESSAGE,
003870                           CSBL1-DATA,
003880                           RTN-OUTPUT-FIELDS.
003890
003900     IF RTN-NO-ERROR
003910        MOVE CSBL1-SCAT-KEY  TO SCAT1-GRP-SCAT-KEY
003920        MOVE FDP-REF-KEY     TO CSBL7-CSBL-KEY
003930
003940        CALL 'CSESCATD' USING RMESSAGE,
003950                              SCAT1-DATA,
003960                              CSBL7-DATA,
003970                              RTN-OUTPUT-FIELDS
003980     END-IF.
003990
004000 C100-EXIT.
004010     EXIT.
004020
004030*****************************************************************
004040**   REVERSE THE TRANSACT_DETAILS THAT WERE INSERTED BY THE
004050**   FORWARD BILLING PROCESS
004060*****************************************************************
004070
004080 C200-UPDATE-TDTLS.
004090
004100*    ALREADY SAVED INCOMING FDP-SCAT-KEY, NOW SET FROM SCAT JUST
004110*    READ.  CU16TDTD USES FDP-SCAT-KEY, AND SUB PROCESS CALL
004120*    WILL ALSO USE THE SCAT1-DATA WE JUST READ.
004130*    CU16TDTD USES FDP VARS FDP-SCAT-KEY, FDP-CYC-DT AND FDP-USER
004140
004150     INITIALIZE TDTL1-DATA.
004160     MOVE SCAT1-SCAT-KEY TO TDTL1-SCAT-KEY.
004170     MOVE SCAT1-CASE-KEY TO TDTL1-CASE-KEY.
004180     CALL 'CU16TDTD'     USING RMESSAGE,
004190                               TDTL1-DATA,
004200                               RTN-OUTPUT-FIELDS.
004210
004220 C200-EXIT.
004230     EXIT.
004240
004250*****************************************************************
004260**   REUSE THE SCHED-ACTIVITY ALREADY READ, SET THE REVERSAL
004270**   PRDF VARIABLES
004280*****************************************************************
004290
004300 C300-ADJUST-SCHED-ACTIVITY.
004310
004320     INITIALIZE PRDF6-DATA.
004330
004340*    GET PRDF-KEY FOR REAL MONEY MOVEMENTS EXPENSES REVERSAL RVRM
004350
004360     MOVE 'RVRM'        TO PRDF6-ALIAS-NM.
004370*    '02' REVERSAL PROCESS
004380     MOVE '02'          TO PRDF6-TYP-CD.
004390
004400     CALL 'CS9PRDFD'    USING RMESSAGE,
004410                              PRDF6-DATA,
004420                              RTN-OUTPUT-FIELDS.
004430     IF RTN-NO-ERROR
004440*       ONLY NEED TO CHANGE VALUES NEEDED FOR THE RVRM CALL
004450        MOVE PRDF6-PRDF-KEY         TO SCAT1-PRDF-KEY
004460        MOVE FDP-TR-REF-NO          TO SCAT1-TR-REF-NO
004470        MOVE FDP-EFF-DT             TO SCAT1-EFF-DT,
004480                                       SCAT1-DUE-DT,
004490                                       SCAT1-TX-DT
004500        MOVE FDP-CYC-DT             TO SCAT1-CYC-DT
004510*       '0' UNPROCESSED
004520        MOVE '0'                    TO SCAT1-STAT-CD
004530        MOVE FDP-BOOK-DT            TO SCAT1-BOOK-DT
004540        MOVE FDP-CYC-DT             TO SCAT1-PROC-DT
004550        MOVE FDP-SCAT-KEY           TO RTN-ORIG-SCAT-KEY
004560
004570     END-IF.
004580
004590 C300-EXIT.
004600     EXIT.
004610
004620**--------------------------------------------------------------**
004630**  Call CS1GNSTD to validate that the steps in the process are **
004640**  COBOL programs.                                             **
004650**--------------------------------------------------------------**
004660 C500-SETUP-SUPERSTUB-CALL.
004670
004680     INITIALIZE FORMMSG.
004690
004700     MOVE PRDF6-PRDF-KEY   TO FRM-PRDF-KEY,
004710                              FDP-PRDF-KEY.
004720     MOVE FDP-USER                 TO FRM-USER.
004730
004740     MOVE SCAT1-SCAT-KEY            TO FRM-SCAT-KEY.
004750     IF FDP-DEBUG > 0                                             FDM3518
004760        MOVE '1' TO FRM-DEBUG (2:1)                               FDM3518
004770     ELSE
004780        MOVE '0' TO FRM-DEBUG (2:1)                               FDM3518
004790     END-IF.                                                      FDM3518
004800
004810     IF FDP-RUNSTAT > 0                                           FDM3518
004820        MOVE '1' TO FRM-DEBUG (1:1)                               FDM3518
004830     ELSE
004840        MOVE '0' TO FRM-DEBUG (1:1)                               FDM3518
004850     END-IF.                                                      FDM3518
004860
004870     CALL 'CS1GNSTD' USING RMESSAGE,
004880                           FORMMSG,
004890                           RTN-OUTPUT-FIELDS.
004900
004910     IF RTN-NO-ERROR
004920*       SET FLAG SO THIS PARAGRAPH WON'T BE CALLED AGAIN
004930        SET SUPERSTUB-SETUP TO TRUE
004940     END-IF.
004950
004960 C500-EXIT.
004970     EXIT.
004980
004990 C600-CALL-SUPERSTUB.
005000
005010     INITIALIZE STEP-DATA,
005020                WS-ERR-PRDF-KEY,
005030                WS-ERR-PRDF-NM,
005040                WS-ERR-TR-REF-NO.
005050
005060
005070**   DO NOT COMMIT DATA
005080     MOVE 'NC'                       TO STEP-COMMIT-FLAG
005090                                        OF STEP-DATA.
005100* SCAT1-LAUNCHED-BY-CD INDICATES WHERE COMPASS WAS ORIGINALLY
005110* KICKED OFF FROM. FDP-LAUNCHED-BY-CD HAS THIS INFO. MOVE THE CD
005120* SO THAT THE CD GETS PASSED THRU
005130     MOVE FDP-LAUNCHED-BY-CD     TO SCAT1-LAUNCHED-BY-CD.
005140     MOVE 'Y'                    TO SCAT1-DB-VALIDATED-CD.
005150
005160     CALL 'CSTCMPST' USING FORMMSG,
005170                           SCAT1-DATA,
005180                           STEP-DATA.
005190
005200**   FRM-ERR-CT = 2  04/FATAL ERROR
005210     IF FRM-ERR-CT = 2
005220**      FORCE HARD ERROR
005230        SET RTN-PROG-ERROR       TO TRUE
005240        MOVE 3                   TO FDP-ERR-CT
005250        MOVE PRDF6-PRDF-KEY      TO WS-ERR-PRDF-KEY
005260        MOVE PRDF6-NM            TO WS-ERR-PRDF-NM
005270**      SCAT1-TR-REF-NO HOLDS THE SUB PROCESS TR-REF-NO
005280        MOVE SCAT1-TR-REF-NO     TO WS-ERR-TR-REF-NO
005290**      804
005300        MOVE 804                 TO RTN-ERROR-CODE
005310        MOVE 'TX'                TO RTN-ERR-PREFIX
005320        MOVE WS-PROG-NM          TO RTN-PROG-NAME
005330        MOVE 'C600-CALL-SUPERSTUB'
005340                                 TO RTN-PARA-NAME
005350        MOVE WS-ERR-MSG3         TO RTN-VAR-TEXT   (1)
005360        MOVE 1                   TO RTN-VAR-SEQ-NO (1)
005370        MOVE WS-ERR-MSG5         TO RTN-VAR-TEXT   (2)
005380        MOVE 2                   TO RTN-VAR-SEQ-NO (2)
005390        MOVE WS-ERR-MSG6         TO RTN-VAR-TEXT   (3)
005400        MOVE 3                   TO RTN-VAR-SEQ-NO (3)
005410        MOVE 3                   TO RTN-VAR-DETL-CNT
005420        CALL 'CUTSVARB'       USING RMESSAGE,
005430                                    RTN-OUTPUT-FIELDS
005440*       exit paragraph
005450        GO TO C600-EXIT
005460     END-IF.
005470
005480**   FRM-ERR-CT = 1  02/ERROR PROCESS HALTED
005490**   TX #805  MSG WILL INFORM THE USER THAT AN ERROR
005500**   OCCURED DURING THE PROCESSING OF THE 'SUB PROCESS'.
005510**   SYS VAR DETLS DISPLAYS THE SUB PROCESS NAME AND TR REF NO
005520
005530     IF FRM-ERR-CT  = 1
005540        MOVE PRDF6-PRDF-KEY    TO WS-ERR-PRDF-KEY
005550        MOVE PRDF6-NM          TO WS-ERR-PRDF-NM
005560**      SCAT1-TR-REF-NO HOLDS THE SUB PROCESS TR REF-NO
005570        MOVE SCAT1-TR-REF-NO   TO WS-ERR-TR-REF-NO
005580**      FORCE HARD ERROR
005590        SET RTN-PROG-ERROR     TO TRUE
005600        MOVE 805               TO RTN-ERROR-CODE
005610        MOVE 'TX'              TO RTN-ERR-PREFIX
005620        MOVE WS-PROG-NM        TO RTN-PROG-NAME
005630        MOVE 'C600-CALL-SUPERSTUB'
005640                               TO RTN-PARA-NAME
005650        MOVE WS-ERR-MSG3       TO RTN-VAR-TEXT   (1)
005660        MOVE 1                 TO RTN-VAR-SEQ-NO (1)
005670        MOVE WS-ERR-MSG5       TO RTN-VAR-TEXT   (2)
005680        MOVE 2                 TO RTN-VAR-SEQ-NO (2)
005690        MOVE WS-ERR-MSG6       TO RTN-VAR-TEXT   (3)
005700        MOVE 3                 TO RTN-VAR-SEQ-NO (3)
005710        MOVE 3                 TO RTN-VAR-DETL-CNT
005720        CALL 'CUTSVARB'     USING RMESSAGE,
005730                                  RTN-OUTPUT-FIELDS
005740**      exit paragraph
005750        GO TO C600-EXIT
005760     END-IF.
005770
005780**   IF FRM-WARN-CT > 0 INFORMATION OR WARNING
005790**   MESSAGE(S) WERE INSERTED.  CONTINUE PROCESSING
005800     IF FRM-WARN-CT > 0
005810
005820        SET RTN-NO-ERROR TO TRUE
005830        MOVE ZEROS        TO  FDP-ERR-CT
005840        MOVE FRM-WARN-CT  TO  FDP-WARN-CT
005850
005860        IF FDP-DEBUG > 0
005870           DISPLAY 'CUTCMPSB - INFORMATION OR WARNING ',
005880           DISPLAY 'MESSAGE(S) RETURNED FROM CALL TO SUPERSTUB'
005890        END-IF
005900*       EXIT PARAGRAPH
005910        GO TO C600-EXIT
005920     END-IF.
005930
005940*    CLEAN RETURN FROM SUPERSTUB
005950     IF FRM-WARN-CT = 0  AND
005960        FRM-ERR-CT  = 0
005970        SET RTN-NO-ERROR TO TRUE
005980        MOVE ZEROS TO FDP-ERR-CT
005990        MOVE ZEROS TO FDP-WARN-CT
006000     END-IF.
006010
006020 C600-EXIT.
006030     EXIT.
006040
006050
