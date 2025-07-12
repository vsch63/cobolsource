000010*Property of FIS©. or its affiliates
000020*all rights reserved. FIS Confidential
000030 IDENTIFICATION DIVISION.
000040 PROGRAM-ID.           CRKCOLLB.
000050 AUTHOR.               PAUL TETREAULT.
000060 DATE-WRITTEN.         APR 2007.
000070****************************************************************
000080*   $Modtime::   10 Oct 2018 17:06:48  $
000090*   $Log::   P:/pvcsproj/DONOTUSE.prj/source/CRKCOLLB.COV  $
000100*
000110*   Rev 1.0.1.4   10 Oct 2018 17:29:34   alex
000120*FDM4167
000130*fis copyright
000140*
000150*   Rev 1.0.1.3   10 Mar 2017 13:11:56   paul
000160*MOM0786
000170*Correct Offseting Benefit/Premium Details Issue
000180*
000190*   Rev 1.0.1.2   01 Jun 2007 11:15:50   paul
000200*STM1033
000210*Reduce Benefit/Premium Details
000220*
000230*   Rev 1.0.1.1   25 May 2007 17:07:46   paul
000240*STM1033
000250*Reduce Benefit/Premium Details
000260****************************************************************
000270*
000280*   THIS STEP CALLS A STORED PROCEDURE TO COLLAPSE BNTD, BCHT,
000290*   PMTD FOR A GIVEN TRANSACTION, BENEFIT AND MEMBER.
000300*
000310****************************************************************
000320*                     CHANGES LOG
000330****************************************************************
000340*
000350*   DATE     PRG   PROJ#      DESCRIPTION
000360*  --------  --- ---------  --------------------------------------
000370*  04/10/07  PDT  STM1033   NEW PROGRAM.                          STM1033
000380*  16-FEB-17 PDT  MOM0786   DELETE 'BNTR' ENTRIES FROM
000390*                           GTMP_SESS_AREA CREATED IN CRKCO01B
000400****************************************************************
000410
000420 ENVIRONMENT DIVISION.
000430 CONFIGURATION SECTION.
000440*
000450 DATA DIVISION.
000460 WORKING-STORAGE SECTION.
000470
000480 01 BEG-WORK-STOR.
000490    05 FILLER                             PIC X(40) VALUE
000500       'THIS IS THE BEGINNING OF WORKING STORAGE'.
000510
000520 01  WS-PGM-NAME                          PIC X(8).
000530
000540 01  WS-MISC-VARIABLES.
000550     05  WS-COMPILED                      PIC X(20).
000560     05  CSMB                             PIC S9(4) COMP.
000570
000580*    DAL WORKING STORAGE / LINKAGE COPYBOOKS
000590     COPY CL2RKPDD.
000600     COPY CL1BNTRD.
000610*    FOR DELETE GTMP_SESS_AREA                                    MOM0786
000620     COPY CL1PKGSD.                                               MOM0786
000630
000640**  REVISION AREA **
000650 01  WS-REVISION           PIC X(35)  VALUE
000660     ' $Revision::   1.0.1.4        $'.
000670 LINKAGE SECTION.
000680     COPY CLKFDPNC.
000690     COPY CL1CASDD.
000700     COPY CLKERRBC.
000710
000720*********************************************************
000730**   P R O C E D U R E    D I V I S I O N              **
000740*********************************************************
000750
000760 PROCEDURE DIVISION USING RMESSAGE,
000770                          CASD1-DATA,
000780                          RTN-OUTPUT-FIELDS.
000790
000800     IF FDP-DEBUG-RUNSTAT > '00'
000810        ADD +1  TO RTN-DIMT-CNT
000820        MOVE WHEN-COMPILED TO
000830             RTN-DIMT-COMPILE-DATE (RTN-DIMT-CNT)
000840        MOVE WS-REVISION (16:10) TO
000850             RTN-DIMT-REVISION (RTN-DIMT-CNT)
000860        MOVE 'CRKCOLLB' TO
000870           RTN-DIMT-PROGRAM-NAME (RTN-DIMT-CNT)
000880        CALL 'CUTDIMTD' USING RMESSAGE,
000890                              RTN-OUTPUT-FIELDS
000900     END-IF.
000910
000920******************************************************************
000930**                MAIN ROUTINE                                  **
000940******************************************************************
000950
000960     PERFORM A000-INITIAL
000970        THRU A000-EXIT.
000980
000990     IF RTN-NO-ERROR
001000        PERFORM B000-MAIN
001010           THRU B000-EXIT
001020     END-IF.
001030
001040*  DELETE 'BNTR' ENTRIES FROM                                     MOM0786
001050*  GTMP_SESS_AREA CREATED IN CRKCO01B                             MOM0786
001060     IF RTN-NO-ERROR                                              MOM0786
001070        MOVE 'BNTR'           TO PKGS1-REF-TYP-CD                 MOM0786
001080        CALL 'CD1GTSAD'    USING RMESSAGE                         MOM0786
001090                                 PKGS1-DATA                       MOM0786
001100                                 RTN-OUTPUT-FIELDS                MOM0786
001110     END-IF.                                                      MOM0786
001120
001130     GOBACK.
001140
001150 A000-INITIAL.
001160
001170     MOVE 'CRKCOLLB'                 TO RTN-PROG-NAME,
001180                                        WS-PGM-NAME.
001190
001200* EDIT INCOMING PARMS
001210     PERFORM A100-READ-PARMS
001220        THRU A100-EXIT.
001230
001240 A000-EXIT.
001250     EXIT.
001260
001270 A100-READ-PARMS.
001280
001290     IF FDP-CASE-KEY <= ZERO
001300        MOVE 'UT'            TO RTN-ERR-PREFIX
001310        MOVE 30              TO RTN-ERROR-CODE
001320        MOVE 'CRKCOLLB'      TO RTN-PROG-NAME
001330        MOVE 'A100-READ'     TO RTN-PARA-NAME
001340        CALL 'CUTSVARB' USING RMESSAGE,
001350                              RTN-OUTPUT-FIELDS
001360     END-IF.
001370
001380     IF FDP-EFF-DT = SPACES AND RTN-NO-ERROR
001390        MOVE 'UT'            TO RTN-ERR-PREFIX
001400        MOVE 35              TO RTN-ERROR-CODE
001410        MOVE 'CRKCOLLB'      TO RTN-PROG-NAME
001420        MOVE 'A100-READ'     TO RTN-PARA-NAME
001430        CALL 'CUTSVARB' USING RMESSAGE,
001440                              RTN-OUTPUT-FIELDS
001450     END-IF.
001460
001470     IF FDP-USER = SPACES AND RTN-NO-ERROR
001480        MOVE 'UT'            TO RTN-ERR-PREFIX
001490        MOVE 39              TO RTN-ERROR-CODE
001500        MOVE 'CRKCOLLB'      TO RTN-PROG-NAME
001510        MOVE 'A100-READ'     TO RTN-PARA-NAME
001520        CALL 'CUTSVARB' USING RMESSAGE,
001530                              RTN-OUTPUT-FIELDS
001540     END-IF.
001550
001560 A100-EXIT.
001570     EXIT.
001580
001590 B000-MAIN.
001600
001610* SELECT MEMBERS WITH BENEFIT\PREMIUM CHANGES
001620* BY TR-REF-NO
001630
001640     INITIALIZE RKPD2-DATA.
001650
001660     PERFORM UNTIL RKPD2-END-FLAG = 'Y' OR RTN-ERROR
001670       MOVE FDP-CASE-MBR-KEY   TO RKPD2-CASE-MBR-KEY (1)
001680       MOVE FDP-TR-REF-NO      TO RKPD2-TR-REF-NO (1)
001690       CALL 'CF5VBMBD' USING RMESSAGE,
001700                             RKPD2-DATA,
001710                             RTN-OUTPUT-FIELDS
001720
001730       PERFORM VARYING CSMB FROM 1 BY 1
001740         UNTIL CSMB > RKPD2-ROWS-FOUND OR RTN-ERROR
001750         PERFORM B100-COLLAPSE
001760            THRU B100-EXIT
001770       END-PERFORM
001780
001790     END-PERFORM.
001800
001810 B000-EXIT.
001820     EXIT.
001830
001840 B100-COLLAPSE.
001850
001860     MOVE RKPD2-CASE-MBR-KEY (CSMB)     TO BNTR1-CASE-MBR-KEY.
001870     MOVE RKPD2-BNTP-KEY (CSMB)         TO BNTR1-BNTP-KEY.
001880     MOVE RKPD2-TR-REF-NO (CSMB)        TO BNTR1-BNTS-TR-REF-NO.
001890
001900     CALL 'CP1COLLD' USING RMESSAGE,
001910                           BNTR1-DATA,
001920                           RTN-OUTPUT-FIELDS.
001930
001940
001950 B100-EXIT.
001960     EXIT.
