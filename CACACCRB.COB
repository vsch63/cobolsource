000010*Property of FIS©. or its affiliates
000020*all rights reserved. FIS Confidential
000030 IDENTIFICATION DIVISION.
000040 PROGRAM-ID.           CACACCRB.
000050 AUTHOR.               CATHY SCOTT.
000060 DATE-WRITTEN.         2001-11-29.
000070****************************************************************
000080*   $Modtime::   10 Oct 2018 10:15:16  $
000090*   $Log::   P:/pvcsproj/DONOTUSE.prj/source/CACACCRB.COV  $
000100*
000110*   Rev 1.0.1.29   10 Oct 2018 11:45:20   alex
000120*FDM4167
000130*fis copyright
000140*
000150*   Rev 1.0.1.28   22 May 2015 05:43:20   shiv
000160*CLM0742
000170*Add Logic to Prevent Loop and Issue Error.
000180*
000190*   Rev 1.0.1.27   12 Dec 2014 14:12:22   paul
000200*CLM0716
000210*Correct Accrual Accounting Restart Logic
000220*
000230*   Rev 1.0.1.26   14 Jun 2011 08:41:06   abhishek
000240*CLM0439
000250*Future dated schemes cannot retrieve province
000260*
000270*   Rev 1.0.1.25   19 Apr 2011 17:27:22   tom
000280*LSM5259
000290*Replace call to CUTDTCAL
000300*
000310*   Rev 1.0.1.24   01 Apr 2011 04:05:36   maheshs
000320*CLM0211
000330*CHANGES RELATED TO ACCRUAL ACCOUNTING
000340*
000350*   Rev 1.0.1.23   11 Aug 2010 01:32:28   shiv
000360*CLM0159
000370*change domain value 'ZA' to 'BS' for domain 'TRGP GRP TYP CD'
000380*
000390*   Rev 1.0.1.22   01 Jul 2010 08:23:56   jayashree
000400*CLM0054
000410*changes for accounting by province and benefit company
000420*
000430*   Rev 1.0.1.21   08 May 2009 03:06:28   sandeepg
000440*USM0065
000450*FIX THE JOURNAL LOGS.
000460*
000470*   Rev 1.0.1.20   14 May 2008 05:37:22   jayashree
000480*LPM0575
000490*increase variable length for change in CWSDTCAL
000500*
000510*   Rev 1.0.1.19   20 Mar 2007 13:52:56   eric
000520*PFM0304
000530*accrual accounting issue
000540*
000550*   Rev 1.0.1.18   12 Mar 2007 16:34:30   eric
000560*PFM0324
000570*correct subscript
000580*
000590*   Rev 1.0.1.17   20 Nov 2006 15:45:14   Cathyc
000600*PFM0286
000610*Change SCAT Eff Dt when future premiums are processed
000620*for schemes that are not yet active.
000630*
000640*   Rev 1.0.1.16   22 Feb 2006 11:02:44   Cathyc
000650*PFZ0113
000660*ACCRUAL ACCTG TOLA
000670*
000680*   Rev 1.0.1.15   21 Feb 2006 08:47:20   Cathyc
000690*PFZ0113
000700*ACCRUAL ACCOUNTING FOR TOLERANCE BILLING
000710*
000720*   Rev 1.0.1.14   10 Feb 2006 16:28:16   Cathyc
000730*PFZ0113
000740*TOLERANCE BILLING CHANGES
000750*
000760*   Rev 1.0.1.13   23 Sep 2005 11:40:00   robin
000770*FDM3514
000780*Module Impact Analysis Tool - WS-REVISION awk script fix.
000790*
000800*   Rev 1.0.1.12   19 Sep 2005 18:58:10   robin
000810*FDM3514
000820*Module Impact Analysis Tool
000830*
000840*   Rev 1.0.1.11   19 May 2005 10:12:24   cathy
000850*ANM0264
000860*neg amount allowed
000870*
000880*   Rev 1.0.1.10   03 Mar 2004 09:44:24   cathy
000890*ANS0208
000900*sfdt only for d&u total
000910*
000920*   Rev 1.0.1.9   26 Jan 2004 07:42:10   cathy
000930*ANS0196
000940*fix sfdt logic for d&u
000950*
000960*   Rev 1.0.1.8   20 Dec 2002 10:55:44   rieko
000970*FDM3175
000980*COBOL modifications to support the Oracle 8 pre-complier.
000990*
001000*   Rev 1.0.1.7   09 Sep 2002 11:06:12   cathy
001010*ANR0101
001020*fix due-prem logic
001030*
001040*   Rev 1.0.1.6   03 Jul 2002 14:26:04   pault
001050*ANR0089
001060*Initialize ws-prem-ratio
001070*
001080*   Rev 1.0.1.5   22 May 2002 16:50:36   cathy
001090*ANS0076
001100*fix error message logic
001110*
001120*   Rev 1.0.1.4   22 May 2002 15:26:30   pault
001130*ANM0035
001140*Reinsurance mods
001150*
001160*   Rev 1.0.1.3   11 Apr 2002 07:49:44   cathy
001170*ANS0076
001180*accrual acct changes
001190*
001200*   Rev 1.0.1.2   26 Mar 2002 16:53:38   cathy
001210*ANS0072
001220*accrual acct changes
001230*
001240*   Rev 1.0.1.1   13 Dec 2001 08:13:42   cathy
001250*ANM0017
001260*Accrual Accounting
001270*
001280****************************************************************
001290* ACCRUAL ACCOUNTING MODULE
001300****************************************************************
001310* THIS MODULE WILL PROCESS ON A CASE BILL
001320* 1)  DETERMINE IF THE BILL IS IN ADVANCE PREMIUM, UNEARNED
001330*     PREMIUM OR DUE AND UNCOLLECTED PREMIUM STATUS.
001340* 2)  DETERMINE THE PREMIUM RATIO TO BE APPLIED.
001350* 3)  FETCH AND SUM ALL BRDT AND SFDT RECORDS.  GROUP BY TR CD.
001360* 4)  MAP EACH BRDT/SFDT RECORD TO ACCOUTING TR CD.  ADD TO
001370*     INTERNAL ARRAY FOR PROCESSING.
001380* 5)  PRORATE BRDT/SFDT AMOUNT BASED ON PREMIUM RATIO.
001390* 6)  INSERT NOTIONAL TRANSACT DETAILS.
001400* 7)  INSERT ENTRY TO XREF TABLE (RELATED TO RESTART LOGIC).
001410****************************************************************
001420*                     CHANGES LOG
001430****************************************************************
001440*
001450*   DATE     PRG   PROJ#      DESCRIPTION
001460*  --------  --- ---------  --------------------------------------
001470*  11/29/01  CMS ANM0017    INITIAL CHECK IN
001480*  03/26/02  CMS ANS0072    CHANGE COPYBOOK TO CL2TRCRD
001490*  04/10/02  CMS ANS0076    ADD EDIT IF TDTL-TR-CD IS NULL
001500*  05/15/02  PDT ANM0035    ADD LOGIC FOR REASSURANCE.
001510*  05/22/02  CMS ANS0076    FIX ERROR MESSAGE
001520*  06/24/02  PDT ANR0089    INITIALIZE WS-PREM-RATIO.
001530*  09/09/02  CMS ANR0101    FIX RATIO FOR D&U TOTAL. ZERO RATIO
001540*                           IS VALID.
001550*  01/22/04  CMS ANS0196    SFDT ONLY FOR D&U TOTAL FIX
001560*  03/03/04  CMS ANS0208    MORE CHANGES FOR SFDT D&U PROCESSING
001570*  05/19/05  CMS ANM0264    D&U PROCESSING FOR NEG AMTS (ANH0208)
001580*  02/07/05  CFC PFZ0113    TOLERANCE BILLING CHANGES - TREAT
001590*                           ADVANCE AND UNEARNED AS FOR DUE-PREM
001600*                           WHEN REINIT-ACCT-RULES HAS '02' ROW
001610*                         (SHOWS TOLERANCE BILLING IS IN EFFECT)
001620*                           TO TAKE ACCOUNT OF PARTIAL PAYMENT
001630* 16-NOV-06  CFC PFM0286    USE NXT-REINIT-DT EXPLICITLY RATHER
001640*                           THAN VIA FDP-EFF-DT, FOR ADVANCE
001650*                           PREMIUMS FOR NOT-YET-ACTIVE SCHEMES.
001660* 12-MAR-07  EDM PFM0324    CORRECT SUBSCRIPT WHEN SAME TDTL-TR-CD
001670* 20-MAR-07  EDM PFM0304    MODIFY B400 TO RECALCUATE THE         PFM0304
001680*                           TDTL AMOUNT FOR UNEARNED PREMIUMS     PFM0304
001690*                           AND PREMIUMS DUE.                     PFM0304
001700* 13-MAY-08  JM  LPM0575    EXPAND FIELD SIZE FOR CHANGE IN CWSDTCAL
001710* 04-MAY-09  SG  USM0065A   FIX THE JOURNAL LOGS FOR UNEARNED ACCRUAL
001720*                           WHEN A BILL IS FULLY PAID BUT IN MULTIPLE
001730*                           PAYMENTS.
001740* 04-MAY-09  SG  US50065B   FIX JOURNAL LOGS FOR DUE ACCRUAL
001750*                           SO THAT WE ONLY REPORT THE ACTUAL
001760*                           PREMIUM PAST DUE, NOT THE FULL AMOUNT.
001770* 25-JUN-10  JM  CLM0054    NEW LOGIC TO PERFORM ACCOUNTING BY
001780*                           BENEFIT COMPANY AND PROVINCE
001790* 10-AUG-10  SNG CLM0159    CHANGE DOMAIN VALUE 'ZA' TO 'BS' FOR
001800*                           DOMAIN 'TRGP GRP TYP CD'
001810* 09-FDB-11  MCS CLM0211    1.DO NOT EXIT PRORATION LOOP IF NEG AMT
001820*                             CHANGES FOR CL70054 SPECIFIC TO HOW
001830*                             CLIC WANTS TO DO ACCRUAL ACCOUNTING
001840*                           2.NEW ACCRUAL ACCOUNTING RULE TYPE '03'
001850*                             ACCRUAL ACCOUNTING BENEFIT AND
001860*                             PROVINCE.
001870*                           3.MORE CHANGES SPECIFIC TO HOW
001880*                             CLIC WANTS TO DO ACCRUAL ACCOUNTING
001890*                           4.INCL TOLA SFDT: SPECIFIC TO HOW
001900*                             CLIC WANTS TO DO ACCRUAL ACCOUNTING
001910*                           5.ADDED BLANK LINES IN SEARCH LOGIC
001920*                           6.FIND FREQ FOR TOLA BILL FOR ADVANCED:
001930*                             SPECIFIC TO CLIC ACCRUAL ACCOUNTING
001940* 30-MAY-11  AD  CLM0439    USE SCHEME COMMENCEMENT DATE AS
001950*                           BOOKING DATE ON TDTL WHEN COMMENCEMENT
001960*                           DATE IS IN THE FUTURE.
001970* 06-OCT-14  PDT CLM0716    CORRECTLY POPULATE XREF1-MSTR-REF-TYP
001980* 21-MAY-15  SNG CLM0742    ADD INITIAL PREVIOUS BILL LOGIC.
001990****************************************************************
002000 ENVIRONMENT DIVISION.
002010 CONFIGURATION SECTION.
002020*
002030 DATA DIVISION.
002040 WORKING-STORAGE SECTION.
002050
002060 01  WS-PGM-NAME                          PIC X(08)
002070                                          VALUE 'CACACCRB'.
002080
002090
002100 01  WS-SUBSCRIPTS.
002110     05  SUB0                             PIC S9(4) COMP.
002120     05  SUB1                             PIC S9(4) COMP.
002130     05  BRDTX                            PIC S9(4) COMP.
002140     05  DUEUX                            PIC S9(4) COMP.
002150     05  TRTRX                            PIC S9(4) COMP.
002160     05  TDTLX                            PIC S9(4) COMP.         ANM0035
002170     05  IVTXH                            PIC S9(4) COMP.         CLM0054
002180     05  IBRDT                            PIC S9(04) COMP.        CLM0211
002190
002200 01  WS-MISC-VARS.
002210     05  WS-PREM-RATIO                    PIC S9(06)V9(06) COMP-3.
002220     05  WS-DETL-RATIO                    PIC S9(06)V9(06) COMP-3.CLM0211
002230     05  WS-DAYS-DIFF-1                   PIC S9(05) COMP.        LPM0575
002240     05  WS-DAYS-DIFF-2                   PIC S9(05) COMP.        LPM0575
002250     05  WS-ACT-NUM                       PIC S9(03) COMP.
002260     05  WS-ACT-NUM-2                     PIC S9(03) COMP.
002270     05  WS-PROC-IND                      PIC S9(01) COMP.
002280     05  WS-BEG-DT                        PIC X(10).
002290     05  WS-TDTL-TR-CD                    PIC X(04).
002300     05  WS-PREV-TAX-ID-CD                PIC X(08).              CLM0054
002310     05  WS-PRORATE-AMT                   PIC S9(13)V9(2) COMP-3. CLM0211
002320     05  WS-SPAN-BEG-DT                   PIC X(10).              CLM0211
002330     05  WS-SPAN-NXT-DT                   PIC X(10).              CLM0211
002340     05  WS-ORIG-CSBL-KEY                 PIC S9(15) COMP-3.      CLM0211
002350     05  WS-CSBL-KEY                      PIC S9(15) COMP-3.      CLM0211
002360     05  WS-CSBL-KEY-NUM                  PIC 9(15).              CLM0742
002370*    THIS ARRAY USED FOR ORIGINAL BILL FOR TOLERANCE BILL.        CLM0211
002380     05 WS-FDRT-ARRAY.                                            CLM0211
002390        10  WS-F-ROWS                    PIC S9(04) COMP.         CLM0211
002400        10  WS-FDRT-ARRAY-REC OCCURS 600 TIMES                    CLM0211
002410                 INDEXED BY FIDX.                                 CLM0211
002420            15  WS-F-FDRT-KEY              PIC S9(15) COMP-3.     CLM0211
002430            15  WS-F-SUM                   PIC S9(13)V99 COMP-3.  CLM0211
002440            15  WS-FP-ROWS               PIC S9(04) COMP.         CLM0211
002450            15  WS-FDRT-PROV-RECORD OCCURS 20 TIMES               CLM0211
002460                 INDEXED BY PIDX.                                 CLM0211
002470                20  WS-FP-TAX-ID-CD        PIC X(08).             CLM0211
002480                20  WS-FP-SUM              PIC S9(13)V99 COMP-3.  CLM0211
002490
002500     05 WS-PROCESS-COUNT                PIC S9(04) COMP.          CLM0211
002510     05 WS-ADJ-AMT                      PIC S9(13)V99 COMP-3.     CLM0211
002520     05 WS-REMAIN-1                     PIC S9(13)V99 COMP-3.     CLM0211
002530     05 WS-REMAIN-2                     PIC S9(13)V99 COMP-3.     CLM0211
002540     05 WS-FACTOR                       PIC S9(6)V9(6) COMP-3.    CLM0211
002550
002560 01  WS-LOCAL-FLAGS.
002570     05  ACCOUNTING-FLAG                  PIC X.
002580         88  NO-ACCOUNTING                VALUE 'N'.
002590         88  ADVANCE-PREM                 VALUE 'A'.
002600         88  UNEARNED-PREM                VALUE 'U'.
002610         88  DUE-PREM                     VALUE 'D'.
002620     05  ROW-FOUND-FLAG                   PIC X.
002630         88  ROW-FOUND                    VALUE 'Y'.
002640         88  ROW-NOT-FOUND                VALUE 'N'.
002650     05  TR-CD-FOUND-FLAG                 PIC X.
002660         88  TR-CD-FOUND                  VALUE 'Y'.
002670         88  TR-CD-NOT-FOUND              VALUE 'N'.
002680     05  RECORD-FLAG                      PIC X.
002690         88  FIRST-REC                    VALUE 'F'.
002700         88  SECOND-REC                   VALUE 'S'.
002710     05  SKIP-REC-FLAG                    PIC X.                  ANS0208
002720         88  SKIP-REC                     VALUE 'Y'.              ANS0208
002730         88  DONT-SKIP-REC                VALUE 'N'.              ANS0208
002740     05  ACCRUAL-TOLA-FLAG                PIC X.                  PFZ0113
002750         88  ACCRUAL-TOLA                 VALUE 'Y'.              PFZ0113
002760         88  NOT-ACCRUAL-TOLA             VALUE 'N'.              PFZ0113
002770     05  OVERRIDE-ACCT-FLAG               PIC X.                  CLM0054
002780         88  OVERRIDE-ACCT                VALUE 'Y'.              CLM0054
002790         88  NO-OVERRIDE-ACCT             VALUE 'N'.              CLM0054
002800     05  DETL-ACCOUNTING-FLAG             PIC X.                  CLM0211
002810         88  DETL-UNEARNED-PREM           VALUE 'Y'.              CLM0211
002820         88  NOT-DETL-UNEARNED-PREM       VALUE 'N'.              CLM0211
002830     05  TOLA-BILL-FLAG                   PIC X.                  CLM0211
002840         88  TOLA-BILL                    VALUE 'Y'.              CLM0211
002850         88  NOT-TOLA-BILL                VALUE 'N'.              CLM0211
002860     05  INITIAL-PREV-BILL-FLAG           PIC X.                  CLM0742
002870         88  INITIAL-PREV-BILL            VALUE 'Y'.              CLM0742
002880         88  NOT-INITIAL-PREV-BILL        VALUE 'N'.              CLM0742
002890
002900 01  WS-ACCR-OUTPUT-ARRAY.
002910     05  WS-ACCR-COUNT                    PIC S9(4) COMP-3.
002920*    Increase array size from 500 to 5000                         CLM0054
002930     05  WS-ACCR-ARRAY-REC OCCURS 5000 TIMES.                     CLM0211
002940         10  WS-REAS-REF-CD               PIC X(04).              CLM0211
002950         10  WS-OUTPUT-TR-CD              PIC X(04).
002960         10  WS-ACCR-IND                  PIC X(01).
002970         10  WS-FDRT-KEY                  PIC S9(15) COMP-3.      CLM0054
002980         10  WS-TAX-ID-CD                 PIC X(08).              CLM0054
002990         10  WS-ACCR-AMT                  PIC S9(13)V9(2) COMP-3.
003000         10  WS-ACCR-PRORATE-AMT          PIC S9(13)V9(2) COMP-3.
003010
003020 01  WS-TOLA-OUTPUT-ARRAY.                                        CLM0211
003030     05  WS-TOLA-COUNT                    PIC S9(4) COMP-3.       CLM0211
003040*    THIS ARRAY MUST BE KEPT IN SYNC WITH WS-ACCR-OUTPUT-ARRAY!!! CLM0211
003050     05  WS-TOLA-ARRAY-REC OCCURS 5000 TIMES.                     CLM0211
003060         10  WS-T-REAS-REF-CD             PIC X(04).              CLM0211
003070         10  WS-T-OUTPUT-TR-CD            PIC X(04).              CLM0211
003080         10  WS-T-ACCR-IND                PIC X(01).              CLM0211
003090         10  WS-T-FDRT-KEY                PIC S9(15) COMP-3.      CLM0211
003100         10  WS-T-TAX-ID-CD               PIC X(08).              CLM0211
003110         10  WS-T-ACCR-AMT                PIC S9(13)V9(2) COMP-3. CLM0211
003120         10  WS-T-ACCR-PRORATE-AMT        PIC S9(13)V9(2) COMP-3. CLM0211
003130
003140 01  SYS-VAR-DETL.
003150     05  WS-ERROR1.
003160         10  FILLER                       PIC X(16)               CLM0211
003170             VALUE 'BILL TR-REF-NO: '.                            CLM0211
003180         10  WS-TR-REF-NO                 PIC X(12).              CLM0211
003190     05  WS-ERROR2.                                               ANS0072
003200         10  FILLER                       PIC X(15)               ANS0072
003210             VALUE ' TR-CD: '.                                    ANS0072
003220         10  WS-TR-CD                     PIC X(04).              ANS0072
003230     05  WS-ERROR3.                                               ANS0076
003240         10  FILLER                       PIC X(15)               ANS0076
003250             VALUE ' ACTIVITY NUM: '.                             ANS0076
003260         10  WS-ACT-NUM-3                 PIC 9(03).              ANS0076
003270     05  WS-ERROR4.                                               CLM0054
003280         10  FILLER                       PIC X(15)               CLM0054
003290             VALUE ' PROVINCE : '.                                CLM0054
003300         10  WS1-TAX-ID-CD                PIC X(08).              CLM0054
003310**   THIS MUST BE KEPT IN SYNC WITH CSBL1-DATA!!!!!               CLM0211
003320 01  SAVE-CSBL1-DATA.                                             CLM0211
003330     05  SAVE-CSBL1-ROWS-FOUND     PIC S9(04) COMP-5.             CLM0211
003340**   DATA BASE FIELDS                                             CLM0211
003350     05  SAVE-CSBL1-CSBL-KEY       PIC S9(15) COMP-3.             CLM0211
003360     05  SAVE-CSBL1-BGRL-KEY       PIC S9(15) COMP-3.             CLM0211
003370     05  SAVE-CSBL1-TSCH-KEY       PIC S9(15) COMP-3.             CLM0211
003380     05  SAVE-CSBL1-TR-REF-NO      PIC X(12).                     CLM0211
003390     05  SAVE-CSBL1-BILL-DUE-DT    PIC X(10).                     CLM0211
003400     05  SAVE-CSBL1-BILL-SENT-STAT-CD PIC X(01).                  CLM0211
003410     05  SAVE-CSBL1-BEG-BILL-DT    PIC X(10).                     CLM0211
003420     05  SAVE-CSBL1-NXT-BILL-DT    PIC X(10).                     CLM0211
003430     05  SAVE-CSBL1-FRST-PYRL-DT   PIC X(10).                     CLM0211
003440     05  SAVE-CSBL1-OVR-PYMT-AMT   PIC S9(13)V9(2) COMP-3.        CLM0211
003450     05  SAVE-CSBL1-TR-TYP-CD      PIC X(2).                      CLM0211
003460     05  SAVE-CSBL1-CASE-KEY       PIC S9(15) COMP-3.             CLM0211
003470     05  SAVE-CSBL1-RCVB-STAT-CD   PIC X(2).                      CLM0211
003480     05  SAVE-CSBL1-BILL-DT        PIC X(10).                     CLM0211
003490     05  SAVE-CSBL1-PAST-DUE-AMT   PIC S9(13)V9(2) COMP-3.        CLM0211
003500     05  SAVE-CSBL1-PAYR-KEY       PIC S9(15) COMP-3.             CLM0211
003510     05  SAVE-CSBL1-PROC-DT        PIC X(10).                     CLM0211
003520     05  SAVE-CSBL1-DUE-ACCT-CD    PIC X(01).                     CLM0211
003530     05  SAVE-CSBL1-SPLIT-STAT-CD  PIC X(2).                      CLM0211
003540     05  SAVE-CSBL1-SLRQ-KEY       PIC S9(15) COMP-3.             CLM0211
003550     05  SAVE-CSBL1-MATCH-ID       PIC X(10).                     CLM0211
003560     05  SAVE-CSBL1-BGRR-KEY       PIC S9(15) COMP-3.             CLM0211
003570     05  SAVE-CSBL1-SCAT-KEY       PIC S9(15) COMP-3.             CLM0211
003580     05  SAVE-CSBL1-XTNS-DT        PIC X(10).                     CLM0211
003590     05  SAVE-CSBL1-ORIG-BILL-AMT  PIC S9(13)V9(2) COMP-3.        CLM0211
003600     05  SAVE-CSBL1-REL-CSBL-KEY   PIC S9(15) COMP-3.             CLM0211
003610     05  SAVE-CSBL1-UMB-CSBL-KEY   PIC S9(15) COMP-3.             CLM0211
003620
003630**
003640**   COPYBOOKS
003650**
003660
003670*  TR_CODE_TRNCH
003680*    COPY CL5TRTRD.                                               ANS0072
003690     COPY CL2TRCRD.                                               ANS0072
003700     COPY CL1TRTRD.                                               ANM0035
003710*  CASE_BILLS
003720     COPY CL1CSBLD.
003730     COPY CL6CSBLD.                                               CLM0211
003740*  CUTDTCAL
003750     COPY CWSDTCAL.
003760*  SHORTFALL_DETL/BILL_REMIT_DETL
003770     COPY CL1SFDTD.                                               CLM0211
003780     COPY CL2SFDTD.
003790     COPY CL3BRDTD.                                               CLM0054
003800*  TRANSACT_DETAILS
003810     COPY CL2TDTLD.
003820     COPY CLSTDTLD.                                               ANM0035
003830*  BILL_GRP_RULES
003840     COPY CL1BGRLD.
003850*  XREFS
003860     COPY CL1XREFD.
003870*  REINIT_ACCT_RULES                                              PFZ0113
003880     COPY CL2REARD.                                               CLM0211
003890*  CASE_UNALL_ACCT/TAX_CD_HIST/TAX_CODES                          CLM0054
003900     COPY CL1VTXHD.                                               CLM0054
003910*  PROCESS_PARAMS                                                 CLM0054
003920     COPY CL1FREQD.                                               CLM0211
003930     COPY CL1TSCHD.                                               CLM0211
003940
003950**  REVISION AREA **                                              FDM3514
003960 01  WS-REVISION           PIC X(35)  VALUE                       FDM3514
003970     ' $Revision::   1.0.1.29       $'.
003980 LINKAGE SECTION.
003990     COPY CLKFDPNC.
004000     COPY CL1CASDD.
004010     COPY CLKERRBC.
004020
004030*********************************************************
004040**   P R O C E D U R E    D I V I S I O N              **
004050*********************************************************
004060
004070 PROCEDURE DIVISION USING RMESSAGE,
004080                          CASD1-DATA,
004090                          RTN-OUTPUT-FIELDS.
004100
004110     IF FDP-DEBUG-RUNSTAT > '00'                                  FDM3514
004120        ADD +1  TO RTN-DIMT-CNT                                   FDM3514
004130        MOVE WHEN-COMPILED TO                                     FDM3514
004140             RTN-DIMT-COMPILE-DATE (RTN-DIMT-CNT)                 FDM3514
004150        MOVE WS-REVISION (16:10) TO                               FDM3514
004160             RTN-DIMT-REVISION (RTN-DIMT-CNT)                     FDM3514
004170        MOVE WS-PGM-NAME TO                                       CLM0054
004180           RTN-DIMT-PROGRAM-NAME (RTN-DIMT-CNT)                   FDM3514
004190        CALL 'CUTDIMTD' USING RMESSAGE,                           FDM3514
004200                              RTN-OUTPUT-FIELDS                   FDM3514
004210     END-IF.                                                      FDM3514
004220
004230******************************************************************
004240**                MAIN ROUTINE                                  **
004250******************************************************************
004260
004270     PERFORM A000-INITIALIZE
004280        THRU A000-INITIALIZE-EXIT.
004290
004300     IF RTN-NO-ERROR
004310        PERFORM B000-MAIN
004320           THRU B000-MAIN-EXIT
004330     END-IF.
004340
004350     IF RTN-NO-ERROR AND TDTL2-ROWS-FOUND > 0
004360        PERFORM C000-TDTL-INSERT
004370           THRU C000-EXIT
004380     END-IF.
004390
004400     IF RTN-NO-ERROR AND                                          ANM0035
004410        ((WS-PREM-RATIO > 0) OR DUE-PREM)                         ANR0101
004420        PERFORM D000-REASSURANCE-ACCT                             ANM0035
004430           THRU D000-EXIT                                         ANM0035
004440     END-IF.                                                      ANM0035
004450
004460     GOBACK.
004470
004480******************************************************************
004490**                END MAIN ROUTINE                              **
004500******************************************************************
004510
004520 A000-INITIALIZE.
004530
004540     INITIALIZE WS-ACCR-OUTPUT-ARRAY.
004550     INITIALIZE WS-TOLA-OUTPUT-ARRAY.                             CLM0211
004560     INITIALIZE WS-FDRT-ARRAY.                                    CLM0211
004570     MOVE ZEROS TO WS-ORIG-CSBL-KEY                               CLM0211
004580     MOVE ZEROS TO WS-PREM-RATIO.                                 ANR0089
004590
004600     PERFORM A050-EDIT-PARMS
004610        THRU A050-EDIT-PARMS-EXIT.
004620
004630 A000-INITIALIZE-EXIT.
004640     EXIT.
004650
004660 A050-EDIT-PARMS.
004670
004680     IF FDP-REF-KEY <= ZERO
004690        MOVE 'UT'                       TO RTN-ERR-PREFIX
004700        MOVE 32                         TO RTN-ERROR-CODE
004710        MOVE WS-PGM-NAME                TO RTN-PROG-NAME
004720        MOVE 'A050-EDIT-PARMS'          TO RTN-PARA-NAME
004730        CALL 'CUTSVARB' USING RMESSAGE,
004740                              RTN-OUTPUT-FIELDS
004750     END-IF.
004760*
004770     IF FDP-CASE-KEY <= ZERO
004780        MOVE 'UT'                       TO RTN-ERR-PREFIX
004790        MOVE 30                         TO RTN-ERROR-CODE
004800        MOVE WS-PGM-NAME                TO RTN-PROG-NAME
004810        MOVE 'A050-EDIT-PARMS'          TO RTN-PARA-NAME
004820        CALL 'CUTSVARB' USING RMESSAGE,
004830                              RTN-OUTPUT-FIELDS
004840     END-IF.
004850*
004860     IF FDP-SCAT-KEY <= ZERO  AND RTN-NO-ERROR
004870        MOVE 'UT'                       TO RTN-ERR-PREFIX
004880        MOVE 42                         TO RTN-ERROR-CODE
004890        MOVE WS-PGM-NAME                TO RTN-PROG-NAME
004900        MOVE 'A050-EDIT-PARMS'          TO RTN-PARA-NAME
004910        CALL 'CUTSVARB' USING RMESSAGE,
004920                              RTN-OUTPUT-FIELDS
004930     END-IF.
004940*
004950     IF FDP-TR-REF-NO = SPACES AND RTN-NO-ERROR
004960        MOVE 'UT'                    TO RTN-ERR-PREFIX
004970        MOVE 34                      TO RTN-ERROR-CODE
004980        MOVE WS-PGM-NAME             TO RTN-PROG-NAME
004990        MOVE 'A050-EDIT-PARMS'       TO RTN-PARA-NAME
005000        CALL 'CUTSVARB'           USING RMESSAGE,
005010                                        RTN-OUTPUT-FIELDS
005020     END-IF.
005030*
005040     IF FDP-EFF-DT = SPACES AND RTN-NO-ERROR
005050        MOVE 'UT'                 TO RTN-ERR-PREFIX
005060        MOVE 35                   TO RTN-ERROR-CODE
005070        MOVE WS-PGM-NAME          TO RTN-PROG-NAME
005080        MOVE 'A050-EDIT-PARMS'    TO RTN-PARA-NAME
005090        CALL 'CUTSVARB'        USING RMESSAGE,
005100                                     RTN-OUTPUT-FIELDS
005110     END-IF.
005120*
005130 A050-EDIT-PARMS-EXIT.
005140     EXIT.
005150
005160 B000-MAIN.
005170
005180     PERFORM B100-LOAD-TR-CODES
005190        THRU B100-EXIT.
005200
005210     IF RTN-NO-ERROR                                              PFZ0113
005220        PERFORM B150-SELECT-REINIT-ACCT-RLS                       PFZ0113
005230           THRU B150-EXIT                                         PFZ0113
005240     END-IF.                                                      PFZ0113
005250
005260     IF RTN-NO-ERROR
005270        PERFORM B200-SELECT-CASE-BILL
005280           THRU B200-EXIT
005290     END-IF.
005300
005310
005320     IF RTN-NO-ERROR
005330        PERFORM B300-PROCESS-ACCOUNTING
005340           THRU B300-EXIT
005350     END-IF.
005360
005370     IF RTN-NO-ERROR AND TOLA-BILL                                CLM0211
005380        PERFORM B350-PROCESS-TOLA-BILL                            CLM0211
005390           THRU B350-EXIT                                         CLM0211
005400     END-IF.                                                      CLM0211
005410
005420     IF RTN-NO-ERROR
005430        PERFORM B400-WRITE-NOTL-TDTLS
005440           THRU B400-EXIT
005450     END-IF.
005460
005470     IF RTN-NO-ERROR
005480        PERFORM B500-INSERT-XREFS
005490           THRU B500-EXIT
005500     END-IF.
005510
005520 B000-MAIN-EXIT.
005530     EXIT.
005540
005550 B100-LOAD-TR-CODES.
005560
005570*  LOAD ALL TR CODES ASSOCIATED WITH ACCRUAL ACCOUNTING ACTIVITY NUMS
005580*  INTO COPYBOOK LINKAGE (175,176,177,178,302,303)                PFZ0113
005590
005600     MOVE ZEROS TO TRCR2-ROWS-FOUND.                              ANS0072
005610
005620     CALL 'CB4TRTRD'  USING RMESSAGE
005630                            TRCR2-DATA                            ANS0072
005640                            RTN-OUTPUT-FIELDS.
005650
005660 B100-EXIT.
005670     EXIT.
005680
005690 B150-SELECT-REINIT-ACCT-RLS.                                     PFZ0113
005700
005710     SET NOT-ACCRUAL-TOLA TO TRUE                                 PFZ0113
005720     SET NO-OVERRIDE-ACCT TO TRUE.                                CLM0211
005730*       02=ACCRUAL ACCOUNTING      (TOLA)                         PFM0286
005740* CHANGING LOGIC TO A BLOCK SELECT                                CLM0211
005750* THIS LOGIC HAS BEEN CHANGED TO NOW SELECT ALL RECORDS OF        CLM0211
005760* TYPE '01' - ACCRUAL ACCOUNTING                                  CLM0211
005770* TYPE '02' - ACCRUAL ACCOUNTING TOLA                             CLM0211
005780* TYPE '03' - ACCRUAL ACCOUNTING BENEFIT AND PROVINCE             CLM0211
005790* ONLY 1 TYPE OF RULE SHOULD EXIST AT A GIVEN TIME.               CLM0211
005800     INITIALIZE REAR2-DATA.                                       CLM0211
005810     MOVE FDP-EFF-DT TO REAR2-EFF-DT(1).                          CLM0211
005820     MOVE '01'       TO REAR2-TYP-CD(1).                          CLM0211
005830     MOVE '02'       TO REAR2-TYP-CD(2).                          CLM0211
005840     MOVE '03'       TO REAR2-TYP-CD(3).                          CLM0211
005850     CALL 'CB1REARD' USING RMESSAGE,                              CLM0211
005860                           REAR2-DATA,                            CLM0211
005870                           RTN-OUTPUT-FIELDS.                     CLM0211
005880
005890
005900     IF RTN-NO-ERROR AND REAR2-ROWS-FOUND > 1                     CLM0211
005910        MOVE 'JL'         TO RTN-ERR-PREFIX                       CLM0211
005920        MOVE 74           TO RTN-ERROR-CODE                       CLM0211
005930        MOVE WS-PGM-NAME  TO RTN-PROG-NAME                        CLM0211
005940        MOVE 'B150'       TO RTN-PARA-NAME                        CLM0211
005950        CALL 'CUTSVARB' USING RMESSAGE,                           CLM0211
005960                              RTN-OUTPUT-FIELDS                   CLM0211
005970     END-IF.                                                      CLM0211
005980
005990     IF RTN-NO-ERROR                                              PFZ0113
006000     AND REAR2-ROWS-FOUND > 0                                     CLM0211
006010     AND REAR2-HOLD-CD(1) = '00'                                  CLM0211
006020        EVALUATE REAR2-TYP-CD(1)                                  CLM0211
006030          WHEN '02'                                               CLM0211
006040               SET ACCRUAL-TOLA TO TRUE                           PFZ0113
006050          WHEN '03'                                               CLM0211
006060               SET OVERRIDE-ACCT TO TRUE                          CLM0211
006070          WHEN OTHER                                              CLM0211
006080              CONTINUE                                            CLM0211
006090        END-EVALUATE                                              CLM0211
006100     END-IF.                                                      PFZ0113
006110
006120
006130 B150-EXIT.                                                       PFZ0113
006140     EXIT.                                                        PFZ0113
006150
006160 B200-SELECT-CASE-BILL.
006170
006180     SET NOT-TOLA-BILL    TO TRUE                                 CLM0211
006190     MOVE FDP-REF-KEY TO CSBL1-CSBL-KEY.
006200
006210     CALL 'CS1CSBLD'  USING RMESSAGE
006220                            CSBL1-DATA
006230                            RTN-OUTPUT-FIELDS.
006240     IF RTN-NO-ERROR                                              CLM0211
006250       MOVE CSBL1-DATA TO SAVE-CSBL1-DATA                         CLM0211
006260     END-IF.                                                      CLM0211
006270
006280 B200-EXIT.
006290     EXIT.
006300
006310 B300-PROCESS-ACCOUNTING.
006320
006330*  DETERMINE IF BILL QUALIFIES FOR ACCRUAL ACCOUNTING
006340     PERFORM B310-DETERMINE-ACCT-TYPE
006350        THRU B310-EXIT.
006360
006370     IF NO-ACCOUNTING OR RTN-ERROR
006380        GO TO B300-EXIT
006390     END-IF.
006400
006410     PERFORM B320-DETERMINE-PREM-RATIO
006420        THRU B320-EXIT.
006430
006440     IF RTN-NO-ERROR
006450        PERFORM B330-LOAD-ARRAY
006460           THRU B330-EXIT
006470     END-IF.
006480
006490 B300-EXIT.
006500     EXIT.
006510
006520 B310-DETERMINE-ACCT-TYPE.
006530
006540     SET NO-ACCOUNTING TO TRUE.
006550     MOVE SPACES       TO WS-BEG-DT.
006560
006570*    IF BILL IS PAID
006580     IF CSBL1-RCVB-STAT-CD = '01' OR '02' OR '08'
006590        IF CSBL1-BILL-DUE-DT > REAR2-NXT-REINIT-DT(1)             CLM0211
006600           SET ADVANCE-PREM TO TRUE
006610           MOVE 175 TO WS-ACT-NUM
006620        ELSE
006630*    NEED TO ADD A DAY TO ACCT MONTH END DATE
006640*    RESULT STORED AS WS-BEG-DT                                   CLM0211
006650           PERFORM D500-CALC-AMCD                                 CLM0211
006660              THRU D500-EXIT                                      CLM0211
006670           IF RTN-NO-ERROR
006680              IF CSBL1-BEG-BILL-DT <= REAR2-NXT-REINIT-DT(1)      CLM0211
006690                 AND CSBL1-NXT-BILL-DT > WS-BEG-DT
006700                 SET UNEARNED-PREM TO TRUE
006710                 MOVE 176 TO WS-ACT-NUM
006720              END-IF
006730           END-IF
006740        END-IF
006750*    IF BILL IS NOT PAID
006760     ELSE
006770        IF CSBL1-BILL-DUE-DT <= REAR2-NXT-REINIT-DT(1)            PFM0286
006780           SET DUE-PREM TO TRUE
006790           MOVE 177 TO WS-ACT-NUM
006800        END-IF
006810     END-IF.
006820
006830 B310-EXIT.
006840     EXIT.
006850
006860 B320-DETERMINE-PREM-RATIO.
006870
006880     MOVE ZEROS TO WS-PREM-RATIO.
006890*  THIS IS THE PREM RATIO BASED ON BILL DATES                     CLM0211
006900*  ADVANCE PREM RATION IS ALWAYS 1.0
006910     IF ADVANCE-PREM
006920        MOVE 1 TO WS-PREM-RATIO
006930        GO TO B320-EXIT
006940     END-IF.
006950
006960     IF WS-BEG-DT = SPACES
006970        PERFORM D500-CALC-AMCD                                    CLM0211
006980           THRU D500-EXIT                                         CLM0211
006990     END-IF.
007000
007010*  IF NXT-BILL-DT BEFORE AMCD + 1, RATIO IS 0
007020     IF RTN-NO-ERROR
007030        IF CSBL1-NXT-BILL-DT <= WS-BEG-DT
007040           GO TO B320-EXIT
007050        END-IF
007060     END-IF.
007070
007080
007090     IF RTN-NO-ERROR
007100        INITIALIZE DATEPARMS
007110        IF UNEARNED-PREM AND ACCRUAL-TOLA                         PFZ0113
007120        AND NO-OVERRIDE-ACCT                                      CLM0211
007130*  GET DAYS DIFF BETWEEN BEG-BILL-DT AND AMCD
007140           MOVE CSBL1-BEG-BILL-DT TO INDATE                       PFZ0113
007150           MOVE WS-BEG-DT         TO INDATE-1                     PFZ0113
007160        ELSE                                                      PFZ0113
007170*  GET DAYS DIFF BETWEEN NXT-BILL-DT AND AMCD
007180           MOVE WS-BEG-DT         TO INDATE
007190           MOVE CSBL1-NXT-BILL-DT TO INDATE-1
007200        END-IF                                                    PFZ0113
007210        MOVE 310               TO REQUEST-TYPE
007220        PERFORM X000-CALL-CUTDTCAL
007230           THRU X000-EXIT
007240        MOVE DAYS-DIFF TO WS-DAYS-DIFF-1
007250
007260*  GET DAYS DIFF BETWEEN BEG-BILL-DT AND NXT-BILL-DT
007270        INITIALIZE DATEPARMS
007280        MOVE CSBL1-BEG-BILL-DT TO INDATE
007290        MOVE CSBL1-NXT-BILL-DT TO INDATE-1
007300        MOVE 310               TO REQUEST-TYPE
007310        PERFORM X000-CALL-CUTDTCAL
007320           THRU X000-EXIT
007330        MOVE DAYS-DIFF TO WS-DAYS-DIFF-2
007340
007350        IF RTN-NO-ERROR
007360           COMPUTE WS-PREM-RATIO ROUNDED =
007370              WS-DAYS-DIFF-1/WS-DAYS-DIFF-2
007380           END-COMPUTE
007390        END-IF
007400     END-IF.
007410
007420 B320-EXIT.
007430     EXIT.
007440
007450 B330-LOAD-ARRAY.
007460
007470*  FETCH ALL ACTIVE OR PAID BILL REMIT/SHORTFALL DETAILS
007480
007490     PERFORM B332-FETCH-BRDT
007500        THRU B332-EXIT.
007510*    FOR OVERRIDE ACCT PRORATION DONE AS EACH DETAIL PROCESSED    CLM0211
007520     IF RTN-NO-ERROR AND NO-OVERRIDE-ACCT                         CLM0211
007530        PERFORM B336-PRORATE-PREMIUM
007540           THRU B336-EXIT
007550     END-IF.
007560
007570 B330-EXIT.
007580     EXIT.
007590
007600 B332-FETCH-BRDT.
007610
007620*    IF DUE-PREM, WE WILL HAVE TO LOOP THROUGH BRDT/SFDT RECS TWICE,
007630*    ONCE FOR ACCRUAL PORTION AND ONCE FOR TOTAL AMOUNT.  THERE IS A
007640*    SEPARATE TR ACTIVITY NUM FOR EACH TYPE.
007650
007660     IF DUE-PREM
007670     OR (ACCRUAL-TOLA AND (ADVANCE-PREM OR UNEARNED-PREM)         CLM0211
007680        AND NO-OVERRIDE-ACCT)                                     CLM0211
007690*       IF TOLERANCE BILLING IS USED, NEED TO CONSIDER EFFECT OF  PFZ0113
007700*       SHORTFALL DETAILS ON ADVANCE AND UNEARNED PREMIUM         PFZ0113
007710        MOVE 2 TO WS-PROC-IND
007720     ELSE
007730        MOVE 1 TO WS-PROC-IND
007740     END-IF.
007750
007760     IF NO-OVERRIDE-ACCT                                          CLM0054
007770        PERFORM B333-NO-OVERRIDE-ACCT                             CLM0054
007780           THRU B333-EXIT                                         CLM0054
007790     ELSE                                                         CLM0054
007800        IF ADVANCE-PREM                                           CLM0211
007810           PERFORM C200-GET-SPAN-PRD                              CLM0211
007820              THRU C200-EXIT                                      CLM0211
007830        END-IF                                                    CLM0211
007840        IF RTN-NO-ERROR                                           CLM0211
007850           PERFORM B334-OVERRIDE-ACCT                             CLM0054
007860              THRU B334-EXIT                                      CLM0054
007870        END-IF                                                    CLM0211
007880     END-IF.                                                      CLM0054
007890                                                                  CLM0054
007900 B332-EXIT.
007910     EXIT.
007920
007930 B333-NO-OVERRIDE-ACCT.
007940
007950     INITIALIZE SFDT2-DATA.
007960
007970     PERFORM UNTIL SFDT2-END-FLAG = 'Y' OR RTN-ERROR              CLM0054
007980        MOVE FDP-REF-KEY    TO SFDT2-CSBL-KEY (1)                 CLM0054
007990*    'BS' - TRANSACTION EXCLUSION FROM ACCRUAL ACCOUNTING         CLM0159
008000        MOVE 'BS'           TO SFDT2-GRP-TYP-CD                   CLM0159
008010
008020        CALL 'CFLBRDTD'  USING RMESSAGE
008030                               SFDT2-DATA
008040                               RTN-OUTPUT-FIELDS                  CLM0054
008050
008060        PERFORM VARYING BRDTX FROM 1 BY 1
008070           UNTIL BRDTX > SFDT2-ROWS-FOUND OR RTN-ERROR
008080
008090           SET FIRST-REC TO TRUE
008100           SET DONT-SKIP-REC TO TRUE                              ANS0208
008110
008120           IF SFDT2-PROC-AMT(BRDTX) > 0
008130
008140*       MAKE SHORTFALL AMOUNTS NEGATIVE
008150              IF SFDT2-BILL-REMIT-CD(BRDTX) = 'S'
008160                 IF DUE-PREM                                      PFZ0113
008170                 OR (ACCRUAL-TOLA                                 PFZ0113
008180                     AND (ADVANCE-PREM OR UNEARNED-PREM))         PFZ0113
008190*                  SFDT ONLY USED FOR DUE-PREM                    ANS0208
008200*                  OR FOR ADVANCE & UNEARNED PREMIUM              PFZ0113
008210*                     WHEN TOLERANCE BILLING IS USED              PFZ0113
008220                    COMPUTE SFDT2-PROC-AMT(BRDTX) =
008230                         SFDT2-PROC-AMT(BRDTX) * -1
008240*       ONLY NEED TO LOOP 1X FOR SFDT RECORD IN D&U TOTAL         ANS0208
008250                    MOVE 1 TO WS-PROC-IND                         ANS0208
008260                 ELSE                                             PFZ0113
008270                    SET SKIP-REC TO TRUE                          ANS0208
008280                 END-IF                                           ANS0208
008290              END-IF
008300
008310              IF SFDT2-BILL-REMIT-CD(BRDTX) = 'B'                 PFZ0113
008320              AND ACCRUAL-TOLA AND ADVANCE-PREM                   PFZ0113
008330*       ONLY NEED TO LOOP 1X FOR BRDT RECORD IN ADVANCE           PFZ0113
008340                    MOVE 1 TO WS-PROC-IND                         PFZ0113
008350              END-IF
008360
008370              PERFORM VARYING DUEUX FROM 1 BY 1
008380                 UNTIL DUEUX > WS-PROC-IND OR RTN-ERROR           ANS0208
008390                    OR SKIP-REC                                   CLM0054
008400
008410*    WE ARE ASSIGNING THE ACCRUAL TR CODES TO FIRST-REC
008420*    THE D&U TOTAL ARRAY RECORDS WILL BE SECOND-REC
008430*    WE NEED THIS DISTINCTION FOR WRITTING OUT THE TDTLS LATER ON
008440
008450*    178 IS TRTR ACTIVITY NUM FOR D&U - TOTAL
008460*    302 IS TRTR ACTIVITY NUM FOR ADVANCE PREM TOTAL              PFZ0113
008470*    303 IS TRTR ACTIVITY NUM FOR UNEARNED PREM COLLECTED TOTAL   PFZ0113
008480                 IF FIRST-REC AND SFDT2-BILL-REMIT-CD(BRDTX) = 'B'ANS0196
008490                    OR (SFDT2-BILL-REMIT-CD(BRDTX) = 'S' AND      USM0065A
008500                        UNEARNED-PREM)                            USM0065A
008510                    MOVE WS-ACT-NUM TO WS-ACT-NUM-2
008520                 ELSE
008530                    EVALUATE TRUE                                 PFZ0113
008540                    WHEN DUE-PREM                                 PFZ0113
008550                       MOVE 178 TO WS-ACT-NUM-2
008560                    WHEN ADVANCE-PREM                             PFZ0113
008570                       MOVE 302 TO WS-ACT-NUM-2                   PFZ0113
008580                    WHEN UNEARNED-PREM                            PFZ0113
008590                       MOVE 303 TO WS-ACT-NUM-2                   PFZ0113
008600                    END-EVALUATE                                  PFZ0113
008610                 END-IF
008620
008630                 SET TR-CD-NOT-FOUND TO TRUE
008640                 PERFORM VARYING TRTRX FROM 1 BY 1
008650                    UNTIL TRTRX > TRCR2-ROWS-FOUND OR TR-CD-FOUND ANS0072
008660                                  OR RTN-ERROR
008670
008680
008690                    IF WS-ACT-NUM-2 = TRCR2-ACTIVITY-NUM(TRTRX)   CLM0054
008700                      AND SFDT2-TR-CD(BRDTX) = TRCR2-TR-CD(TRTRX) CLM0054
008710                       IF TRCR2-TDTL-TR-CD(TRTRX) > SPACES        ANS0076
008720                          MOVE TRCR2-TDTL-TR-CD(TRTRX)            ANH0072
008730                                             TO WS-TDTL-TR-CD     ANH0072
008740                          SET TR-CD-FOUND TO TRUE
008750                       ELSE
008760*  ERROR - TR CODE IN TDTL TRNCH MUST BE SETUP
008770                          MOVE 'UT'          TO RTN-ERR-PREFIX    ANS0076
008780                          MOVE 79            TO RTN-ERROR-CODE    ANS0076
008790                          MOVE WS-PGM-NAME   TO RTN-PROG-NAME     ANS0076
008800                          MOVE 'B333-NO-OVERRIDE-ACCT'            ANS0076
008810                                             TO RTN-PARA-NAME     CLM0211
008820                          MOVE SFDT2-TR-CD(BRDTX) TO WS-TR-CD     ANS0076
008830                          MOVE WS-ACT-NUM-2  TO WS-ACT-NUM-3      ANS0076
008840                          MOVE 1             TO RTN-VAR-SEQ-NO (1)ANS0076
008850                          MOVE WS-ERROR2     TO RTN-VAR-TEXT (1)  ANS0076
008860                          MOVE 2             TO RTN-VAR-SEQ-NO (2)ANS0076
008870                          MOVE WS-ERROR3     TO RTN-VAR-TEXT (2)  ANS0076
008880                          MOVE 2             TO RTN-VAR-DETL-CNT  ANS0076
008890                          CALL 'CUTSVARB' USING RMESSAGE,         ANS0076
008900                                                RTN-OUTPUT-FIELDS ANS0076
008910                       END-IF
008920                    END-IF
008930
008940                 END-PERFORM
008950
008960                 IF TR-CD-NOT-FOUND AND RTN-NO-ERROR              ANS0072
008970                    MOVE 'UT'     TO RTN-ERR-PREFIX
008980                    MOVE 1257     TO RTN-ERROR-CODE
008990                    MOVE WS-PGM-NAME TO RTN-PROG-NAME
009000                    MOVE 'B333-NO-OVERRIDE-ACCT' TO RTN-PARA-NAME CLM0211
009010                    MOVE SFDT2-TR-CD(BRDTX) TO WS-TR-CD           ANS0072
009020                    MOVE WS-ACT-NUM-2       TO WS-ACT-NUM-3       PFZ0113
009030                    MOVE 1                  TO RTN-VAR-SEQ-NO (1) ANS0072
009040                    MOVE WS-ERROR2          TO RTN-VAR-TEXT (1)   ANS0072
009050                    MOVE 1                  TO RTN-VAR-DETL-CNT   ANS0072
009060                    CALL 'CUTSVARB' USING RMESSAGE,
009070                                          RTN-OUTPUT-FIELDS
009080                 END-IF
009090
009100
009110                 SET ROW-NOT-FOUND TO TRUE
009120                 PERFORM VARYING SUB0 FROM 1 BY 1
009130                    UNTIL SUB0 > WS-ACCR-COUNT OR RTN-ERROR
009140                                 OR ROW-FOUND
009150
009160
009170                    IF WS-TDTL-TR-CD = WS-OUTPUT-TR-CD(SUB0)
009180                       IF ADVANCE-PREM                            PFZ0113
009190                       AND SFDT2-BILL-REMIT-CD(BRDTX) = 'S'       PFZ0113
009200*       ADV PREM 'A' TYPE SHOULD HAVE ONLY THE SUM OF SHORTFALL   PFZ0113
009210*       NEED TO SUBTRACT THIS NEGATIVE NUMBER FOR POSITIVE SUM    PFZ0113
009220                          COMPUTE WS-ACCR-AMT(SUB0)               PFM0324
009230                                = WS-ACCR-AMT(SUB0)               PFM0324
009240                                  - SFDT2-PROC-AMT(BRDTX)         PFZ0113
009250                       ELSE                                       PFZ0113
009260                          COMPUTE WS-ACCR-AMT(SUB0)               PFM0324
009270                                = WS-ACCR-AMT(SUB0)               PFM0324
009280                                  + SFDT2-PROC-AMT(BRDTX)         PFZ0113
009290                       END-IF                                     PFZ0113
009300                      SET ROW-FOUND TO TRUE
009310                    END-IF
009320                 END-PERFORM
009330
009340                 IF ROW-NOT-FOUND AND RTN-NO-ERROR                ANS0076
009350                    IF WS-PREM-RATIO > 0 OR                       ANR0101
009360                       SECOND-REC                                 PFZ0113
009370
009380                       IF WS-ACCR-COUNT < 5000                    CLM0211
009390                          ADD +1 TO WS-ACCR-COUNT
009400                          MOVE WS-ACCR-COUNT TO SUB1
009410                          MOVE WS-TDTL-TR-CD TO
009420                                           WS-OUTPUT-TR-CD(SUB1)
009430                          IF ADVANCE-PREM                         PFZ0113
009440                          AND SFDT2-BILL-REMIT-CD(BRDTX) = 'S'    PFZ0113
009450*          NEED TO SUBTRACT THIS NEGATIVE NUMBER FOR POSITIVE SUM PFZ0113
009460                             COMPUTE WS-ACCR-AMT(SUB1)            PFZ0113
009470                                   = WS-ACCR-AMT(SUB1)            PFZ0113
009480                                     - SFDT2-PROC-AMT(BRDTX)      PFZ0113
009490                          ELSE                                    PFZ0113
009500                             COMPUTE WS-ACCR-AMT(SUB1)            PFZ0113
009510                                   = WS-ACCR-AMT(SUB1)            PFZ0113
009520                                     + SFDT2-PROC-AMT(BRDTX)      PFZ0113
009530                          END-IF                                  PFZ0113
009540                       ELSE                                       CLM0211
009550*                         ERROR - ARRAY SIZE EXCEEDED             CLM0211
009560                           MOVE 'UT'         TO RTN-ERR-PREFIX    CLM0211
009570                           MOVE 3028         TO RTN-ERROR-CODE    CLM0211
009580                           MOVE WS-PGM-NAME  TO RTN-PROG-NAME     CLM0211
009590                           MOVE 'B333-NO-OVERRIDE-ACCT'           CLM0211
009600                                             TO RTN-PARA-NAME     CLM0211
009610                           MOVE SFDT2-TR-CD(BRDTX) TO WS-TR-CD    CLM0211
009620                           MOVE WS-ACT-NUM-2 TO WS-ACT-NUM-3      CLM0211
009630                           MOVE 1            TO RTN-VAR-SEQ-NO (1)CLM0211
009640                           MOVE WS-ERROR2    TO RTN-VAR-TEXT (1)  CLM0211
009650                           MOVE 2            TO RTN-VAR-SEQ-NO (2)CLM0211
009660                           MOVE WS-ERROR3    TO RTN-VAR-TEXT (2)  CLM0211
009670                           MOVE 2            TO RTN-VAR-DETL-CNT  CLM0211
009680                           CALL 'CUTSVARB' USING RMESSAGE,        CLM0211
009690                                                RTN-OUTPUT-FIELDS CLM0211
009700                       END-IF                                     CLM0211
009710
009720                       IF FIRST-REC                               PFZ0113
009730                          MOVE 'A' TO WS-ACCR-IND(SUB1)
009740                       ELSE
009750                          MOVE 'U' TO WS-ACCR-IND(SUB1)
009760                       END-IF
009770                    END-IF
009780                 END-IF
009790
009800*  WNT TO EXIT LOOP IF SECOND-REC                                 ANS0196
009810                 IF SECOND-REC                                    ANS0196
009820                    MOVE +3 TO DUEUX                              ANS0196
009830                 END-IF                                           ANS0196
009840
009850                 SET SECOND-REC TO TRUE
009860              END-PERFORM
009870           END-IF
009880        END-PERFORM                                               CLM0054
009890     END-PERFORM.
009900
009910 B333-EXIT.                                                       CLM0054
009920     EXIT.                                                        CLM0054
009930
009940 B334-OVERRIDE-ACCT.                                              CLM0054
009950                                                                  CLM0054
009960     PERFORM C100-GET-PROVINCES                                   CLM0054
009970        THRU C100-EXIT.                                           CLM0054
009980                                                                  CLM0054
009990     INITIALIZE BRDT3-DATA.                                       CLM0054
010000
010010     PERFORM UNTIL BRDT3-END-FLAG = 'Y'                           CLM0054
010020        OR RTN-ERROR                                              CLM0054
010030        MOVE FDP-REF-KEY    TO BRDT3-CSBL-KEY (1)                 CLM0054
010040*    'BS' - TRANSACTION EXCLUSION FROM ACCRUAL ACCOUNTING         CLM0159
010050        MOVE 'BS'           TO BRDT3-GRP-TYP-CD                   CLM0159
010060        CALL 'CFFBRDTD'  USING RMESSAGE                           CLM0054
010070                               BRDT3-DATA                         CLM0054
010080                               RTN-OUTPUT-FIELDS                  CLM0054
010090                                                                  CLM0054
010100        PERFORM VARYING BRDTX FROM 1 BY 1                         CLM0054
010110           UNTIL BRDTX > BRDT3-ROWS-FOUND OR RTN-ERROR            CLM0054
010120                                                                  CLM0054
010130           SET FIRST-REC TO TRUE                                  CLM0054
010140           SET DONT-SKIP-REC TO TRUE                              CLM0054
010150*   IF THE BILL HAS ANY TOLA DETAIL SET TOLA-BILL PROCESSING FLAG CLM0211
010160           IF BRDT3-REAS-REF-CD (BRDTX) = 'TOLA'                  CLM0211
010170              SET TOLA-BILL      TO TRUE                          CLM0211
010180           END-IF                                                 CLM0211
010190           IF BRDT3-TOT-SUBM-AMT(BRDTX) > 0                       CLM0054
010200                                                                  CLM0054
010210*    MAKE SHORTFALL AMOUNTS NEGATIVE                              CLM0054
010220              IF BRDT3-BILL-REMIT-CD(BRDTX) = 'S'                 CLM0054
010230                 COMPUTE BRDT3-TOT-SUBM-AMT(BRDTX) =              CLM0054
010240                    BRDT3-TOT-SUBM-AMT(BRDTX) * -1                CLM0054
010250              END-IF                                              CLM0054
010260                                                                  CLM0054
010270*             CALCULATE PREMIUM RATIO BASED ON DETAIL BILL DUE DT CLM0211
010280              PERFORM B340-DETAIL-PREM-RATIO                      CLM0211
010290                 THRU B340-EXIT                                   CLM0211
010300                                                                  CLM0054
010310              PERFORM VARYING DUEUX FROM 1 BY 1                   CLM0054
010320                 UNTIL DUEUX > WS-PROC-IND OR RTN-ERROR           CLM0054
010330                              OR SKIP-REC                         CLM0054
010340                                                                  CLM0054
010350                 PERFORM B338-ACCUMULATE-PREM                     CLM0211
010360                    THRU B338-EXIT                                CLM0211
010370                 SET SECOND-REC TO TRUE                           CLM0054
010380              END-PERFORM                                         CLM0054
010390           END-IF                                                 CLM0054
010400        END-PERFORM                                               CLM0054
010410     END-PERFORM.                                                 CLM0054
010420                                                                  CLM0054
010430 B334-EXIT.                                                       CLM0054
010440     EXIT.                                                        CLM0054
010450
010460 B336-PRORATE-PREMIUM.
010470
010480   PERFORM VARYING SUB0 FROM 1 BY 1
010490      UNTIL SUB0 > WS-ACCR-COUNT
010500
010510      IF WS-ACCR-AMT(SUB0) < 0
010520        MOVE CSBL1-TR-REF-NO   TO WS-TR-REF-NO                    CLM0211
010530        MOVE 'JL'              TO RTN-ERR-PREFIX
010540        MOVE 70                TO RTN-ERROR-CODE
010550        MOVE WS-PGM-NAME       TO RTN-PROG-NAME
010560        MOVE 'B336-PRRT-PREM'  TO RTN-PARA-NAME
010570        MOVE 1                 TO RTN-VAR-SEQ-NO (1)
010580        MOVE WS-ERROR1         TO RTN-VAR-TEXT (1)
010590        MOVE 1                 TO RTN-VAR-DETL-CNT
010600
010610        CALL 'CUTSVARB' USING RMESSAGE,
010620                              RTN-OUTPUT-FIELDS
010630      END-IF
010640
010650      COMPUTE WS-ACCR-PRORATE-AMT(SUB0) ROUNDED = WS-PREM-RATIO *
010660              WS-ACCR-AMT(SUB0)
010670      END-COMPUTE
010680
010690   END-PERFORM.
010700
010710 B336-EXIT.
010720     EXIT.
010730
010740 B338-ACCUMULATE-PREM.                                            CLM0211
010750* CODE MOVED FROM B334 TO CALLED PARAGRAPH SO WE CAN GO TO EXIT   CLM0211
010760*    WE ARE ASSIGNING THE ACCRUAL TR CODES TO FIRST-REC           CLM0054
010770*    THE TOTAL ARRAY RECORDS WILL BE SECOND-REC                   CLM0211
010780*    WE NEED THIS DISTINCTION FOR WRITING OUT THE TDTLS LATER ON  CLM0211
010790                                                                  CLM0054
010800*    178 IS TRTR ACTIVITY NUM FOR D&U - TOTAL                     CLM0054
010810*    302 IS TRTR ACTIVITY NUM FOR ADVANCE PREM TOTAL              CLM0054
010820*    303 IS TRTR ACTIVITY NUM FOR UNEARNED PREM COLLECTED TOTAL   CLM0054
010830     IF FIRST-REC AND NOT                                         CLM0211
010840        (BRDT3-BILL-REMIT-CD(BRDTX) = 'S' AND                     CLM0211
010850            ADVANCE-PREM)                                         CLM0211
010860        MOVE WS-ACT-NUM TO WS-ACT-NUM-2                           CLM0054
010870     ELSE                                                         CLM0054
010880        EVALUATE TRUE                                             CLM0054
010890        WHEN DUE-PREM                                             CLM0054
010900           MOVE 178 TO WS-ACT-NUM-2                               CLM0054
010910        WHEN ADVANCE-PREM                                         CLM0054
010920           MOVE 302 TO WS-ACT-NUM-2                               CLM0054
010930        WHEN UNEARNED-PREM                                        CLM0054
010940           MOVE 303 TO WS-ACT-NUM-2                               CLM0054
010950        END-EVALUATE                                              CLM0054
010960     END-IF                                                       CLM0054
010970                                                                  CLM0054
010980     SET NOT-DETL-UNEARNED-PREM TO TRUE                           CLM0211
010990*    FOR SOME ACTIVITIES BACKDATED ADJUSTMENTS ARE EXCLUDED       CLM0211
011000     IF BRDT3-BILL-DUE-DT(BRDTX) < CSBL1-BEG-BILL-DT              CLM0211
011010        EVALUATE WS-ACT-NUM-2                                     CLM0211
011020        WHEN 177                                                  CLM0211
011030        WHEN 176                                                  CLM0211
011040           GO TO B338-EXIT                                        CLM0211
011050*       FOR ADVANCED PREMIUM ACTIVITIES EXCLUSION MORE COMPLICATEDCLM0211
011060        WHEN 175                                                  CLM0211
011070        WHEN 302                                                  CLM0211
011080           IF BRDT3-BILL-DUE-DT(BRDTX) < REAR2-NXT-REINIT-DT(1)   CLM0211
011090              IF BRDT3-BILL-DUE-DT(BRDTX) < WS-SPAN-BEG-DT        CLM0211
011100                 GO TO B338-EXIT                                  CLM0211
011110              ELSE                                                CLM0211
011120                 SET DETL-UNEARNED-PREM TO TRUE                   CLM0211
011130                 MOVE 176 TO WS-ACT-NUM-2                         CLM0211
011140              END-IF                                              CLM0211
011150           END-IF                                                 CLM0211
011160        END-EVALUATE                                              CLM0211
011170     END-IF.                                                      CLM0211
011180
011190     SET TR-CD-NOT-FOUND TO TRUE                                  CLM0054
011200     PERFORM VARYING TRTRX FROM 1 BY 1                            CLM0054
011210        UNTIL TRTRX > TRCR2-ROWS-FOUND OR TR-CD-FOUND             CLM0054
011220                      OR RTN-ERROR                                CLM0054
011230                                                                  CLM0054
011240       IF WS-ACT-NUM-2 = TRCR2-ACTIVITY-NUM(TRTRX) AND            CLM0054
011250           BRDT3-TR-CD(BRDTX) = TRCR2-TR-CD(TRTRX)                CLM0054
011260           IF TRCR2-TDTL-TR-CD(TRTRX) > SPACES                    CLM0054
011270              MOVE TRCR2-TDTL-TR-CD(TRTRX)                        CLM0054
011280                                 TO WS-TDTL-TR-CD                 CLM0054
011290              SET TR-CD-FOUND TO TRUE                             CLM0054
011300           ELSE                                                   CLM0054
011310*    ERROR - TR CODE IN TDTL TRNCH MUST BE SETUP                  CLM0054
011320              MOVE 'UT'                    TO RTN-ERR-PREFIX      CLM0054
011330              MOVE 79                      TO RTN-ERROR-CODE      CLM0054
011340              MOVE WS-PGM-NAME             TO RTN-PROG-NAME       CLM0054
011350              MOVE 'B338-ACCUMULATE-PREM'  TO RTN-PARA-NAME       CLM0211
011360              MOVE BRDT3-TR-CD(BRDTX)      TO WS-TR-CD            CLM0054
011370              MOVE WS-ACT-NUM-2            TO WS-ACT-NUM-3        CLM0054
011380              MOVE 1                       TO RTN-VAR-SEQ-NO (1)  CLM0054
011390              MOVE WS-ERROR2               TO RTN-VAR-TEXT (1)    CLM0054
011400              MOVE 2                       TO RTN-VAR-SEQ-NO (2)  CLM0054
011410              MOVE WS-ERROR3               TO RTN-VAR-TEXT (2)    CLM0054
011420              MOVE 2                       TO RTN-VAR-DETL-CNT    CLM0054
011430              CALL 'CUTSVARB'           USING RMESSAGE,           CLM0054
011440                                              RTN-OUTPUT-FIELDS   CLM0054
011450           END-IF                                                 CLM0054
011460        END-IF                                                    CLM0054
011470                                                                  CLM0054
011480     END-PERFORM                                                  CLM0054
011490                                                                  CLM0054
011500     IF TR-CD-NOT-FOUND AND RTN-NO-ERROR                          CLM0054
011510        MOVE 'UT'                   TO RTN-ERR-PREFIX             CLM0054
011520        MOVE 1257                   TO RTN-ERROR-CODE             CLM0054
011530        MOVE WS-PGM-NAME            TO RTN-PROG-NAME              CLM0054
011540        MOVE 'B338-ACCUMULATE-PREM' TO RTN-PARA-NAME              CLM0211
011550        MOVE BRDT3-TR-CD(BRDTX)     TO WS-TR-CD                   CLM0054
011560        MOVE WS-ACT-NUM-2           TO WS-ACT-NUM-3               CLM0054
011570        MOVE 1                      TO RTN-VAR-SEQ-NO (1)         CLM0054
011580        MOVE WS-ERROR2              TO RTN-VAR-TEXT (1)           CLM0054
011590        MOVE 2                      TO RTN-VAR-SEQ-NO (2)         CLM0211
011600        MOVE WS-ERROR3              TO RTN-VAR-TEXT (2)           CLM0211
011610        MOVE 2                      TO RTN-VAR-DETL-CNT           CLM0211
011620        CALL 'CUTSVARB'          USING RMESSAGE,                  CLM0054
011630                                       RTN-OUTPUT-FIELDS          CLM0054
011640     END-IF                                                       CLM0054
011650                                                                  CLM0054
011660     SET ROW-NOT-FOUND TO TRUE                                    CLM0054
011670     PERFORM VARYING SUB0 FROM 1 BY 1                             CLM0054
011680        UNTIL SUB0 > WS-ACCR-COUNT OR RTN-ERROR                   CLM0054
011690                     OR ROW-FOUND                                 CLM0054
011700                                                                  CLM0054
011710        IF WS-TDTL-TR-CD              = WS-OUTPUT-TR-CD(SUB0)     CLM0054
011720         AND BRDT3-FDRT-KEY(BRDTX)    = WS-FDRT-KEY(SUB0)         CLM0054
011730         AND BRDT3-TAX-ID-CD(BRDTX)   = WS-TAX-ID-CD(SUB0)        CLM0054
011740         AND BRDT3-REAS-REF-CD(BRDTX) = WS-REAS-REF-CD(SUB0)      CLM0211
011750           PERFORM B342-ADD-DETAIL-TO-BUCKET                      CLM0211
011760              THRU B342-EXIT                                      CLM0211
011770           SET ROW-FOUND TO TRUE                                  CLM0054
011780        END-IF                                                    CLM0054
011790     END-PERFORM                                                  CLM0054
011800                                                                  CLM0054
011810     IF ROW-NOT-FOUND AND RTN-NO-ERROR                            CLM0054
011820       IF WS-PREM-RATIO > 0 OR                                    CLM0054
011830                   SECOND-REC                                     CLM0054
011840          IF WS-ACCR-COUNT < 5000                                 CLM0054
011850             ADD +1 TO WS-ACCR-COUNT                              CLM0054
011860             MOVE WS-ACCR-COUNT          TO SUB0                  CLM0211
011870             MOVE WS-TDTL-TR-CD          TO WS-OUTPUT-TR-CD(SUB0) CLM0211
011880             MOVE BRDT3-FDRT-KEY(BRDTX)  TO  WS-FDRT-KEY(SUB0)    CLM0211
011890             MOVE BRDT3-TAX-ID-CD(BRDTX) TO  WS-TAX-ID-CD(SUB0)   CLM0211
011900             MOVE BRDT3-REAS-REF-CD(BRDTX)                        CLM0211
011910                                         TO WS-REAS-REF-CD(SUB0)  CLM0211
011920          ELSE                                                    CLM0054
011930*            ERROR - ARRAY SIZE EXCEEDED                          CLM0054
011940              MOVE 'UT'                    TO RTN-ERR-PREFIX      CLM0054
011950              MOVE 3028                    TO RTN-ERROR-CODE      CLM0054
011960              MOVE WS-PGM-NAME             TO RTN-PROG-NAME       CLM0054
011970              MOVE 'B338-ACCUMULATE-PREM'  TO RTN-PARA-NAME       CLM0211
011980              MOVE BRDT3-TR-CD(BRDTX)      TO WS-TR-CD            CLM0054
011990              MOVE WS-ACT-NUM-2            TO WS-ACT-NUM-3        CLM0054
012000              MOVE 1                       TO RTN-VAR-SEQ-NO (1)  CLM0054
012010              MOVE WS-ERROR2               TO RTN-VAR-TEXT (1)    CLM0054
012020              MOVE 2                       TO RTN-VAR-SEQ-NO (2)  CLM0054
012030              MOVE WS-ERROR3               TO RTN-VAR-TEXT (2)    CLM0054
012040              MOVE 2                       TO RTN-VAR-DETL-CNT    CLM0054
012050              CALL 'CUTSVARB'           USING RMESSAGE,           CLM0054
012060                                              RTN-OUTPUT-FIELDS   CLM0054
012070          END-IF                                                  CLM0054
012080
012090          PERFORM B342-ADD-DETAIL-TO-BUCKET                       CLM0211
012100             THRU B342-EXIT                                       CLM0211
012110
012120          IF FIRST-REC                                            CLM0054
012130             MOVE 'P' TO WS-ACCR-IND(SUB0)                        CLM0211
012140          ELSE                                                    CLM0054
012150             MOVE 'T' TO WS-ACCR-IND(SUB0)                        CLM0211
012160          END-IF                                                  CLM0054
012170       END-IF                                                     CLM0054
012180    END-IF                                                        CLM0054
012190                                                                  CLM0054
012200*       WANT TO EXIT LOOP IF SECOND-REC                           CLM0054
012210    IF SECOND-REC                                                 CLM0054
012220       MOVE +3 TO DUEUX                                           CLM0054
012230    END-IF.                                                       CLM0211
012240                                                                  CLM0054
012250 B338-EXIT.                                                       CLM0211
012260     EXIT.                                                        CLM0211
012270
012280 B340-DETAIL-PREM-RATIO.                                          CLM0211
012290
012300     MOVE ZEROS TO WS-DETL-RATIO.                                 CLM0211
012310*  THIS IS THE PREM RATIO BASED ON DETAIL BILL DUE DATE           CLM0211
012320*  USED FOR UNEARNED PREMIUM INCLUDING D&U UNEARNED               CLM0211
012330
012340*  IF NXT-BILL-DT BEFORE AMCD + 1, RATIO IS 0 ONLY REPORT TOTALS  CLM0211
012350     IF CSBL1-NXT-BILL-DT <= WS-BEG-DT                            CLM0211
012360        GO TO B340-EXIT                                           CLM0211
012370     END-IF.                                                      CLM0211
012380     IF NOT ADVANCE-PREM                                          CLM0211
012390        MOVE CSBL1-NXT-BILL-DT TO WS-SPAN-NXT-DT                  CLM0211
012400     END-IF.                                                      CLM0211
012410
012420     IF BRDT3-BILL-DUE-DT(BRDTX) < REAR2-NXT-REINIT-DT(1)         CLM0211
012430*     GET DAYS DIFF BETWEEN DETAIL BILL-DUE-DT AND NXT-BILL-DT    CLM0211
012440        INITIALIZE DATEPARMS                                      CLM0211
012450        MOVE BRDT3-BILL-DUE-DT(BRDTX) TO INDATE                   CLM0211
012460        MOVE WS-SPAN-NXT-DT     TO INDATE-1                       CLM0211
012470        MOVE 310                TO REQUEST-TYPE                   CLM0211
012480        PERFORM X000-CALL-CUTDTCAL                                CLM0211
012490           THRU X000-EXIT                                         CLM0211
012500        MOVE DAYS-DIFF TO WS-DAYS-DIFF-2                          CLM0211
012510                                                                  CLM0211
012520        IF RTN-NO-ERROR                                           CLM0211
012530           COMPUTE WS-DETL-RATIO ROUNDED =                        CLM0211
012540              WS-DAYS-DIFF-1/WS-DAYS-DIFF-2                       CLM0211
012550           END-COMPUTE                                            CLM0211
012560        END-IF                                                    CLM0211
012570     ELSE                                                         CLM0211
012580        MOVE 1  TO WS-DETL-RATIO                                  CLM0211
012590     END-IF.                                                      CLM0211
012600
012610 B340-EXIT.                                                       CLM0211
012620     EXIT.                                                        CLM0211
012630
012640 B342-ADD-DETAIL-TO-BUCKET.                                       CLM0211
012650     IF ADVANCE-PREM AND NOT-DETL-UNEARNED-PREM                   CLM0211
012660     AND BRDT3-BILL-REMIT-CD(BRDTX) = 'S'                         CLM0211
012670* ADV PREM 'T' TYPE HAVE ONLY THE SUM OF SHORTFALL.               CLM0211
012680* NEED TO FLIP THIS NEGATIVE NUMBER BACK TO POSITIVE SUM          CLM0211
012690        COMPUTE BRDT3-TOT-SUBM-AMT(BRDTX) =                       CLM0211
012700             BRDT3-TOT-SUBM-AMT(BRDTX) * -1                       CLM0211
012710        END-COMPUTE                                               CLM0211
012720     END-IF.                                                      CLM0211
012730     COMPUTE WS-ACCR-AMT(SUB0) = WS-ACCR-AMT(SUB0)                CLM0211
012740                               + BRDT3-TOT-SUBM-AMT(BRDTX)        CLM0211
012750     END-COMPUTE.                                                 CLM0211
012760*    UNEARNED PREM AND DUE PREM HAVE SEPARATE RATIO FOR EACH DETL CLM0211
012770     IF UNEARNED-PREM OR DUE-PREM OR DETL-UNEARNED-PREM           CLM0211
012780        COMPUTE WS-PRORATE-AMT ROUNDED = WS-DETL-RATIO *          CLM0211
012790                  BRDT3-TOT-SUBM-AMT(BRDTX)                       CLM0211
012800        END-COMPUTE                                               CLM0211
012810     ELSE                                                         CLM0211
012820*    THE PREM RATIO HERE IS ALWAYS 1, CODE LEFT TO MATCH OLD CODE CLM0211
012830        COMPUTE WS-PRORATE-AMT ROUNDED = WS-PREM-RATIO *          CLM0211
012840                  BRDT3-TOT-SUBM-AMT(BRDTX)                       CLM0211
012850        END-COMPUTE                                               CLM0211
012860     END-IF.                                                      CLM0211
012870     COMPUTE WS-ACCR-PRORATE-AMT(SUB0) =                          CLM0211
012880             WS-ACCR-PRORATE-AMT(SUB0)                            CLM0211
012890                + WS-PRORATE-AMT                                  CLM0211
012900     END-COMPUTE.                                                 CLM0211
012910
012920 B342-EXIT.                                                       CLM0211
012930     EXIT.                                                        CLM0211
012940
012950 B350-PROCESS-TOLA-BILL.                                          CLM0211
012960* IF WE ARE DOING OVERRIDE ACCT WE NEED TO DO SPECIAL LOGIC IF    CLM0211
012970* THE BILL HAS TOLERANCE DETAILS. THIS BILL DOES.                 CLM0211
012980* SO WE MUST NOW FIND THE ASSOCIATED ORIGINAL BILL                CLM0211
012990* IN ORDER TO PRORATE THE AMOUNTS TO DO ACCOUNTING BY PROVINCE    CLM0211
013000* IF THIS IS ADVANCED ACCOUNTING ORIGINAL BILL MAY BE RESOLVED    CLM0211
013010
013020     IF WS-ORIG-CSBL-KEY = 0                                      CLM0211
013030        PERFORM D450-ORIG-BILL                                    CLM0211
013040           THRU D450-EXIT                                         CLM0211
013050     END-IF                                                       CLM0211
013060
013070*  SUM ALL BRDT'S AND SFDT'S BY FDRT, LOAD DATA INTO INTERNAL TBL CLM0211
013080     IF RTN-NO-ERROR                                              CLM0211
013090        PERFORM C400-SUM-BY-FDRT THRU C400-EXIT                   CLM0211
013100     END-IF                                                       CLM0211
013110
013120     PERFORM VARYING SUB0 FROM 1 BY 1                             CLM0211
013130        UNTIL SUB0 > WS-ACCR-COUNT OR RTN-ERROR                   CLM0211
013140        IF WS-REAS-REF-CD (SUB0) <> 'TOLA'                        CLM0211
013150           ADD 1 TO WS-TOLA-COUNT                                 CLM0211
013160           MOVE WS-TOLA-COUNT TO SUB1                             CLM0211
013170           MOVE WS-ACCR-ARRAY-REC(SUB0) TO                        CLM0211
013180                   WS-TOLA-ARRAY-REC(SUB1)                        CLM0211
013190        ELSE                                                      CLM0211
013200           PERFORM C500-PROCESS-FDRT THRU C500-EXIT               CLM0211
013210        END-IF                                                    CLM0211
013220     END-PERFORM.                                                 CLM0211
013230
013240     MOVE WS-TOLA-OUTPUT-ARRAY TO WS-ACCR-OUTPUT-ARRAY.           CLM0211
013250
013260 B350-EXIT.                                                       CLM0211
013270     EXIT.                                                        CLM0211
013280
013290
013300 B400-WRITE-NOTL-TDTLS.
013310
013320     PERFORM VARYING SUB0 FROM 1 BY 1
013330        UNTIL SUB0 > WS-ACCR-COUNT OR RTN-ERROR
013340        PERFORM B405-POPULATE-TDTL-FIELDS                         PFM0304
013350           THRU B405-EXIT                                         PFM0304
013360** INSERT A BLOCK OF 100
013370        IF TDTL2-ROWS-FOUND = 100 AND RTN-NO-ERROR
013380           PERFORM C000-TDTL-INSERT
013390              THRU C000-EXIT
013400           INITIALIZE TDTL2-DATA
013410        END-IF
013420
013430     END-PERFORM.
013440 B400-EXIT.
013450     EXIT.
013460
013470 B405-POPULATE-TDTL-FIELDS.                                       PFM0304
013480                                                                  PFM0304
013490** IF TOLA BILLING AND PREMIUM NOT PAID WOULD HAVE CREATED        PFM0304
013500**  AND OFFSET TO MAKE IT NET TO ZERO                             PFM0304
013510**  THE UNEARNED PREMIUM THAT WAS CALCULATED ON THE               PFM0304
013520**  UNCOLLECTED PREMIUM.                                          PFM0304
013530     IF (ACCRUAL-TOLA AND DUE-PREM)                               PFM0304
013540     AND  WS-ACCR-IND(SUB0) = 'A'                                 PFM0304
013550     AND  NO-OVERRIDE-ACCT                                        CLM0211
013560        GO TO B405-EXIT                                           PFM0304
013570     END-IF.                                                      PFM0304
013580
013590** SETUP TDTL
013600     ADD 1      TO  TDTL2-ROWS-FOUND
013610
013620     PERFORM B410-POPULATE-TDTL-AMTS                              CLM0211
013630        THRU B410-EXIT                                            CLM0211
013640
013650     MOVE WS-OUTPUT-TR-CD(SUB0)
013660        TO TDTL2-TR-CD              (TDTL2-ROWS-FOUND).
013670
013680*    APPLY NORMAL PROCESSING RULES
013690     MOVE '0'
013700        TO TDTL2-INT-INCL-CD        (TDTL2-ROWS-FOUND).
013710
013720*    NOTIONAL
013730     MOVE '2'
013740        TO TDTL2-STAT-CD            (TDTL2-ROWS-FOUND).
013750
013760     MOVE FDP-EFF-DT
013770        TO TDTL2-EFF-DT             (TDTL2-ROWS-FOUND)
013780           TDTL2-DUE-DT             (TDTL2-ROWS-FOUND)
013790           TDTL2-TX-DT              (TDTL2-ROWS-FOUND).
013800
013810     IF CASD1-CMNC-DT > FDP-CYC-DT AND OVERRIDE-ACCT              CLM0439
013820*       USE SCHEME COMMENCEMENT DATE WHEN CYCLE DATE IS IN THE    CLM0439
013830*       FUTURE AND ACCRUAL ACCOUNTING BY BENEFIT AND PROVINCE     CLM0439
013840        MOVE CASD1-CMNC-DT                                        CLM0439
013850           TO TDTL2-BOOK-DT            (TDTL2-ROWS-FOUND)         CLM0439
013860              FDP-BOOK-DT                                         CLM0439
013870     ELSE                                                         CLM0439
013880        MOVE FDP-CYC-DT
013890           TO TDTL2-BOOK-DT            (TDTL2-ROWS-FOUND)
013900     END-IF.                                                      CLM0439
013910
013920     MOVE FDP-CYC-DT
013930        TO TDTL2-PROC-DT            (TDTL2-ROWS-FOUND).
013940
013950     MOVE FDP-TR-REF-NO
013960        TO TDTL2-TR-REF-NO          (TDTL2-ROWS-FOUND).
013970
013980     MOVE FDP-SCAT-KEY
013990        TO TDTL2-SCAT-KEY           (TDTL2-ROWS-FOUND).
014000
014010     MOVE FDP-CASE-KEY
014020        TO TDTL2-CASE-KEY           (TDTL2-ROWS-FOUND).
014030
014040     MOVE WS-FDRT-KEY(SUB0)                                       CLM0054
014050        TO TDTL2-FDRT-KEY           (TDTL2-ROWS-FOUND).           CLM0054
014060
014070     MOVE ZEROS
014080        TO TDTL2-UNIT-CT            (TDTL2-ROWS-FOUND)
014090           TDTL2-BKT-NO             (TDTL2-ROWS-FOUND)
014100           TDTL2-FD-NO              (TDTL2-ROWS-FOUND)
014110           TDTL2-FD-NO              (TDTL2-ROWS-FOUND)
014120           TDTL2-CHRL-KEY           (TDTL2-ROWS-FOUND)
014130           TDTL2-CASE-MBR-KEY       (TDTL2-ROWS-FOUND)
014140           TDTL2-TRNCH-KEY          (TDTL2-ROWS-FOUND)
014150           TDTL2-ORIG-SCAT-KEY      (TDTL2-ROWS-FOUND)
014160           TDTL2-MNY-TYP-NO         (TDTL2-ROWS-FOUND).
014170
014180     MOVE SPACES
014190        TO TDTL2-MNY-CAT-NUM        (TDTL2-ROWS-FOUND)
014200           TDTL2-UNALLOC-ACCT-CD    (TDTL2-ROWS-FOUND)
014210           TDTL2-FD-DESC-ID         (TDTL2-ROWS-FOUND)
014220           TDTL2-RVSL-CYC-DT        (TDTL2-ROWS-FOUND).
014230
014240     PERFORM VARYING IVTXH FROM 1 BY 1 UNTIL                      CLM0054
014250              IVTXH > VTXH1-ROWS-FOUND                            CLM0054
014260        IF VTXH1-TAX-ID-CD (IVTXH) = WS-TAX-ID-CD (SUB0)          CLM0054
014270           MOVE VTXH1-UNALLOC-ACCT-CD (IVTXH)                     CLM0054
014280              TO TDTL2-UNALLOC-ACCT-CD (TDTL2-ROWS-FOUND)         CLM0054
014290        END-IF                                                    CLM0054
014300     END-PERFORM.                                                 CLM0054
014310
014320     MOVE CSBL1-CSBL-KEY
014330        TO TDTL2-REF-KEY            (TDTL2-ROWS-FOUND).
014340
014350*  CASE BILLS
014360     MOVE 19
014370        TO TDTL2-DEF-BY-NUM         (TDTL2-ROWS-FOUND).
014380
014390 B405-EXIT.                                                       PFM0304
014400     EXIT.                                                        PFM0304
014410
014420 B410-POPULATE-TDTL-AMTS.                                         CLM0211
014430
014440** IF OVERRIDE ACCOUNTING ACCRUAL-TOLA SETTING DOES NOT APPLY     CLM0211
014450*  SO FOR CLARITY WE HAVE MOVED THE CODE FOR SETTING AMT TO THIS  CLM0211
014460*  CALLED PARAGRAPH AND MADE A BRANCH                             CLM0211
014470
014480     IF NO-OVERRIDE-ACCT                                          CLM0211
014490
014500*  IF D&U TOTAL, WRITE TOTAL BRDT/SFDT AMOUNTS TO TDTL2-AMT
014510*  ELSE WRITE PRORATED AMT TO THIS FIELD
014520
014530        IF WS-ACCR-IND(SUB0) = 'A'
014540*     TOLA ACCRUAL THE AMOUNT FOR THE UNEARNED PREMIUM            PFM0304
014550           IF (ACCRUAL-TOLA AND UNEARNED-PREM)                    PFM0304
014560              COMPUTE TDTL2-AMT (TDTL2-ROWS-FOUND) =              PFM0304
014570                  WS-ACCR-AMT (SUB0) - WS-ACCR-PRORATE-AMT (SUB0) PFM0304
014580              MOVE TDTL2-AMT (TDTL2-ROWS-FOUND) TO                PFM0304
014590                   TDTL2-PRNC-AMT (TDTL2-ROWS-FOUND)              PFM0304
014600           ELSE                                                   PFM0304
014610              MOVE WS-ACCR-PRORATE-AMT(SUB0)
014620                 TO TDTL2-AMT             (TDTL2-ROWS-FOUND)
014630                    TDTL2-PRNC-AMT        (TDTL2-ROWS-FOUND)
014640           END-IF                                                 PFM0304
014650        ELSE
014660           IF (ACCRUAL-TOLA AND DUE-PREM)                         USM0065B
014670              COMPUTE TDTL2-AMT (TDTL2-ROWS-FOUND) =              USM0065B
014680                  WS-ACCR-AMT (SUB0) - WS-ACCR-PRORATE-AMT (SUB0) USM0065B
014690              MOVE TDTL2-AMT (TDTL2-ROWS-FOUND) TO                USM0065B
014700                   TDTL2-PRNC-AMT (TDTL2-ROWS-FOUND)              USM0065B
014710           ELSE                                                   USM0065B
014720              MOVE WS-ACCR-AMT(SUB0)
014730                 TO TDTL2-AMT             (TDTL2-ROWS-FOUND)
014740                    TDTL2-PRNC-AMT        (TDTL2-ROWS-FOUND)
014750           END-IF                                                 USM0065B
014760        END-IF                                                    CLM0211
014770     ELSE                                                         CLM0211
014780*  IF D&U TOTAL, WRITE TOTAL BRDT/SFDT AMOUNTS TO TDTL2-AMT       CLM0211
014790*  ELSE WRITE PRORATED AMT TO THIS FIELD                          CLM0211
014800        IF WS-ACCR-IND(SUB0) = 'P'                                CLM0211
014810           MOVE WS-ACCR-PRORATE-AMT(SUB0)                         CLM0211
014820           TO TDTL2-AMT             (TDTL2-ROWS-FOUND)            CLM0211
014830              TDTL2-PRNC-AMT        (TDTL2-ROWS-FOUND)            CLM0211
014840        ELSE                                                      CLM0211
014850           MOVE WS-ACCR-AMT(SUB0)                                 CLM0211
014860              TO TDTL2-AMT             (TDTL2-ROWS-FOUND)         CLM0211
014870                 TDTL2-PRNC-AMT        (TDTL2-ROWS-FOUND)         CLM0211
014880        END-IF                                                    CLM0211
014890     END-IF.
014900 B410-EXIT.                                                       CLM0211
014910     EXIT.                                                        CLM0211
014920
014930 B500-INSERT-XREFS.
014940
014950*  WE ARE WRITING TO THIS TABLE SO WE DON'T PICK UP THE SAME CSBL KEY
014960*  AGAIN IF WE NEED TO RESTART
014970
014980     INITIALIZE XREF1-DATA.
014990
015000*  USE REINIT DATE AS FDP-EFF-DT IS NOT ALWAYS REINIT DATE        CLM0716
015010     MOVE REAR2-NXT-REINIT-DT (1) TO XREF1-EFF-DT.                CLM0716
015020
015030     MOVE 'CSBL'         TO XREF1-REF-TYP.
015040     MOVE CSBL1-CSBL-KEY TO XREF1-REF-KEY.
015050*  THIS 'BJGL' IS USED IN CF3CSBLD                                CLM0716
015060     MOVE 'BJGL'         TO XREF1-MSTR-REF-TYP.                   CLM0716
015070*  THIS FIELD IS PURPOSELY SET TO ZERO AS DAL                     CLM0716
015080*  CF3CSBLD DOES NOT USE IN SELECT                                CLM0716
015090     MOVE ZERO           TO XREF1-MSTR-REF-KEY.                   CLM0716
015100
015110
015120     CALL 'CI1XREFD' USING RMESSAGE,
015130                           XREF1-DATA,
015140                           RTN-OUTPUT-FIELDS.
015150
015160 B500-EXIT.
015170     EXIT.
015180
015190 C000-TDTL-INSERT.
015200
015210     CALL 'CI1TDTLD' USING RMESSAGE,
015220                           TDTL2-DATA,
015230                           RTN-OUTPUT-FIELDS.
015240
015250 C000-EXIT.
015260     EXIT.
015270
015280 C100-GET-PROVINCES.                                              CLM0054
015290                                                                  CLM0054
015300     INITIALIZE VTXH1-DATA.                                       CLM0054
015310     MOVE FDP-CASE-KEY         TO  VTXH1-CASE-KEY.                CLM0054
015320     MOVE FDP-EFF-DT           TO  VTXH1-EFF-DT.                  CLM0054
015330                                                                  CLM0054
015340     CALL 'CB1VTXHD'     USING RMESSAGE                           CLM0054
015350                              ,VTXH1-DATA                         CLM0054
015360                              ,RTN-OUTPUT-FIELDS.                 CLM0054
015370                                                                  CLM0054
015380     IF RTN-NO-ERROR                                              CLM0054
015390        MOVE SPACES TO WS-PREV-TAX-ID-CD                          CLM0054
015400        PERFORM VARYING IVTXH FROM 1 BY 1 UNTIL                   CLM0054
015410           IVTXH > VTXH1-ROWS-FOUND                               CLM0054
015420           IF VTXH1-TAX-ID-CD (IVTXH) = WS-PREV-TAX-ID-CD         CLM0054
015430              MOVE 'UT'          TO RTN-ERR-PREFIX                CLM0054
015440              MOVE 1317          TO RTN-ERROR-CODE                CLM0054
015450              MOVE WS-PGM-NAME   TO RTN-PROG-NAME                 CLM0054
015460              MOVE 'C100-GET-PROVINCES'                           CLM0054
015470                                 TO RTN-PARA-NAME                 CLM0054
015480              MOVE VTXH1-TAX-ID-CD (IVTXH) TO WS1-TAX-ID-CD       CLM0054
015490              MOVE 1             TO RTN-VAR-SEQ-NO (1)            CLM0054
015500              MOVE WS-ERROR4     TO RTN-VAR-TEXT (1)              CLM0054
015510              MOVE 1             TO RTN-VAR-DETL-CNT              CLM0054
015520              CALL 'CUTSVARB' USING RMESSAGE,                     CLM0054
015530                                    RTN-OUTPUT-FIELDS             CLM0054
015540           ELSE                                                   CLM0054
015550              MOVE VTXH1-TAX-ID-CD (IVTXH) TO WS-PREV-TAX-ID-CD   CLM0054
015560           END-IF                                                 CLM0054
015570        END-PERFORM                                               CLM0054
015580     END-IF.                                                      CLM0054
015590                                                                  CLM0054
015600 C100-EXIT.                                                       CLM0054
015610     EXIT.                                                        CLM0054
015620
015630 C200-GET-SPAN-PRD.                                               CLM0211
015640     MOVE CSBL1-BEG-BILL-DT TO WS-SPAN-BEG-DT                     CLM0211
015650     MOVE CSBL1-NXT-BILL-DT TO WS-SPAN-NXT-DT                     CLM0211
015660     PERFORM D400-READ-TRANSACT-SCHED THRU D400-EXIT              CLM0211
015670     MOVE WS-SPAN-BEG-DT    TO TSCH1-BASE-DT                      CLM0211
015680     INITIALIZE FREQ1-DATA.                                       CLM0211
015690     PERFORM UNTIL WS-SPAN-BEG-DT <= REAR2-NXT-REINIT-DT(1)       CLM0211
015700          OR WS-SPAN-BEG-DT <= CASD1-CMNC-DT OR RTN-ERROR         CLM0211
015710        MOVE TSCH1-TSCH-KEY      TO FREQ1-TSCH-KEY                CLM0211
015720        MOVE TSCH1-FREQ-KEY      TO FREQ1-FREQ-KEY                CLM0211
015730        MOVE TSCH1-BASE-DT       TO FREQ1-IN-DATE                 CLM0211
015740        MOVE ZEROS               TO FREQ1-EXCH-KEY                CLM0211
015750**        'DECREMENT DATE'                                        CLM0211
015760        MOVE 'D'               TO FREQ1-DIR-CD                    CLM0211
015770        CALL 'CUTFREQB'     USING RMESSAGE,                       CLM0211
015780                                  FREQ1-DATA,                     CLM0211
015790                                  RTN-OUTPUT-FIELDS               CLM0211
015800**
015810        IF RTN-NO-ERROR                                           CLM0211
015820           MOVE WS-SPAN-BEG-DT   TO WS-SPAN-NXT-DT                CLM0211
015830           IF  CASD1-CMNC-DT > FREQ1-OUT-DATE                     CLM0211
015840           AND CSBL1-FRST-PYRL-DT NOT EQUAL SPACES                CLM0211
015850              MOVE CASD1-CMNC-DT     TO WS-SPAN-BEG-DT            CLM0211
015860           ELSE                                                   CLM0211
015870              MOVE FREQ1-OUT-DATE    TO WS-SPAN-BEG-DT            CLM0211
015880           END-IF                                                 CLM0211
015890
015900           MOVE FREQ1-OUT-BASE-DATE  TO TSCH1-BASE-DT             CLM0211
015910        END-IF                                                    CLM0211
015920     END-PERFORM.                                                 CLM0211
015930     IF WS-BEG-DT = SPACES AND RTN-NO-ERROR                       CLM0211
015940        PERFORM D500-CALC-AMCD                                    CLM0211
015950           THRU D500-EXIT                                         CLM0211
015960     END-IF.                                                      CLM0211
015970     IF RTN-NO-ERROR                                              CLM0211
015980        INITIALIZE DATEPARMS                                      CLM0211
015990*  GET DAYS DIFF BETWEEN SPAN-NXT-BILL-DT AND AMCD                CLM0211
016000        MOVE WS-BEG-DT         TO INDATE                          CLM0211
016010        MOVE WS-SPAN-NXT-DT    TO INDATE-1                        CLM0211
016020        MOVE 310               TO REQUEST-TYPE                    CLM0211
016030        PERFORM X000-CALL-CUTDTCAL                                CLM0211
016040           THRU X000-EXIT                                         CLM0211
016050        MOVE DAYS-DIFF TO WS-DAYS-DIFF-1                          CLM0211
016060     END-IF.                                                      CLM0211
016070 C200-EXIT.                                                       CLM0211
016080     EXIT.                                                        CLM0211
016090
016100 C400-SUM-BY-FDRT.                                                CLM0211
016110***************************************************************** CLM0211
016120* SUM BRDT'S AND SFDT'S BY FDRT                                   CLM0211
016130***************************************************************** CLM0211
016140
016150**   GET BILL REMIT AND SHORTFALL DETAILS FOR ORIGINAL BILL       CLM0211
016160     INITIALIZE BRDT3-DATA.                                       CLM0211
016170     MOVE 0 TO WS-F-ROWS                                          CLM0211
016180     MOVE 0 TO WS-FP-ROWS(FIDX)                                   CLM0211
016190     PERFORM UNTIL BRDT3-END-FLAG = 'Y'                           CLM0211
016200        OR RTN-ERROR                                              CLM0211
016210        MOVE WS-ORIG-CSBL-KEY TO BRDT3-CSBL-KEY(1)                CLM0211
016220
016230        CALL 'CFNBRDTD' USING RMESSAGE,                           CLM0211
016240                              BRDT3-DATA,                         CLM0211
016250                              RTN-OUTPUT-FIELDS                   CLM0211
016260*       LOAD FDRT ARRAY                                           CLM0211
016270        PERFORM VARYING IBRDT                                     CLM0211
016280          FROM 1 BY 1                                             CLM0211
016290          UNTIL IBRDT > BRDT3-ROWS-FOUND OR RTN-ERROR             CLM0211
016300*
016310**   LOAD THE NET BILLED AMOUNT INTO TABLE, BRDT AMT LESS SFDT AMTCLM0211
016320**         BY FUND RATE                                           CLM0211
016330           SET FIDX TO 1                                          CLM0211
016340           SEARCH WS-FDRT-ARRAY-REC VARYING FIDX                  CLM0211
016350**          AT END OF TABLE, NO ENTRY FOUND                       CLM0211
016360**          ERROR CONDITION - THE ARRAY IS NOT LARGE ENOUGH       CLM0211
016370            AT END                                                CLM0211
016380*            Error - Table has reached maximum rows allowed       CLM0211
016390             MOVE 'TX'           TO RTN-ERR-PREFIX                CLM0211
016400             MOVE 31             TO RTN-ERROR-CODE                CLM0211
016410             MOVE WS-PGM-NAME    TO RTN-PROG-NAME                 CLM0211
016420             MOVE 'C400-SUM-BY-FDRT' TO RTN-PARA-NAME             CLM0211
016430             CALL 'CUTSVARB' USING RMESSAGE,                      CLM0211
016440                                   RTN-OUTPUT-FIELDS              CLM0211
016450
016460**         ENTRY FOUND IN TABLE, ADD OR SUBTRACT FROM FDRT SUM    CLM0211
016470            WHEN WS-F-FDRT-KEY (FIDX) = BRDT3-FDRT-KEY(IBRDT)     CLM0211
016480               IF BRDT3-BILL-REMIT-CD(IBRDT) = 'B'                CLM0211
016490                  COMPUTE WS-F-SUM(FIDX) = WS-F-SUM(FIDX) +       CLM0211
016500                                        BRDT3-TOT-SUBM-AMT(IBRDT) CLM0211
016510               ELSE                                               CLM0211
016520                  COMPUTE WS-F-SUM(FIDX) = WS-F-SUM(FIDX) -       CLM0211
016530                                        BRDT3-TOT-SUBM-AMT(IBRDT) CLM0211
016540               END-IF                                             CLM0211
016550               PERFORM C410-SUM-BY-PROV THRU C410-EXIT
016560                                                                  CLM0211
016570**          NO ENTRY IN TABLE, ADD ENTRY TO TABLE                 CLM0211
016580            WHEN WS-F-FDRT-KEY (FIDX) = ZEROES                    CLM0211
016590               ADD 1 TO WS-F-ROWS                                 CLM0211
016600               MOVE BRDT3-FDRT-KEY(IBRDT) TO WS-F-FDRT-KEY(FIDX)  CLM0211
016610               IF BRDT3-BILL-REMIT-CD(IBRDT) = 'B'                CLM0211
016620                 MOVE BRDT3-TOT-SUBM-AMT(IBRDT) TO WS-F-SUM(FIDX) CLM0211
016630               ELSE                                               CLM0211
016640                 MULTIPLY BRDT3-TOT-SUBM-AMT(IBRDT) BY -1         CLM0211
016650                    GIVING BRDT3-TOT-SUBM-AMT(IBRDT)              CLM0211
016660                 MOVE BRDT3-TOT-SUBM-AMT(IBRDT) TO WS-F-SUM(FIDX) CLM0211
016670               END-IF                                             CLM0211
016680               PERFORM C410-SUM-BY-PROV THRU C410-EXIT
016690                                                                  CLM0211
016700         END-SEARCH                                               CLM0211
016710
016720        END-PERFORM                                               CLM0211
016730     END-PERFORM.                                                 CLM0211
016740 C400-EXIT.                                                       CLM0211
016750     EXIT.                                                        CLM0211
016760
016770 C410-SUM-BY-PROV.                                                CLM0211
016780***************************************************************** CLM0211
016790* SUM BRDT'S AND SFDT'S BY PROVINCE FOR EACH FDRT                 CLM0211
016800***************************************************************** CLM0211
016810**   LOAD THE NET BILLED AMOUNT INTO TABLE, BRDT AMT LESS SFDT AMTCLM0211
016820**   BY PROVINCE FOR FUND RATE                                    CLM0211
016830     SET PIDX TO 1                                                CLM0211
016840     SEARCH WS-FDRT-PROV-RECORD VARYING PIDX                      CLM0211
016850**    AT END OF TABLE, NO ENTRY FOUND                             CLM0211
016860**    ERROR CONDITION - THE ARRAY IS NOT LARGE ENOUGH             CLM0211
016870      AT END                                                      CLM0211
016880*      Error - Table has reached maximum rows allowed             CLM0211
016890       MOVE 'TX'                 TO RTN-ERR-PREFIX                CLM0211
016900       MOVE 31                   TO RTN-ERROR-CODE                CLM0211
016910       MOVE WS-PGM-NAME          TO RTN-PROG-NAME                 CLM0211
016920       MOVE 'C410-SUM-BY-PROV' TO RTN-PARA-NAME                   CLM0211
016930       CALL 'CUTSVARB' USING RMESSAGE,                            CLM0211
016940                             RTN-OUTPUT-FIELDS                    CLM0211
016950
016960**   ENTRY FOUND IN TABLE, ADD OR SUBTRACT FROM PROVINCE SUM      CLM0211
016970      WHEN WS-FP-TAX-ID-CD (FIDX,PIDX) = BRDT3-TAX-ID-CD(IBRDT)   CLM0211
016980         IF BRDT3-BILL-REMIT-CD(IBRDT) = 'B'                      CLM0211
016990            COMPUTE WS-FP-SUM(FIDX,PIDX) =                        CLM0211
017000               WS-FP-SUM(FIDX,PIDX) + BRDT3-TOT-SUBM-AMT(IBRDT)   CLM0211
017010         ELSE                                                     CLM0211
017020            COMPUTE WS-FP-SUM(FIDX,PIDX) =                        CLM0211
017030               WS-FP-SUM(FIDX,PIDX) - BRDT3-TOT-SUBM-AMT(IBRDT)   CLM0211
017040         END-IF                                                   CLM0211
017050
017060**    NO ENTRY IN TABLE, ADD ENTRY TO TABLE                       CLM0211
017070      WHEN WS-FP-TAX-ID-CD (FIDX,PIDX) = SPACES                   CLM0211
017080         ADD 1 TO WS-FP-ROWS(FIDX)                                CLM0211
017090         MOVE BRDT3-TAX-ID-CD(IBRDT) TO WS-FP-TAX-ID-CD(FIDX,PIDX)CLM0211
017100         IF BRDT3-BILL-REMIT-CD(IBRDT) = 'B'                      CLM0211
017110           MOVE BRDT3-TOT-SUBM-AMT(IBRDT) TO WS-FP-SUM(FIDX,PIDX) CLM0211
017120         ELSE                                                     CLM0211
017130           MULTIPLY BRDT3-TOT-SUBM-AMT(IBRDT) BY -1               CLM0211
017140              GIVING BRDT3-TOT-SUBM-AMT(IBRDT)                    CLM0211
017150           MOVE BRDT3-TOT-SUBM-AMT(IBRDT) TO WS-FP-SUM(FIDX,PIDX) CLM0211
017160         END-IF                                                   CLM0211
017170
017180   END-SEARCH.                                                    CLM0211
017190
017200 C410-EXIT.                                                       CLM0211
017210     EXIT.                                                        CLM0211
017220
017230 C500-PROCESS-FDRT.                                               CLM0211
017240***************************************************************** CLM0211
017250*                                                                 CLM0211
017260***************************************************************** CLM0211
017270     MOVE WS-ACCR-AMT(SUB0)         TO WS-REMAIN-1.               CLM0211
017280     MOVE WS-ACCR-PRORATE-AMT(SUB0) TO WS-REMAIN-2.               CLM0211
017290
017300     PERFORM VARYING FIDX                                         CLM0211
017310        FROM 1 BY 1  UNTIL                                        CLM0211
017320        FIDX > WS-F-ROWS                                          CLM0211
017330
017340        IF WS-FDRT-KEY(SUB0) = WS-F-FDRT-KEY(FIDX)                CLM0211
017350           PERFORM VARYING PIDX                                   CLM0211
017360              FROM 1 BY 1 UNTIL                                   CLM0211
017370              PIDX > WS-FP-ROWS(FIDX)                             CLM0211
017380              ADD 1 TO WS-TOLA-COUNT                              CLM0211
017390              MOVE WS-TOLA-COUNT TO SUB1                          CLM0211
017400              MOVE WS-ACCR-ARRAY-REC(SUB0) TO                     CLM0211
017410                   WS-TOLA-ARRAY-REC(SUB1)                        CLM0211
017420              MOVE WS-FP-TAX-ID-CD(FIDX,PIDX) TO                  CLM0211
017430                   WS-T-TAX-ID-CD(SUB1)                           CLM0211
017440              IF PIDX = WS-FP-ROWS(FIDX)                          CLM0211
017450**            PROCESSING LAST ROW MOVE ENTIRE REMAINING AMOUNT IN CLM0211
017460                 MOVE WS-REMAIN-1 TO WS-T-ACCR-AMT(SUB1)          CLM0211
017470                 MOVE WS-REMAIN-2 TO WS-T-ACCR-PRORATE-AMT(SUB1)  CLM0211
017480              ELSE                                                CLM0211
017490**            WHAT PCT THIS ROW IS OF TOTAL AMT FOR FDRT/PROVINCE CLM0211
017500                 COMPUTE WS-FACTOR ROUNDED =                      CLM0211
017510                    WS-FP-SUM (FIDX,PIDX) / WS-F-SUM(FIDX)        CLM0211
017520                 END-COMPUTE                                      CLM0211
017530*                CALCULATE USING THE FACTOR                       CLM0211
017540                 COMPUTE WS-T-ACCR-AMT(SUB1) ROUNDED =            CLM0211
017550                          WS-ACCR-AMT(SUB0) * WS-FACTOR           CLM0211
017560                 END-COMPUTE                                      CLM0211
017570                 COMPUTE WS-T-ACCR-PRORATE-AMT(SUB1) ROUNDED =    CLM0211
017580                          WS-ACCR-PRORATE-AMT(SUB0) * WS-FACTOR   CLM0211
017590                 END-COMPUTE                                      CLM0211
017600*                SUBTRACT AMT FROM REMAINDER AMT                  CLM0211
017610                 COMPUTE WS-REMAIN-1 = WS-REMAIN-1                CLM0211
017620                             - WS-T-ACCR-AMT(SUB1)                CLM0211
017630                 END-COMPUTE                                      CLM0211
017640                 COMPUTE WS-REMAIN-2 = WS-REMAIN-2                CLM0211
017650                             - WS-T-ACCR-PRORATE-AMT(SUB1)        CLM0211
017660                 END-COMPUTE                                      CLM0211
017670              END-IF                                              CLM0211
017680           END-PERFORM                                            CLM0211
017690        END-IF                                                    CLM0211
017700     END-PERFORM.                                                 CLM0211
017710
017720 C500-EXIT.                                                       CLM0211
017730     EXIT.                                                        CLM0211
017740
017750 D000-REASSURANCE-ACCT.                                           ANM0035
017760                                                                  ANM0035
017770     EVALUATE TRUE                                                ANM0035
017780       WHEN ADVANCE-PREM                                          ANM0035
017790**       ADVANCE REINSURANCE PREMIUM                              ANM0035
017800         MOVE 195 TO TRTR1-ACTIVITY-NUM                           ANM0035
017810       WHEN UNEARNED-PREM                                         ANM0035
017820**       UNEARNED REINSURANCE PREMIUM - COLLECTED                 ANM0035
017830         MOVE 196 TO TRTR1-ACTIVITY-NUM                           ANM0035
017840       WHEN DUE-PREM                                              ANM0035
017850**       UNEARNED REINSURANCE PREMIUM - D&U                       ANM0035
017860         MOVE 197 TO TRTR1-ACTIVITY-NUM                           ANM0035
017870       WHEN OTHER                                                 ANM0035
017880         GO TO D000-EXIT                                          ANM0035
017890     END-EVALUATE.                                                ANM0035
017900                                                                  ANM0035
017910     PERFORM D100-PROCESS-TDTL                                    ANM0035
017920        THRU D100-EXIT.                                           ANM0035
017930                                                                  ANM0035
017940     IF RTN-NO-ERROR AND DUE-PREM                                 ANM0035
017950**       TOTAL UNEARNED REINSURANCE PREMIUM - D&U                 ANM0035
017960        MOVE 198 TO TRTR1-ACTIVITY-NUM                            ANM0035
017970        PERFORM D100-PROCESS-TDTL                                 ANM0035
017980           THRU D100-EXIT                                         ANM0035
017990     END-IF.                                                      ANM0035
018000                                                                  ANM0035
018010 D000-EXIT.                                                       ANM0035
018020     EXIT.                                                        ANM0035
018030                                                                  ANM0035
018040 D100-PROCESS-TDTL.                                               ANM0035
018050                                                                  ANM0035
018060* SELECT ALL TRANSACT DETAILS FOR THE CASE BILL THAT HAVE         ANM0035
018070* A TR CODE THAT EXIST IN THE REINSURANCE ACCRUAL ACCOUNTING      ANM0035
018080* TR CODE GROUP AND ASSIGN THE APPRORIATE TR CODE FROM TR CODE    ANM0035
018090* TRANCHE FOR THE ASSOCIATED ACTIVITY NUMBER.                     ANM0035
018100                                                                  ANM0035
018110     INITIALIZE TDTLS-DATA.                                       ANM0035
018120     MOVE CSBL1-TR-REF-NO TO TDTLS-TR-REF-NO (1).                 ANM0035
018130                                                                  ANM0035
018140     PERFORM UNTIL TDTLS-END-FLAG = 'Y'                           ANM0035
018150     OR RTN-ERROR                                                 ANM0035
018160                                                                  ANM0035
018170       CALL 'CFXVTDLD'  USING RMESSAGE                            ANM0035
018180                              TDTLS-DATA                          ANM0035
018190                              TRTR1-DATA                          ANM0035
018200                              RTN-OUTPUT-FIELDS                   ANM0035
018210                                                                  ANM0035
018220       PERFORM VARYING TDTLX FROM 1 BY 1                          ANM0035
018230          UNTIL TDTLX > TDTLS-ROWS-FOUND OR RTN-ERROR             ANM0035
018240                                                                  ANM0035
018250         IF TRTR1-ACTIVITY-NUM <> 198                             ANM0035
018260**  IF TOTAL UNEARNED REINSURANCE PREMIUM - D&U DO NOT COMPUTE    ANM0035
018270**  THE AMOUNT USING THE PREMIUM RATIO, USE THE TOTAL AMOUNT.     ANM0035
018280            COMPUTE TDTLS-AMT (TDTLX) =                           ANM0035
018290                    TDTLS-AMT (TDTLX) * WS-PREM-RATIO             ANM0035
018300         END-IF                                                   ANM0035
018310                                                                  ANM0035
018320         MOVE FDP-SCAT-KEY TO TDTLS-SCAT-KEY (TDTLX)              ANM0035
018330                                                                  ANM0035
018340       END-PERFORM                                                ANM0035
018350                                                                  ANM0035
018360       IF RTN-NO-ERROR AND TDTLS-ROWS-FOUND > 0                   ANM0035
018370          CALL 'CIATDTLD' USING RMESSAGE,                         ANM0035
018380                                TDTLS-DATA,                       ANM0035
018390                                RTN-OUTPUT-FIELDS                 ANM0035
018400       END-IF                                                     ANM0035
018410                                                                  ANM0035
018420     END-PERFORM.                                                 ANM0035
018430                                                                  ANM0035
018440 D100-EXIT.                                                       ANM0035
018450     EXIT.                                                        ANM0035
018460                                                                  ANM0035
018470
018480******************************************************************CLM0211
018490**                                                                CLM0211
018500******************************************************************CLM0211
018510 D400-READ-TRANSACT-SCHED.                                        CLM0211
018520                                                                  CLM0211
018530     INITIALIZE TSCH1-DATA.                                       CLM0211
018540     IF CSBL1-TSCH-KEY > ZEROES                                   CLM0211
018550        MOVE CSBL1-TSCH-KEY        TO TSCH1-TSCH-KEY              CLM0211
018560     ELSE                                                         CLM0211
018570*    IF BILL ACCOUNTING ON HAS NO TSCH IT MIGHT BE TOLERANCE BILL CLM0211
018580        PERFORM D450-ORIG-BILL                                    CLM0211
018590           THRU D450-EXIT                                         CLM0211
018600        IF RTN-NO-ERROR AND WS-ORIG-CSBL-KEY > 0                  CLM0211
018610           MOVE WS-ORIG-CSBL-KEY TO CSBL1-CSBL-KEY                CLM0211
018620                                                                  CLM0211
018630           CALL 'CS1CSBLD' USING RMESSAGE                         CLM0211
018640                                 CSBL1-DATA                       CLM0211
018650                                 RTN-OUTPUT-FIELDS                CLM0211
018660        END-IF                                                    CLM0211
018670        IF RTN-NO-ERROR                                           CLM0742
018680           IF CSBL1-TSCH-KEY > ZEROES                             CLM0742
018690              MOVE CSBL1-TSCH-KEY TO TSCH1-TSCH-KEY               CLM0211
018700           ELSE                                                   CLM0211
018710*             Error - CANNOT FIND FREQUENCY FOR BILL              CLM0211
018720              MOVE 'JL'             TO RTN-ERR-PREFIX             CLM0211
018730              MOVE   78             TO RTN-ERROR-CODE             CLM0211
018740              MOVE WS-PGM-NAME      TO RTN-PROG-NAME              CLM0211
018750              MOVE 'D400-READ-TRANS' TO RTN-PARA-NAME             CLM0211
018760              CALL 'CUTSVARB' USING RMESSAGE,                     CLM0211
018770                                    RTN-OUTPUT-FIELDS             CLM0211
018780           END-IF                                                 CLM0742
018790        END-IF                                                    CLM0211
018800        MOVE SAVE-CSBL1-DATA TO CSBL1-DATA                        CLM0211
018810     END-IF                                                       CLM0211
018820     IF RTN-NO-ERROR                                              CLM0211
018830        CALL 'CS1TSCHD'         USING RMESSAGE,                   CLM0211
018840                                      TSCH1-DATA,                 CLM0211
018850                                      RTN-OUTPUT-FIELDS           CLM0211
018860     END-IF.                                                      CLM0211
018870 D400-EXIT.                                                       CLM0211
018880     EXIT.                                                        CLM0211
018890                                                                  CLM0211
018900******************************************************************CLM0211
018910 D450-ORIG-BILL.                                                  CLM0211
018920* FIRST CHECK IF BILL ACCOUNTING ON ONLY HAS TOLERANCE DETAILS    CLM0211
018930     MOVE FDP-REF-KEY               TO WS-CSBL-KEY                CLM0211
018940     PERFORM D460-CHCK-TOLA-BILL                                  CLM0211
018950        THRU D460-EXIT                                            CLM0211
018960     IF RTN-NO-ERROR AND CSBL6-ROWS-FOUND > ZERO                  CLM0211
018970        PERFORM D455-FIND-ORIG-BILL                               CLM0211
018980           THRU D455-EXIT                                         CLM0211
018990     ELSE                                                         CLM0211
019000        MOVE FDP-REF-KEY TO WS-ORIG-CSBL-KEY                      CLM0211
019010     END-IF.                                                      CLM0211
019020 D450-EXIT.                                                       CLM0211
019030     EXIT.                                                        CLM0211
019040                                                                  CLM0211
019050 D455-FIND-ORIG-BILL.                                             CLM0211
019060*    MOVE ZEROS TO WS-ORIG-CSBL-KEY                               CLM0211
019070     SET INITIAL-PREV-BILL TO TRUE.                               CLM0742
019080     PERFORM UNTIL WS-ORIG-CSBL-KEY > 0 OR RTN-ERROR              CLM0211
019090        PERFORM D457-FIND-PREV-BILL                               CLM0211
019100           THRU D457-EXIT                                         CLM0211
019110*       CHECK THE FIRST RECORD.                                   CLM0211
019120*    IF THE TOLERANCE DETL IS LINKED TO ANOTHER TOLERANCE BILL,   CLM0211
019130*       WE NEED TO KEEP GOING UNTIL WE FIND THE ORIGINAL BILL.    CLM0211
019140*                                                                 CLM0211
019150*    IF THE TOLERANCE DETL IS NOT LINKED TO A TOLERANCE CASE BILL CLM0211
019160*       THIS IS THE ORIGINAL BILL AND WE NEED TO SELECT THE       CLM0211
019170*       CORRESPONDING BILL REMIT DETL AND SHORTFALL DETAILS.      CLM0211
019180*                                                                 CLM0211
019190        IF RTN-NO-ERROR                                           CLM0211
019200           IF SFDT1-ROWS-FOUND = 0 AND                            CLM0742
019210              INITIAL-PREV-BILL                                   CLM0742
019220*          ERROR - CANNOT DETERMINE ORIGINAL BILL AS              CLM0742
019230*          TOLA SHORTFALL NOT FOUND FOR TR-REF-NO/UA              CLM0742
019240              MOVE 'JL'             TO RTN-ERR-PREFIX             CLM0742
019250              MOVE  79              TO RTN-ERROR-CODE             CLM0742
019260              MOVE WS-PGM-NAME      TO RTN-PROG-NAME              CLM0742
019270              MOVE 'D455-FIND-ORIG' TO RTN-PARA-NAME              CLM0742
019280              MOVE CSBL6-CSBL-KEY   TO WS-CSBL-KEY-NUM            CLM0742
019290              STRING 'CURRENT CASE BILL KEY - ' DELIMITED BY SIZE CLM0742
019300                     WS-CSBL-KEY-NUM DELIMITED BY SIZE            CLM0742
019310                                  INTO RTN-VAR-TEXT (1)           CLM0742
019320              MOVE 1                TO RTN-VAR-SEQ-NO (1)         CLM0742
019330              STRING 'UNALLOCATED ACCOUNT - ' DELIMITED BY SIZE   CLM0742
019340                     CSBL6-UNALLOC-ACCT-CD DELIMITED BY SIZE      CLM0742
019350                                  INTO RTN-VAR-TEXT (2)           CLM0742
019360              MOVE 2                TO RTN-VAR-SEQ-NO (2)         CLM0742
019370              STRING 'TR-REF-NO - ' DELIMITED BY SIZE             CLM0742
019380                     CSBL6-TR-REF-NO DELIMITED BY SIZE            CLM0742
019390                                  INTO RTN-VAR-TEXT (3)           CLM0742
019400              MOVE 3                TO RTN-VAR-SEQ-NO (3)         CLM0742
019410              MOVE 3                TO RTN-VAR-DETL-CNT           CLM0742
019420              CALL 'CUTSVARB' USING RMESSAGE,                     CLM0742
019430                                    RTN-OUTPUT-FIELDS             CLM0742
019440           ELSE                                                   CLM0742
019450              SET NOT-INITIAL-PREV-BILL   TO TRUE                 CLM0742
019460              MOVE SFDT1-CSBL-KEY         TO WS-CSBL-KEY          CLM0211
019470              PERFORM D460-CHCK-TOLA-BILL                         CLM0211
019480                 THRU D460-EXIT                                   CLM0211
019490              IF RTN-NO-ERROR AND CSBL6-ROWS-FOUND = ZERO         CLM0211
019500                 MOVE WS-CSBL-KEY TO WS-ORIG-CSBL-KEY             CLM0211
019510              END-IF                                              CLM0211
019520           END-IF                                                 CLM0742
019530        END-IF                                                    CLM0211
019540     END-PERFORM.                                                 CLM0211
019550 D455-EXIT.                                                       CLM0211
019560     EXIT.                                                        CLM0211
019570                                                                  CLM0211
019580 D457-FIND-PREV-BILL.                                             CLM0211
019590*    CHECK FOR A BILL WITH TOLERANCE SHORTFALL DETAILS THAT HAVE  CLM0211
019600*    BEEN CREATED IN COLLECTION PROCESS WITH TOLERANCE TR-REF-NO  CLM0211
019610*    AND REAS REF CODE IS 'TOLA = TOLERANCE'                      CLM0211
019620*    AND BILL REMIT CODE IS '1'= BILL/WAIVER DETAIL               CLM0211
019630     INITIALIZE SFDT1-DATA                                        CLM0211
019640                                                                  CLM0211
019650*    UNPROCESSED                                                  CLM0211
019660     MOVE '0'                 TO SFDT1-STAT-CD                    CLM0211
019670     MOVE 'TOLA'              TO SFDT1-REAS-REF-CD                CLM0211
019680*    1 = BILL/WAIVER DETAIL                                       CLM0211
019690     MOVE '1'                 TO SFDT1-BILL-REMIT-CD              CLM0211
019700     MOVE CSBL6-TR-REF-NO     TO SFDT1-TR-REF-NO                  CLM0211
019710     MOVE CSBL6-UNALLOC-ACCT-CD TO SFDT1-UNALLOC-ACCT-CD          CLM0211
019720     MOVE FDP-CASE-KEY        TO SFDT1-CASE-KEY                   CLM0211
019730                                                                  CLM0211
019740     CALL 'CSHSFDTD' USING RMESSAGE,                              CLM0211
019750                           SFDT1-DATA,                            CLM0211
019760                           RTN-OUTPUT-FIELDS.                     CLM0211
019770 D457-EXIT.                                                       CLM0211
019780     EXIT.                                                        CLM0211
019790                                                                  CLM0211
019800 D460-CHCK-TOLA-BILL.                                             CLM0211
019810     INITIALIZE CSBL6-DATA                                        CLM0211
019820     MOVE WS-CSBL-KEY               TO CSBL6-CSBL-KEY             CLM0211
019830     CALL 'CS0VCSBD' USING RMESSAGE,                              CLM0211
019840                           CSBL6-DATA,                            CLM0211
019850                           RTN-OUTPUT-FIELDS.                     CLM0211
019860 D460-EXIT.                                                       CLM0211
019870     EXIT.                                                        CLM0211
019880
019890** THIS CODE MOVED HERE SO IT COULD BE CALLED                     CLM0211
019900******************************************************************CLM0211
019910 D500-CALC-AMCD.                                                  CLM0211
019920*    NEED TO ADD A DAY TO ACCT MONTH END DATE                     CLM0211
019930     INITIALIZE DATEPARMS                                         CLM0211
019940     MOVE REAR2-NXT-REINIT-DT(1) TO INDATE                        CLM0211
019950     MOVE +1                   TO MONTHS-DAY-CHG                  CLM0211
019960     MOVE 100                  TO REQUEST-TYPE                    CLM0211
019970     PERFORM X000-CALL-CUTDTCAL                                   CLM0211
019980        THRU X000-EXIT                                            CLM0211
019990     IF RTN-NO-ERROR                                              CLM0211
020000        MOVE ODATE TO WS-BEG-DT                                   CLM0211
020010     END-IF.                                                      CLM0211
020020 D500-EXIT.                                                       CLM0211
020030     EXIT.                                                        CLM0211
020040                                                                  CLM0211
020050 X000-CALL-CUTDTCAL.
020060
020070     CALL 'CUTDTCAB' USING RMESSAGE,                              LSM5259
020080                           DATEPARMS,                             LSM5259
020090                           RTN-OUTPUT-FIELDS.                     LSM5259
020100
020110     IF LK-ERROR-CD <> ZEROES
020120        MOVE WS-PGM-NAME  TO RTN-PROG-NAME
020130        MOVE 'X000'       TO RTN-PARA-NAME
020140        MOVE 'UT'         TO RTN-ERR-PREFIX
020150        MOVE 25           TO RTN-ERROR-CODE
020160        CALL 'CUTSVARB' USING RMESSAGE,
020170                              RTN-OUTPUT-FIELDS
020180     END-IF.
020190
020200 X000-EXIT.
020210     EXIT.
020220
020230**
020240**   E N D     O F    P R O G R A M
020250**
