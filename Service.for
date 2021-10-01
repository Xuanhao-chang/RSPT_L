!-----------------------------------------------------------------------
!     Normal Coordinate Analysis.
!     General Library for All Programs.
!
!     File : Service.for
!     Date : 24 Jan 2020
!     Size : 2069 Lines
!
!     Copyright (C) 1983-2020 by Sergey V. Krasnoshchekov
!     All rights reserved.
!-----------------------------------------------------------------------



!-----------------------------------------------------------------------
!     Function: 'REALEX'      FIRST: 22 Jan 2020  LAST EDIT: 18 Feb 2020
!       +1.00*10^(-00)
!     1234567890123456
!-----------------------------------------------------------------------
      CHARACTER*16 FUNCTION REALEX (RENU)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      CHARACTER* 1 IS
      CHARACTER*16 OUTPUT
      CHARACTER* 4 POWER

      A0 = RENU
      IF (A0 .EQ. 0.0D0) THEN
         REALEX = '   0.0          '
         RETURN
      ENDIF

      A1 = ABS (RENU)
      A2 = LOG10 (A1)
      IX = INT (A2)
      IF (A2 .LT. 0.0D0 .AND. A2 .NE. REAL(IX)) IX = IX - 1
      IS = ' '; IF (A0 .LT. 0.0D0) IS = '-'
      AM = A1 / (10.0D0 ** IX)

      WRITE (FMT = 1100, UNIT = OUTPUT) RENU
      IF (OUTPUT(13:13) .EQ. 'E') THEN
         READ  (FMT = "(I3)", UNIT = OUTPUT(14:16), ERR = 100) IX
      ELSE
         READ  (FMT = "(I4)", UNIT = OUTPUT(13:16), ERR = 100) IX
      ENDIF
      IF (IX .EQ. 0) THEN
         REALEX = ' ' // OUTPUT(1:5)
      ELSE
         WRITE (FMT = "(I4)", UNIT = POWER) IX
         REALEX = ' ' // OUTPUT(1:5) // '*10^(' // POWER // ')'
      ENDIF

      GO TO 900

  100 CONTINUE
!----    Possible Error Case: 9.999 >> 10.00
      IF (IX .EQ. 0) THEN
         WRITE (FMT = 1000, UNIT = REALEX) IS,AM
      ELSE
         WRITE (FMT = 1010, UNIT = REALEX) IS,AM,IX
      ENDIF

 1000 FORMAT (' ',A1,F4.2)
 1010 FORMAT (' ',A1,F4.2,'*10^(',I4,')')
 1100 FORMAT (ES16.9)

  900 RETURN
      END



      SUBROUTINE TYPECONT
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      CHARACTER* 1 LETTER

      WRITE (*,1000)
 1000 FORMAT (/'Type letter ''c'' to acknowledge and continue: '\)

      READ  (*,"(A1)") LETTER
      IF (LETTER .EQ. 'c' .OR. LETTER .EQ. 'C') RETURN
      STOP 'Fatal error.'

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'TXPROC'    FIRST: 28 Sep 2010  LAST EDIT: 15 Mar 2018
!
!     PURPOSE:
!     ...
!
!     CALLED: MAIN (ANCO.for)
!
!     CALLS: LENSTR ();
!
!     INPUT:
!     MODRUN -- >
!     FILE_DAT  >
!
!     OUTPUT:
!     FILE_DAT  >
!
!     NOTE(S):
!     1. 22-Dec-2015 : A list of file options from history added
!-----------------------------------------------------------------------
      SUBROUTINE TXPROC (MODRUN,FILE_DAT)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  LOUT,NLOG,NINP,NOUT,NAUX,NSCR
      COMMON /PATHS/  PATH_EXE,PATH_PRO,PATH_OUT,PATH_BIN
      CHARACTER*64 PATH_EXE,PATH_PRO,PATH_OUT,PATH_BIN
      CHARACTER*64 FILE_CFG,FILE_INP,FILE_DAT,FILE_LST,FILE_LOG
      CHARACTER*64 FILE,FILE_DEF(0:8)
      CHARACTER*80 LINE,INPUT
      CHARACTER*32 SIGNATUR
      CHARACTER* 4 EXTDAT,EXTLST,EXTLOG
      CHARACTER* 4 CTRL
      CHARACTER* 8 ANCO_def
      LOGICAL AVAIL,DEFINED
      DATA EXTDAT /'.DAT'/, EXTLST/'.TXT'/, EXTLOG/'.LOG'/
      DATA ANCO_def /'ANCO.def'/  !  Located in PATH_EXE directory
      DATA SIGNATUR /'//  ANCO.def file, do not edit !'/

      IF (MODRUN .EQ. 0) THEN
!----    Batch Mode
         FILE = FILE_DAT
      ELSE
!----    Console Mode
         FILE_CFG = PATH_EXE(1 : LENSTR(PATH_EXE,64)) // ANCO_def
         INQUIRE (FILE = FILE_CFG, EXIST = AVAIL)

         DO 100 K = 0, 8
  100    FILE_DEF(K) = ' '

         IF (AVAIL) THEN
            OPEN (UNIT = NAUX, FILE = FILE_CFG)
            WRITE (*,1100)
            M = 0
            READ (NAUX, 2010, END = 120) LINE
            IF (LINE(1:32) .NE. SIGNATUR) THEN
               CLOSE (NAUX)
               GO TO 140
            ENDIF
            DO 110 K = 1, 8
               IF (EOF (NAUX)) EXIT
               READ (NAUX, 2020, END = 120) FILE_DEF(K)
               IF (FILE_DEF(K) .EQ. ' ') EXIT
               WRITE (*,1110) K,FILE_DEF(K)
               M = M + 1
  110       CONTINUE
  120       CLOSE (NAUX)

            IOPTION = 0  !  User can enter text by mistake
            WRITE (*,1120)
            READ (*,"(A80)") INPUT
            READ (UNIT = INPUT, FMT = "(I4)", ERR = 140) IOPTION
            IF (IOPTION .EQ. 0) IOPTION = 1

            DEFINED = .FALSE.
            IF (IOPTION .GT. 0 .AND. IOPTION .LE. M) THEN
               FILE = FILE_DEF(IOPTION)
               WRITE (*,1140) FILE
               DEFINED = .TRUE.
            ENDIF
         ELSE
            WRITE (*,1150)
            INPUT = ' '
            DEFINED = .FALSE.
         ENDIF

  140    IF (.NOT. DEFINED) THEN
            IF (INPUT .EQ. ' ' .OR. INPUT .EQ. '/') THEN
               WRITE (*,1130)
               READ  (*,2020) FILE
            ELSE
               FILE = INPUT
            ENDIF
            IF (FILE .EQ. ' ') STOP 'Input data file must be specified.'
         ENDIF

         FILE_DEF(0) = FILE

         OPEN (UNIT = NAUX, FILE = FILE_CFG)
         WRITE (NAUX,"(A32)") SIGNATUR
         WRITE (NAUX,2020) FILE_DEF(0)
         M = 0
         DO 160 K = 1, 8
            DO 150 L = 0, K - 1
               IF (FILE_DEF(K) .EQ. FILE_DEF(L)) GO TO 160
  150       CONTINUE
            WRITE (NAUX,2020) FILE_DEF(K)
            M = M + 1
            IF (M .EQ. 8) EXIT
  160    CONTINUE  !  GO TO
         CLOSE (NAUX)

         INQUIRE (FILE = FILE(1 : LENSTR(FILE,64)) // EXTDAT,
     &      EXIST = AVAIL)
         IF (.NOT. AVAIL) STOP 'ANCO: Input data file does not exist.'
      ENDIF

 1100 FORMAT (/'The list of recently used input files (*.DAT):'/)
 1110 FORMAT ('Option (',I1,') : ',A64)
 1120 FORMAT (/'Enter option number or "/" to choose another file: '\)
 1130 FORMAT (/'Type filename without extension *.DAT: '\)
 1140 FORMAT (/'You have chosen the following input file: ',A64)
 1150 FORMAT (/'Warning: ANCO.def does not exist, create a new one.')
!
!-----------------------------------------------------------------------
!     Open input file 'FILE' and process it (remove ! and blanc lines):
!-----------------------------------------------------------------------
      FILE_DAT = FILE

      FILE_INP = PATH_EXE(1 : LENSTR(PATH_EXE,64)) //
     &   FILE(1 : LENSTR(FILE,64)) // EXTDAT
      INQUIRE (FILE = FILE_INP, EXIST = AVAIL)
      IF (AVAIL) THEN
         OPEN (UNIT = NAUX, ACTION = 'READ', FILE = FILE_INP)
      ELSE
         WRITE (*,'(/2A)') 'File *.DAT does not exist: ', FILE_INP
         STOP
      END IF

      FILE_LST = PATH_EXE(1 : LENSTR(PATH_EXE,64)) //
     &   FILE(1 : LENSTR(FILE,64)) // EXTLST
      OPEN (UNIT = NOUT, ACTION = 'WRITE', FILE = FILE_LST)
      DO WHILE (.NOT. EOF (NAUX))
         READ (NAUX,2000) CTRL
         IF (CTRL .EQ. 'ANCO') EXIT
      ENDDO

      FILE_LOG = PATH_EXE(1 : LENSTR(PATH_EXE,64)) //
     &   FILE(1 : LENSTR(FILE,64)) // EXTLOG
      OPEN (UNIT = NLOG, ACTION = 'WRITE', STATUS = 'REPLACE',
     &   FILE = FILE_LOG)
      WRITE (NLOG,1000)

      IF (CTRL .NE. 'ANCO') CALL ERRMSG ('11',0)
      OPEN (UNIT = NINP, STATUS = 'SCRATCH', FORM = 'FORMATTED')
      WRITE (NLOG,1010)
      DO WHILE (.NOT. EOF (NAUX))
         READ (NAUX,2010) LINE
         IF (.NOT. (LINE .EQ. ' ' .OR. LINE(1:1) .EQ. '!' .OR.
     &         LINE(1:1) .EQ. '*')) THEN
            WRITE (NINP,2010) LINE
            WRITE (NLOG,2010) LINE
         ENDIF
         IF (LINE(1:4) .EQ. 'STOP') EXIT
      ENDDO
      WRITE (NLOG,1020)

      IF (LINE(1:4) .NE. 'STOP') CALL ERRMSG ('12',0)

      CLOSE   (NAUX)
      ENDFILE (NINP)
      REWIND  (NINP)

      RETURN
 1000 FORMAT (/'ESSENTIAL DIAGNOSTICS EVENTS LOGFILE:--')
 1010 FORMAT (/'[TXPROC]'//
     &   'INPUT DATA FILE (WITH REMOVED COMMENT LINES):'//80('='))
 1020 FORMAT (80('='))

 2000 FORMAT (A4)
 2010 FORMAT (A80)
 2020 FORMAT (A64)
      END


!-----------------------------------------------------------------------
!     Subroutine: 'COUNTER'   FIRST: 06 Sep 2011  LAST EDIT: 06 Sep 2011
!
!     PURPOSE:
!     Output to console the file number (four digits) or percentage of
!     accomplished jobs (six characters: " 100 %").
!
!     CALLED: COLLECT4 (Commute.for) ...
!
!     INPUT:
!     IERASE -- = 1, Erases previous data
!     ICOUNT -- File number or percent of accomplished job
!     IPERCENT  = 1, percent sign printed
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE COUNTER (IERASE,ICOUNT,IPERCENT)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /ENVIRO/ IOPSYS,ISYST,MODRUN,IGAUS,IPROG
      CHARACTER*8 DEL
      DATA DEL /''/

      IF (MODRUN .EQ. 0) RETURN

      IF (IERASE .GT. 0) THEN
         IF (IPERCENT .EQ. 0) THEN
            WRITE (*,"(A4\)") DEL(1:4)
         ELSE
            WRITE (*,"(A6\)") DEL(1:6)
         ENDIF
      ENDIF

      IF (ICOUNT .GE. 0) THEN
         WRITE (*,"(I4\)") ICOUNT
         IF (IPERCENT .GT. 0) WRITE (*,"(' %'\)")
      ENDIF

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'TIMDAT'    PREV.: 15 Jul 2009  LAST EDIT: 20 Mar 2015
!
!     PURPOSE:
!     Prints current date and time to logical unit number LUNIT.
!
!     CALLED: MAIN (ANCO.for), ??
!
!     CALLS: GETTIM (), GETDAT (), FLTX ().
!
!     INPUT:
!     LUNIT  -- Logical unit number for output device.
!               = 0, Console output
!
!     OUTPUT:
!     No output variables.
!
!     PRINTS:
!     One line with current date and time.
!
!     NOTE(S):
!     1. Uses platform-dependent routines GETTIM, GETDAT.
!-----------------------------------------------------------------------
      SUBROUTINE TIMDAT (LUNIT)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      CHARACTER*1 HOUR(2),MINU(2),SECO(2),CENT(2)
      CHARACTER*3 MONTH(12)
      DATA MONTH/'Jan','Feb','Mar','Apr','May','Jun',
     &   'Jul','Aug','Sep','Oct','Nov','Dec'/

      CALL GETTIM (IHOUR,IMIN,ISEC,I100TH)
      CALL GETDAT (IYEAR,IMON,IDAY)

      CALL FLTX (IHOUR, HOUR)
      CALL FLTX (IMIN,  MINU)
      CALL FLTX (ISEC,  SECO)
      CALL FLTX (I100TH,CENT)

      IF (LUNIT .EQ. 0) THEN
         WRITE (*    ,1000) IDAY,MONTH(IMON),IYEAR,HOUR,MINU,SECO,CENT
      ELSE
         WRITE (LUNIT,1000) IDAY,MONTH(IMON),IYEAR,HOUR,MINU,SECO,CENT
      ENDIF

      RETURN
 1000 FORMAT ('Date: ',I2,1X,A3,1X,I4,4X,'Time: ',
     &   2(2A1,':'),2A1,'.',2A1)
      END



!-----------------------------------------------------------------------
!     Subroutine: 'WATCH'                         LAST EDIT: 14 Jun 2011
!
!     PURPOSE:
!     Returns current split time in 100th of second (from 00:00 a.m.)
!
!     CALLED: MAIN (ANCO.for), ??
!
!     CALLS: GETTIM (), GETDAT ();
!
!     INPUT:
!     No input variables.
!
!     OUTPUT:
!     ISPLIT -- Current split time in 100th of second (from 00:00 a.m.)
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE WATCH (ISPLIT)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)

      CALL GETTIM (IHOUR,IMIN,ISEC,I100TH)
      CALL GETDAT (IYEAR,IMON,IDAY)

      ISPLIT = I100TH + 100 * (ISEC + 60 * (IMIN + 60 * IHOUR))

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'WATCH1'                        LAST EDIT: 14 Jun 2011
!
!     PURPOSE:
!     Returns time passed from the previous call of WATCH (), broken
!     down to hours, minutes, seconds and 100th parts of the second.
!
!     CALLED: MAIN (ANCO.for), ??
!
!     INPUT:
!     ISPLIT0  -- The moment in the past, measured in 100th of a second
!                 since 00:00:00.00 of a day;
!
!     OUTPUT:
!     IThour   -- The number of hours elapsed since ISPLIT0;
!     ITmin    -- The number of hours elapsed since ISPLIT0;
!     ITsec    -- The number of hours elapsed since ISPLIT0;
!     IT100th  -- The number of hours elapsed since ISPLIT0;
!
!     NOTE(S):
!-----------------------------------------------------------------------
      SUBROUTINE WATCH1 (ISPLIT0,IThour,ITmin,ITsec,IT100th)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)

      CALL GETTIM (IHOUR,IMIN,ISEC,I100TH)
      CALL GETDAT (IYEAR,IMON,IDAY)

      ISPLIT1 = I100TH + 100 * (ISEC + 60 * (IMIN + 60 * IHOUR))
      IDIFF   = ISPLIT1 - ISPLIT0
      IF (IDIFF .LT. 0) IDIFF = IDIFF + 24 * 60 * 60 * 100

      IT100th = MOD (IDIFF, 100)
      ITsec   = MOD (IDIFF /           (100), 60)
      ITmin   = MOD (IDIFF /      (60 * 100), 60)
      IThour  = IDIFF / (60 * 60 * 100)

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'WATCH2'                        LAST EDIT: 14 Jun 2011
!
!     PURPOSE:
!     Outputs to console the time passed from the previous call of
!     WATCH (), in format " done: 00:00:00.00".
!
!     CALLED: MAIN (ANCO.for), ??
!
!     INPUT:
!     ISPLIT0--
!
!     OUTPUT:
!     No output variables.
!
!     NOTE(S):
!-----------------------------------------------------------------------
      SUBROUTINE WATCH2 (ISPLIT0)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  LOUT,NLOG,NINP,NOUT,NAUX,NSCR

      CALL GETTIM (IHOUR,IMIN,ISEC,I100TH)
      CALL GETDAT (IYEAR,IMON,IDAY)

      ISPLIT1 = I100TH + 100 * (ISEC + 60 * (IMIN + 60 * IHOUR))
      IDIFF   = ISPLIT1 - ISPLIT0
      IF (IDIFF .LT. 0) IDIFF = IDIFF + 24 * 60 * 60 * 100

      IT100th = MOD (IDIFF, 100)
      ITsec   = MOD (IDIFF /       100 , 60)
      ITmin   = MOD (IDIFF / (60 * 100), 60)
      IThour  = IDIFF / (60 * 60 * 100)

      WRITE (*,1000) IThour,ITmin,ITsec,IT100th

      RETURN
 1000 FORMAT (' done:',I3,':',I2.2,':',I2.2,'.',I2.2)
      END



!-----------------------------------------------------------------------
!     Subroutine: 'LWATCH'                        LAST EDIT: 28 Jun 2011
!
!     PURPOSE:
!     Returns current split time in 100th of second (from 00:00 a.m. of
!     1 January 2000)
!
!     THEORY:
!     There are 24h * 60min * 60sec * 100th = 8,640,000 units in 1 day
!     Max INTEGER*4 Value =             2,147,483,647 units, = 248 days
!     One year            =             3,153,600,000
!     Max INTEGER*8 Value = 9,223,372,036,854,775,807
!
!     CALLED: MAIN (ANCO.for), ??
!
!     CALLS: GETTIM ();
!
!     INPUT:
!     No input variables.
!
!     OUTPUT:
!     LSPLIT -- Current split time in 100th of second,
!               (from 00:00 a.m. 1 January 2000)
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE LWATCH (LSPLIT)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  IOUT,NLOG,NINP,NOUT,NAUX,NSCR
      INTEGER*8 LSPLIT,LONGDAY
      INTEGER*4 MONTHS(12)
      DATA LONGDAY /8640000/
      DATA MONTHS /31,28,31,30,31,30,31,31,30,31,30,31/

      CALL GETDAT (IYEAR,MONTH,IDAY)

      LSPLIT = 0
      DO 110 I = 2011, IYEAR - 1
         LSPLIT = LSPLIT + LONGDAY * 365
  110 CONTINUE

      DO 120 I = 1, MONTH - 1
         LSPLIT = LSPLIT + LONGDAY * MONTHS(I)
  120 CONTINUE

      DO 130 I = 1, IDAY - 1
         LSPLIT = LSPLIT + LONGDAY
  130 CONTINUE

      CALL GETTIM (IHOUR,IMIN,ISEC,I100TH)

      LSPLIT = LSPLIT + I100TH + 100 * (ISEC + 60 * (IMIN + 60 * IHOUR))

      I100  = MOD (LSPLIT, 100)
      Isec  = MOD (LSPLIT / 100, 60)
      Imin  = MOD (LSPLIT / (60 * 100), 60)
      Ihour = MOD (LSPLIT / (60 * 60 * 100), 24)
      Idays = LSPLIT / (24 * 60 * 60 * 100)

!     WRITE (NOUT,1000) Idays,Ihour,Imin,Isec,I100
!1000 FORMAT (I8,'d',I2.2,'h',I2.2,'m',I2.2,'.',I2.2,'sec')

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'LWATCHTX'                      LAST EDIT: 28 Jun 2011
!
!     PURPOSE:
!     Returns time passed from the previous call of WATCH (), broken
!     down to hours, minutes, seconds and 100th parts of the second.
!
!     NOTES:
!     CHAR*16 1d00h00m00.00sec
!
!     CALLED: MAIN (ANCO.for), ??
!
!     INPUT:
!     ISPLIT0--
!
!     OUTPUT:
!     TEXT   -- "1d00h00m00.00sec" CHAR*16
!
!     NOTE(S):
!-----------------------------------------------------------------------
      SUBROUTINE LWATCHTX (LSPLIT0,TEXT)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      INTEGER*8 LSPLIT0,LSPLIT,LDIFF,LONGDAY
      INTEGER*4 MONTHS(12)
      CHARACTER*16 TEXT
      DATA LONGDAY /8640000/
      DATA MONTHS /31,28,31,30,31,30,31,31,30,31,30,31/

      CALL GETDAT (IYEAR,MONTH,IDAY)
      CALL GETTIM (IHOUR,IMIN,ISEC,I100TH)

      LSPLIT = 0
      DO 110 I = 2011, IYEAR - 1
         LSPLIT = LSPLIT + LONGDAY * 365
  110 CONTINUE

      DO 120 I = 1, MONTH - 1
         LSPLIT = LSPLIT + LONGDAY * MONTHS(I)
  120 CONTINUE

      DO 130 I = 1, IDAY - 1
         LSPLIT = LSPLIT + LONGDAY
  130 CONTINUE

      LSPLIT = LSPLIT + I100TH + 100 * (ISEC + 60 * (IMIN + 60 * IHOUR))
      LDIFF  = LSPLIT - LSPLIT0

      I100  = MOD (LDIFF, 100)
      Isec  = MOD (LDIFF /                (100), 60)
      Imin  = MOD (LDIFF /           (60 * 100), 60)
      Ihour = MOD (LDIFF /      (60 * 60 * 100), 24)
      Idays = LDIFF / (24 * 60 * 60 * 100)

      IF (Idays .GT. 0) THEN
         WRITE (FMT = "(I1,'d',I2.2,'h',I2.2,1H',I2.2,'.',I2.2,'sec')",
     &      UNIT = TEXT, IOSTAT = IOS) Idays,Ihour,Imin,Isec,I100
      ELSE
         WRITE (FMT = "(I4,'h',I2.2,1H',I2.2,'.',I2.2,'sec')",
     &      UNIT = TEXT, IOSTAT = IOS) Ihour,Imin,Isec,I100
      ENDIF

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'NUMSTR'    FIRST: 12 Mar 2010  LAST EDIT: 12 Mar 2010
!
!     PURPOSE:
!     ...
!
!     CALLED: COMMUTE (Commute.for)
!
!     INPUT:
!     NUMBER --
!
!     OUTPUT:
!     NUMSTR --
!
!     NOTE(S):
!-----------------------------------------------------------------------
      CHARACTER*12 FUNCTION NUMSTR (NUMBER)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)

      IF (NUMBER .LT. 10**3) THEN
         WRITE (FMT = "(I12)", UNIT = NUMSTR, IOSTAT = IOS) NUMBER
         RETURN
      ENDIF

      I10E3 = MOD (NUMBER,  10**3)
      I10E6 = MOD (NUMBER / 10**3, 1000)

      IF (NUMBER .LT. 10**6) THEN
         WRITE (FMT = "(I8,',',I3.03)", UNIT = NUMSTR, IOSTAT = IOS)
     &      I10E6,I10E3
         RETURN
      ENDIF

      I10E9 = MOD (NUMBER / 10**6, 1000)

!----    Max INT*4 = 2147,483,647

      WRITE (FMT = "(I4,2(',',I3.03))", UNIT = NUMSTR, IOSTAT = IOS)
     &   I10E9,I10E6,I10E3

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'LNUMSTR'   FIRST: 12 Mar 2010  LAST EDIT: 18 Mar 2014
!
!     PURPOSE:
!     CHARACTER*16 FUNCTION LNUMSTR (LONG)
!       Original Terms
!     0001,000,000,000
!
!     CALLED: COMMUTE (Commute.for)
!
!     INPUT:
!     NUMBER --
!
!     OUTPUT:
!     NUMSTR --
!
!     NOTE(S):
!-----------------------------------------------------------------------
      CHARACTER*16 FUNCTION LNUMSTR (LONG)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      INTEGER*8 LONG,L10E3,L10E6,L10E9,L10E12

      IF (LONG .LT. 10 ** 3) THEN
         WRITE (FMT = "(I16)", UNIT = LNUMSTR, IOSTAT = IOS) LONG
         RETURN
      ENDIF

      L10E3  = MOD (LONG,  10**3)
      L10E6  = MOD (LONG / 10**3, 1000)
      L10E9  = MOD (LONG / 10**6, 1000)
      L10E12 = MOD (LONG / 10**9, 1000)

      IF (LONG .LT. 10 ** 6) THEN
         WRITE (FMT = "(I12,',',I3.03)", UNIT = LNUMSTR, IOSTAT = IOS)
     &      L10E6,L10E3
         RETURN
      ENDIF

      IF (LONG .LT. 10 ** 9) THEN
         WRITE (FMT = "(I8,2(',',I3.03))", UNIT = LNUMSTR, IOSTAT = IOS)
     &      L10E9,L10E6,L10E3
         RETURN
      ENDIF

      WRITE (FMT = "(I4,3(',',I3.03))", UNIT = LNUMSTR, IOSTAT = IOS)
     &   L10E12,L10E9,L10E6,L10E3

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'INFORM'    PREV.: 10 Sep 2007  LAST EDIT: 13 Jul 2009
!
!     PURPOSE:
!     Save information about the molecule in a file for inverse problem.
!
!     CALLED: MAIN (ANCO.for)
!
!     INPUT:
!     MR     --
!     NS     --
!     NAT    --
!     ISA    --
!     ISOT   --
!     ATW    --
!     COR    --
!     T      --
!     F      --
!     ICR    --
!     ICLS   --
!     NINF   -- Logical Unit Number (LUN) for formatted file output
!     NAME   -- Char*64 Text Identifier of the Molecule (Comment)
!
!     OUTPUT:
!     No output variables.
!
!     FILE I/O:
!     The only purpose of this routine is to write to LUN formatted
!     data for subsequent solution of the inverse problem (SCAL).
!
!     PRINTS:
!     No information is printed.
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE INFORM (MR,NS,NAT,ISA,ISOT,ATW,COR,T,F,
     &   ICR,ICLS,NINF,NAME)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  IOUT,NLOG,NINP,NOUT,NAUX,NSCR
      COMMON /COOR/   NC1,NC2,NC3,NC4,NC5,IAC1,IAC2,IAC3,IAC4,IAC5
      CHARACTER*64 NAME
      CHARACTER*12 ICR(MR),ICLS(MR)
      CHARACTER* 2 ISA(NAT)
      DIMENSION IAC1(300,2),IAC2(300,3),IAC3(300,4),
     &   IAC4(300,4),IAC5(300,8)
      DIMENSION ISOT(NAT),ATW(NAT),COR(3,NAT)
      DIMENSION T(MR,MR),F(MR,MR)
      DIMENSION C(300),IC(300)
      DATA KEY /'INFO'/

      WRITE (NINF,1000) KEY
      WRITE (NINF,1010) NAME
      WRITE (NINF,1020) NAT

      DO 100 I = 1, NAT
  100 WRITE (NINF,1030) I,ISA(I),ISOT(I),ATW(I),(COR(J,I),J=1,3)

      WRITE (NINF,1040) NC1,NC2,NC3,NC4,NC5

      K = 0
      DO 110 I = 1, NC1
         K = K + 1
         WRITE (NINF,1100) K,ICR(K),(IAC1(I,J),J=1,2)
  110 CONTINUE
      DO 120 I = 1, NC2
         K = K + 1
         WRITE (NINF,1100) K,ICR(K),(IAC2(I,J),J=1,3)
  120 CONTINUE
      DO 130 I = 1, NC3
         K = K + 1
         WRITE (NINF,1100) K,ICR(K),(IAC3(I,J),J=1,4)
  130 CONTINUE
      DO 140 I = 1, NC4
         K = K + 1
         WRITE (NINF,1100) K,ICR(K),(IAC4(I,J),J=1,4)
  140 CONTINUE
      DO 150 I = 1, NC5
         K = K + 1
         WRITE (NINF,1100) K,ICR(K),(IAC5(I,J),J=1,8)
  150 CONTINUE

      WRITE (NINF,1200)

      NRR = NC1 + NC2 + NC3 + NC4 + NC5
      DO 210 J = 1, NS
         K = 0
         DO 200 I = 1, NRR
            IF (T(I,J) .EQ. 0.0D0) CYCLE
               K = K + 1
               IC(K) = I
               C(K) = T(I,J)
  200    CONTINUE

         K1 = MIN(K,3)
         WRITE (NINF,1210) J,ICLS(J),(IC(I),C(I),I=1,K1)
         IF (K .GT. 3) WRITE (NINF,1220) J,(IC(I),C(I),I=4,K)
  210 CONTINUE
      WRITE (NINF,1210) 0

      WRITE (NINF,1300)
      DO 300 I = 1, NS
  300 WRITE (NINF,1310) I,(F(I,J),J=1,I)

      RETURN
 1000 FORMAT (A4,2X,'Generated file. Do not edit!')
 1010 FORMAT (A64)
 1020 FORMAT ('ATOM'/I4/'CART')
 1030 FORMAT (I4,2X,A2,I4,4F16.8)
 1040 FORMAT ('INTE'/5I4)
 1100 FORMAT (I4,2X,A12,8I4)
 1200 FORMAT ('TRAN')
 1210 FORMAT (I4,2X,A12,2X,3(I4,F14.8))
 1220 FORMAT (I4,15X,'X',3(I4,F14.8))
 1300 FORMAT ('FORC')
 1310 FORMAT (I4,6F12.8:/(4X,6F12.8))
      END



!-----------------------------------------------------------------------
!     Subroutine: 'OPTION'    PREV.: 16 Aug 2009  LAST EDIT: 05 May 2013
!
!     PURPOSE:
!     Reads the sections of input file header concerning keys with
!     integer values, (printing) options, parameters with real values.
!
!     CALLED: MAIN (ANCO.for)
!
!     INPUT:
!     NOPT   --
!     NFLAG  --
!     KOPT   --
!     KFLAG  --
!     IDEFAU -- Default values for PARameters;
!
!     OUTPUT:
!     IOPT   -- Array of options (integer values);
!     IFLAG  -- Array or flags (0, 1);
!     IERR   -- = 0 if successful, = 1, keyword was illegal.
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE OPTION (NOPT,NFLAG,KOPT,KFLAG,IDEFAU,IOPT,IFLAG,IERR)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  IOUT,NLOG,NINP,NOUT,NAUX,NSCR
      DIMENSION IOPT(9),IDEFAU(9),IFLAG(12)
      CHARACTER* 4 KOPT(9),KFLAG(12)
      CHARACTER* 4 WORD(12)
      CHARACTER*80 LINE
      DIMENSION IVALUE(9)

      DO 100 I = 1, NOPT
  100 IOPT(I) = IDEFAU(I)

      READ (NINP,1000,ERR=810) (WORD(I),IVALUE(I),I=1,NOPT)

      DO 130 I = 1, NOPT
         IF (WORD(I) .EQ. ' ') EXIT
         DO 110 J = 1, NOPT
            IF (WORD(I) .EQ. KOPT(J)) GO TO 120
  110    CONTINUE
         GO TO 820
  120    CONTINUE
         IOPT(J) = IVALUE(I)
  130 CONTINUE

      DO 200 I = 1, NFLAG
  200 IFLAG(I) = 0

      READ (NINP,1010) (WORD(I),I=1,NFLAG)

      DO 230 I = 1, NFLAG
         IF (WORD(I) .EQ. ' ') EXIT
         DO 210 J = 1, NFLAG
            IF (WORD(I) .EQ. KFLAG(J)) GO TO 220
  210    CONTINUE
         GO TO 830
  220    CONTINUE
         IFLAG(J) = 1
  230 CONTINUE

  800 IERR = 0
      GO TO 900
  810 BACKSPACE (NINP)
      READ  (NINP,1100) LINE
      WRITE (NOUT,1110) LINE
      IERR = 1
      GO TO 900
  820 IERR = 2
      GO TO 900
  830 IERR = 3
  900 RETURN

 1000 FORMAT (9(A4,1X,I2,1X))
 1010 FORMAT (12(A4,1X))
 1100 FORMAT (A80)
 1110 FORMAT (/'[OPTION] Erroneous Input Data Line:/'
     &   'FORMAT (9(A4,1X,I2,1X)):'//A80)
      END



!-----------------------------------------------------------------------
!     Subroutine: 'MATOUT'    PREV.: 20 May 2006  LAST EDIT: 12 Oct 2017
!
!     PURPOSE:
!     Prints lower triangle of a square symmetric matrix with 6 digits
!     after decimal point.
!
!     CALLED: MAIN (ANCO.for), UNIVERSAL.
!
!     INPUT:
!     MARR   -- declared vertical dimension of a square matrix;
!     NDIM   -- actual dimension of a square matrix;
!     A      -- matrix to be printed;
!     MODE   -- = 0, print all-zero strings too;
!     IACC   -- Accracy: 1 = F12.4, 2 = F12.6, 3 = F12.8.
!
!     NOTE(S):
!     1. 29/01/2010 --
!        NCOL = 5, and 1100 FORMAT (I4,2X,5F14.8), same as MATOUTRE.
!     2. 05/10/2017 --
!        Accuracy of float numbers added.
!-----------------------------------------------------------------------
      SUBROUTINE MATOUT (MARR,NDIM,A,MODE,IACC)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  IOUT,NLOG,NINP,NOUT,NAUX,NSCR
      DIMENSION A(MARR,NDIM)
      CHARACTER*80 SPECI(3),SPECF(3)
      LOGICAL OUTPUT
      DATA NCOL /5/
      DATA TOL  /5.0D-9/
      DATA SPECI /'(4X,5(I12,2X))','(4X,5(I12,2X))','(4X,5(I12,2X))'/
      DATA SPECF /'(I4,2X,5F14.4)','(I4,2X,5F14.6)','(I4,2X,5F14.8)'/

      N = (NDIM - 1) / NCOL + 1
      DO 120 M = 1, N
         K = NCOL * (M - 1) + 1
         L = MIN0 (NCOL * M, NDIM)
         WRITE (NOUT,SPECI(IACC)) (J, J = K, L)

         DO 110 I = K, NDIM
            OUTPUT = .FALSE.
            IF (MODE .EQ. 0) OUTPUT = .TRUE.
            DO 100 J = K, MIN0(I,L)
               IF (ABS (A(I,J)) .GT. TOL) OUTPUT = .TRUE.
  100       CONTINUE
            IF (OUTPUT)
     &         WRITE (NOUT,SPECF(IACC)) I,(A(I,J),J=K,MIN0(I,L))
  110    CONTINUE
  120 CONTINUE

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'MATOUTID'  PREV.: 18 Feb 2003  LAST EDIT: 19 Oct 2017
!
!     PURPOSE:
!     Print square symmetric matrix with identifiers (CHAR*12).
!
!     CALLED: MAIN (ANCO.for), ??
!
!     INPUT:
!     MARR   -- Declared vertical dimension of a square array (A);
!     A      -- Square matrix to be printed;
!     NR     -- Actual dimension of a square matrix (A);
!     NC     -- Actual dimension of a square matrix (A);
!     ID     -- Char*12 Identifiers;
!
!     NOTE(S):
!     1. The number of columns (NCOL) is an adjustable parameter.
!-----------------------------------------------------------------------
      SUBROUTINE MATOUTID (MARR,NR,NC,A,ID)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  IOUT,NLOG,NINP,NOUT,NAUX,NSCR
      DIMENSION A(MARR,NC)
      CHARACTER*12 ID(NR)
      DATA NCOL /5/

      N = (NC - 1) / NCOL + 1
      DO 110 M = 1, N
         K = NCOL * (M - 1) + 1
         L = MIN0 (NCOL * M, NC)
         WRITE (NOUT,1000) (J,J=K,L)
         DO 100 I = K, NR
            WRITE (NOUT,1100) I,ID(I),(A(I,J),J=K,MIN0(I,L))
  100    CONTINUE
  110 CONTINUE

      RETURN

 1000 FORMAT (/16X,5(2X,I10))
 1100 FORMAT (I4,2X,A12,2X,5F12.4)
      END



!-----------------------------------------------------------------------
!     Subroutine: 'MATOUTRE'  PREV.: 01 Sep 2007  LAST EDIT: 24 Sep 2009
!
!     PURPOSE:
!     Prints rectangular non-symmetric matrix.
!
!     CALLED: MAIN (ANCO.for), ??
!
!     INPUT:
!     NDIM   -- declared vertical dimension of a rectangular matrix
!     NR     -- actual vertical dimension of a rectangular matrix
!     NC     -- actual horizontal dimension of a rectangular matrix
!     A      -- matrix to be printed
!
!     NOTES:
!     1. The number of columns (NCOL) is an adjustable parameter.
!-----------------------------------------------------------------------
      SUBROUTINE MATOUTRE (NDIM,NR,NC,A)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  LOUT,NLOG,NINP,NOUT,NAUX,NSCR
      DIMENSION A(NDIM,NC)
      DATA NCOL /5/

      N = (NC - 1) / NCOL + 1
      DO 110 M = 1, N
         K = NCOL * (M - 1) + 1
         L = MIN0 (NCOL * M, NC)
         WRITE (NOUT,1010) (J,J=K,L)
         DO 100 I = 1, NR
            IF (I .GT. 1 .AND. MOD (I,10) .EQ. 1) WRITE (NOUT,1020)
            WRITE (NOUT,1030) I,(A(I,J),J=K,L)
  100    CONTINUE
  110 CONTINUE

      RETURN
 1010 FORMAT (5X,5(I12,2X))
 1020 FORMAT (1X)
 1030 FORMAT (I4,2X,5F14.6)
      END



!-----------------------------------------------------------------------
!     Subroutine: 'VECOUT'    PREV.: 06 Sep 2007  LAST EDIT: 05 Oct 2017
!
!     PURPOSE:
!     General subroutine to print eigenvalues and eigenvectors.
!
!     CALLED: MAIN (ANCO.for)
!
!     INPUT:
!     MARR   -- Vertical dimension of array VECTOR;
!     NDIM   -- Actual dimension of square matrix VECTOR;
!     VECTOR -- The matrix of eigenvectors;
!     VALUE  -- Array holding eigenvalues;
!     IDENT  -- (CHAR*12) Text identifiers associated with variables;
!
!     NOTES:
!     1. 29/01/2010 --
!        Changed from specialized routine for output of eigenvalues and
!        eigenvectors of G-matrix to general purpose one.
!-----------------------------------------------------------------------
      SUBROUTINE VECOUT (MARR,NDIM,VECTOR,VALUE,IDENT)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  IOUT,NLOG,NINP,NOUT,NAUX,NSCR
      CHARACTER*12 IDENT(NDIM)
      DIMENSION VECTOR(MARR,NDIM),VALUE(NDIM)
      DATA NCOL /5/

      N = (NDIM - 1) / NCOL + 1
      DO 110 M = 1, N
         K = NCOL * (M - 1) + 1
         L = MIN0 (NCOL * M, NDIM)
         WRITE (NOUT,1010) (J,J=K,L)
         WRITE (NOUT,1020) (VALUE(J),J=K,L)
         DO 100 I = 1, NDIM
            IF (MOD (I,10) .EQ. 1) WRITE (NOUT,1030)
            WRITE (NOUT,1040) I,IDENT(I),(VECTOR(I,J),J=K,L)
  100    CONTINUE
  110 CONTINUE

      RETURN

 1010 FORMAT (/22X,5(I6,5X))
 1020 FORMAT (6X,'Eigenvalue -- ',5F11.4)
 1030 FORMAT (1X)
 1040 FORMAT (I4,2X,A12,2X,5F11.6)
      END



!-----------------------------------------------------------------------
!     Subroutine: 'VECOUT1'                       LAST EDIT: 19 Apr 2018
!
!     PURPOSE:
!     General subroutine to print eigenvalues and eigenvectors.
!
!     CALLED: MAIN (ANCO.for)
!
!     INPUT:
!     MARR   -- Vertical dimension of array VECTOR;
!     NDIM   -- Actual dimension of square matrix VECTOR;
!     VECTOR -- The matrix of eigenvectors;
!     VALUE  -- Array holding eigenvalues;
!     IDENT  -- (CHAR*12) Text identifiers associated with variables;
!
!     NOTES:
!     1. 29/01/2010 --
!        Changed from specialized routine for output of eigenvalues and
!        eigenvectors of G-matrix to general purpose one.
!-----------------------------------------------------------------------
      SUBROUTINE VECOUT1 (MARR,NROW,NCOL,VECTOR,VALUE,IDENT)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  IOUT,NLOG,NINP,NOUT,NAUX,NSCR
      CHARACTER*12 IDENT(NROW)
      DIMENSION VECTOR(MARR,*),VALUE(*)
      DATA MCOL /5/

      N = (NCOL - 1) / MCOL + 1
      DO 110 M = 1, N
         K = MCOL * (M - 1) + 1
         L = MIN0 (MCOL * M, NCOL)
         WRITE (NOUT,1010) (J,J=K,L)
         WRITE (NOUT,1020) (VALUE(J),J=K,L)
         DO 100 I = 1, NROW
            IF (MOD (I,10) .EQ. 1) WRITE (NOUT,1030)
            WRITE (NOUT,1040) I,IDENT(I),(VECTOR(I,J),J=K,L)
  100    CONTINUE
  110 CONTINUE

      RETURN

 1010 FORMAT (/22X,5(I6,5X))
 1020 FORMAT (6X,'Eigenvalue -- ',5F11.4)
 1030 FORMAT (1X)
 1040 FORMAT (I4,2X,A12,2X,5F11.6)
      END



!-----------------------------------------------------------------------
!     Subroutine: 'COPYAR'                        LAST EDIT: 15 Jul 2009
!
!     PURPOSE:
!     Copies one vector to another.
!
!     CALLED: MAIN (ANCO.for)
!
!     INPUT:
!     N      -- Dimension of the vectors ARR1, ARR2;
!     ARR1   -- Source vector to be copied;
!
!     OUTPUT:
!     ARR2   -- Destination vector to which the copy should be placed.
!
!     PRINTS:
!     No information is printed.
!
!     NOTE(S):
!     1. May rename COPYAR to CPYVEC.
!     2. Implicit dynamic allocation is used. (*****)
!     3. In FORTRAN 90 this routine is redundant, ARR2 = ARR1.
!-----------------------------------------------------------------------
      SUBROUTINE COPYAR (N,ARR1,ARR2)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      DIMENSION ARR1(N),ARR2(N)

      DO 100 I = 1, N
  100 ARR2(I) = ARR1(I)

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'COPYDIP1'                      LAST EDIT: 15 Jul 2009
!
!     PURPOSE:
!     Copies first derivatives of dipole moment vector to another array
!
!     CALLED: MAIN (ANCO.for); ???
!
!     INPUT:
!     NQ     -- Dimension of the vectors ARR1, ARR2;
!     ARR1   -- Array to be copied;
!
!     OUTPUT:
!     ARR2   -- Destination array to which the copy should be placed.
!
!     PRINTS:
!     No information is printed.
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE COPYDIP1 (NQ,ARR1,ARR2)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      DIMENSION ARR1(3,NQ),ARR2(3,NQ)

      DO 100 L = 1, 3
      DO 100 I = 1, NQ
  100 ARR2(L,I) = ARR1(L,I)

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'COPYDIP2'                      LAST EDIT: 15 Jul 2009
!
!     PURPOSE:
!     Copies second derivatives of dipole moment vector to another array
!
!     CALLED: MAIN (ANCO.for); ???
!
!     INPUT:
!     NDIM   -- Declared second dimension of dipole moment derivatives
!               matrix [3,N,N];
!     NQ     -- Dimension of the vectors ARR1, ARR2;
!     ARR1   -- Source array to be copied;
!
!     OUTPUT:
!     ARR2   -- Destination array to which the copy should be placed.
!
!     PRINTS:
!     No information is printed.
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE COPYDIP2 (NDIM,NQ,ARR1,ARR2)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      DIMENSION ARR1(3,NDIM,NQ),ARR2(3,NDIM,NQ)

      DO 100 L = 1, 3
      DO 100 I = 1, NQ
      DO 100 J = 1, NQ
  100 ARR2(L,I,J) = ARR1(L,I,J)

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'COPYDIP3'                      LAST EDIT: 15 Jul 2009
!
!     PURPOSE:
!     Copies third derivatives of dipole moment vector to another array
!
!     CALLED: MAIN (ANCO.for); ???
!
!     INPUT:
!     NDIM   -- Declared second & third dimension of dipole moment
!               derivatives matrix [3,N,N,N];
!     NQ     -- Dimension of the vectors ARR1, ARR2;
!     ARR1   -- Source array to be copied;
!
!     OUTPUT:
!     ARR2   -- Destination array to which the copy should be placed.
!
!     PRINTS:
!     No information is printed.
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE COPYDIP3 (NDIM,NQ,ARR1,ARR2)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      DIMENSION ARR1(3,NDIM,NDIM,NQ),ARR2(3,NDIM,NDIM,NQ)

      DO 100 L = 1, 3
      DO 100 I = 1, NQ
      DO 100 J = 1, NQ
      DO 100 K = 1, NQ
  100 ARR2(L,I,J,K) = ARR1(L,I,J,K)

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'COPYPOL1'                      LAST EDIT: 15 Jul 2009
!
!     PURPOSE:
!     Copies first derivatives of polarizability vector to another array
!
!     CALLED: MAIN (ANCO.for); ???
!
!     INPUT:
!     NQ     -- Dimension of the vectors ARR1, ARR2;
!     ARR1   -- Source array to be copied;
!
!     OUTPUT:
!     ARR2   -- Destination array to which the copy should be placed.
!
!     PRINTS:
!     No information is printed.
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE COPYPOL1 (NQ,ARR1,ARR2)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      DIMENSION ARR1(3,3,NQ),ARR2(3,3,NQ)

      DO 100 K = 1, 3
      DO 100 L = 1, 3
      DO 100 I = 1, NQ
  100 ARR2(K,L,I) = ARR1(K,L,I)

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'COPYPOL2'                      LAST EDIT: 15 Jul 2009
!
!     PURPOSE:
!     Copies 2nd derivatives of polarizability vector to another array
!
!     CALLED: MAIN (ANCO.for)
!
!     INPUT:
!     NDIM   --
!     NQ     -- Dimension of the vectors ARR1, ARR2;
!     ARR1   -- Source array to be copied;
!
!     OUTPUT:
!     ARR2   -- Destination array to which the copy should be placed.
!
!     PRINTS:
!     No information is printed.
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE COPYPOL2 (NDIM,NQ,ARR1,ARR2)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      DIMENSION ARR1(3,3,NDIM,NQ),ARR2(3,3,NDIM,NQ)

      DO 100 K = 1, 3
      DO 100 L = 1, 3
      DO 100 I = 1, NQ
      DO 100 J = 1, NQ
  100 ARR2(K,L,I,J) = ARR1(K,L,I,J)

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'FLAG'                          LAST EDIT: 15 Jul 2009
!
!     PURPOSE:
!     Standard routine to be called upon detected run-time error.
!
!     CALLED: MAIN (ANCO.for), ???
!
!     CALLS: TIMDAT
!
!     INPUT:
!     IERR   --
!
!     OUTPUT:
!     No output variables.
!
!     PRINTS:
!     ...
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE FLAG (IERR)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  IOUT,NLOG,NINP,NOUT,NAUX,NSCR

      CALL TIMDAT (NOUT)
      WRITE (NOUT,1000) IERR
      WRITE (*,   1000) IERR
      WRITE (NOUT,1100)

      STOP

 1000 FORMAT (/,4X,8('*'),' Error # ',I4,' have been detected.')
 1100 FORMAT (/,10X,10('*'),' Execution Terminated ',10('*')/78('='))
      END



!-----------------------------------------------------------------------
!     Subroutine: 'FINISH'                        LAST EDIT: 03 Sep 2012
!
!     PURPOSE:
!     Routine to be called upon normal termination of the program.
!
!     CALLED: MAIN (ANCO.for), ??
!
!     CALLS: TIMDAT
!
!     INPUT:
!     No input variables.
!
!     OUTPUT:
!     No output variables.
!
!     PRINTS:
!     ...
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE FINISH
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  IOUT,NLOG,NINP,NOUT,NAUX,NSCR

      WRITE (*,   1000)
      WRITE (NOUT,1000)
      WRITE (NLOG,1000)
      CALL TIMDAT (NOUT)
!     CALL TIMDAT (NLOG)
      STOP

 1000 FORMAT (/'Normal Termination of the Program.')
      END



!-----------------------------------------------------------------------
!     Subroutine: 'FLTX'           PREV.: 15 Jul 2009  LAST: 20 Nov 2015
!
!     PURPOSE:
!     Converts an integer number < 100 to (Character*1) text (2).
!
!     CALLED: TIMDAT (*)
!
!     INPUT:
!     INT    -- A positive integer number, smaller than 100.
!
!     OUTPUT:
!     CHAR  -- An array of two characters in format (Character*1)
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE FLTX (INT,CHAR)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      CHARACTER*1 CHAR(2)
      CHARACTER*1 DIGIT(10)
      DATA DIGIT /'0','1','2','3','4','5','6','7','8','9'/

      CHAR(1) = DIGIT (INT / 10      + 1)
      CHAR(2) = DIGIT (MOD (INT, 10) + 1)

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'XBLANC'         FIRST: 05 Sep 2009  LAST: 18 Jan 2016
!
!     PURPOSE:
!     Remove blancs in the string array
!     ...
!
!     CALLED: ESTCOR (IntCor.for)
!
!     INPUT:
!     LEN0   --
!     STR0   --
!     LEN1   --
!
!     OUTPUT:
!     STR1   --
!     LEN    --
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE XBLANC (LEN0,STR0,LEN1,STR1,LEN)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      CHARACTER*1 STR0(LEN0),STR1(LEN1)

      DO 100 I = LEN0, 1, -1
         IF (STR0(I) .EQ. ' ') CYCLE
         LEN = MIN (LEN0,I+1)
         EXIT
  100 CONTINUE

      L = 0
      DO 110 I = 1, LEN
         IF (STR0(I) .EQ. ' ') CYCLE
         L = L + 1
         IF (L .GT. LEN1) EXIT
         STR1(L) = STR0(I)
  110 CONTINUE

      DO 120 I = L + 1, LEN1
  120 STR1(I) = ' '

      LEN = L

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'STRLEN'                        LAST EDIT: 05 Sep 2009
!
!     PURPOSE:
!     Measures and returns the length of text string (Char*80),
!     terminated by a <Space> character.
!
!     CALLED: MAIN (ANCO.for), ??
!
!     INPUT:
!     STRING --
!
!     OUTPUT:
!     LEN    --
!
!     PRINTS:
!     No information is printed.
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE STRLEN (STRING,LEN)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      CHARACTER*80 STRING

      DO 100 I = 1, 80
         IF (STRING(I : I) .NE. ' ') CYCLE
            LEN = I - 1
            EXIT
  100 CONTINUE

      RETURN
      END



!-----------------------------------------------------------------------
!     Function: 'LENSTR'                          LAST EDIT: 25 Sep 2009
!
!     PURPOSE:
!     Measures and returns the length of text string (Char*1),
!     terminated by the <Space> character.
!
!     CALLED: MAIN (ANCO.for), ??
!
!     INPUT:
!     STRING -- Text string (Char*1);
!     LEN    -- Maximum length (array dimension);
!
!     OUTPUT:
!     LENSTR -- Value of string length is returned via FUNCTION name.
!
!     PRINTS:
!     No information is printed.
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      INTEGER*4 FUNCTION LENSTR (STRING,LEN)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      CHARACTER*1 STRING(*)

      LENSTR = LEN
      DO 100 I = 1, LEN
         IF (STRING(I) .EQ. ' ') THEN
            LENSTR = I - 1
            EXIT
         ENDIF
  100 CONTINUE

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'SETZERO1'                         LAST EDIT: 22 Jul 2009
!
!     PURPOSE:
!     Initializes real one-dimensional array to zero.
!
!     CALLED: MAIN (ANCO.for), ??
!
!     INPUT:
!     ARR    -- (REAL*16) One-dimensional array ARR, arbitrary content;
!     NDIM   -- The number of elements in one-dimensional array ARR.
!
!     OUTPUT:
!     ARR    -- All elements are equial to 0.0D0.
!
!     NOTE(S):
!     1. Rename SETZERO1 >> SETZERO1
!-----------------------------------------------------------------------
      SUBROUTINE SETZERO1 (NDIM,A)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      DIMENSION A(NDIM)

      DO 100 I = 1, NDIM
  100 A(I) = 0.0D0

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'SETZ2'                         LAST EDIT: 15 Jul 2009
!
!     PURPOSE:
!     Initializes real two-dimensional array to zero.
!
!     CALLED: MAIN (ANCO.for), ??
!
!     INPUT:
!     NDIM   --
!     NROW   --
!     NCOL   --
!
!     OUTPUT:
!     A      --
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE SETZ2 (NDIM,NROW,NCOL,A)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      DIMENSION A(NDIM,NCOL)

      DO 100 J = 1, NCOL
      DO 100 I = 1, NROW
  100 A(I,J) = 0.0D0

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'SETUNIT'   PREV.: 20 Sep 2007  LAST EDIT: 14 Sep 2009
!
!     PURPOSE:
!     Initialize square array A as a unit matrix.
!
!     CALLED: MAIN (ANCO.for), FNDSYM (Sym.for)
!
!     INPUT:
!     MDIM   -- Vertical dimension of array A
!     N      -- the number of rows/columns in matrix A
!
!     OUTPUT:
!     A      -- Resulting array A initialized to unity matrix
!
!     PRINTS:
!     No information is printed.
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE SETUNIT (MDIM,N,A)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      DIMENSION A(MDIM,N)

      DO 100 I = 1, N
      DO 100 J = 1, N
         A(I,J) = 0.0D0
         IF (I .EQ. J) A(I,J) = 1.0D0
  100 CONTINUE

      RETURN
      END



!-----------------------------------------------------------------------
!     Subroutine: 'DEFINE'                        LAST EDIT: 15 Jul 2009
!
!     PURPOSE:
!     Initializes I/O channel labelled as (LUN) for subsequent
!     unformatted random access read/write of same-size blocks
!
!     CALLED: MAIN (ANCO.for), ??
!
!     INPUT:
!     LUN    --
!     LENREC --
!     IOUT   --
!
!     OUTPUT:
!     No output variables.
!
!     PRINTS:
!     No information is printed.
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE DEFINE (LUN,LENREC,IOUT)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /ENVIRO/ IOPSYS,ISYST,MODRUN,IGAUS,IPROG
      COMMON /IOLUN/  LOUT,NLOG,NINP,NOUT,NAUX,NSCR

      IF (ISYST .GT. 0) WRITE (*,   1000) LUN,LENREC
      IF (IOUT  .NE. 0) WRITE (NOUT,1000) LUN,LENREC
      OPEN (UNIT = LUN, STATUS = 'SCRATCH',
     &   ACCESS = 'DIRECT', FORM = 'UNFORMATTED', RECL = LENREC)

      RETURN

 1000 FORMAT (/,'<< DEFINE >>',
     &   '  Channel # ',I2,3X,'Size = ',I6,' bytes per record.')
      END



!-----------------------------------------------------------------------
!     Subroutine: 'ERRMSG'                        LAST EDIT: 15 Jul 2009
!
!     PURPOSE:
!     Prints and outputs to console the explanation of an error message
!     given its code and additional parameter.
!
!     CALLED: MAIN (ANCO.for), ??
!
!     CALLS: TIMDAT
!
!     INPUT:
!     CODE   --
!     IEXIT  --
!
!     OUTPUT:
!     No output variables.
!
!     PRINTS:
!     ...
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE ERRMSG (CODE,IEXIT)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  LOUT,NLOG,NINP,NOUT,NAUX,NSCR
      CHARACTER*2 CODE
      CHARACTER*80 MESSAGE(30)

!     Types of possible errors:
!     1 - keywords; 2 - errors in parameters; 3 - beyond limits;
!     4 - runtime errors.

      DATA MESSAGE/
     &'11Keyword <<ANCO>> opening the data file missing.',
     &'12Keyword <<STOP>> terminating the data file missing.',
     &'21[OPTION] Unrecognized parameter. Exit error code:--',
     &'13Keywords *STRU* or *CART* for Cartesian coordinates missing.',
     &'31Total number of atoms is greater than allowed:--',
     &'32The number of internal coordinates of particular type > limit',
     &'33Bad atom number in the definition of an internal coordinate.',
     &'34Total number of internal coordinates exceeded the limit:--',
     &'22Angle bending coordinate may not correspond to 180deg angle.',
     &'23[TRANS] Wrong index of local symmetry coordinate.',
     &'41[JACOBI] Limit of iterations during diagonalization exceeded.',
     &'42[REDUND] Declared redundant coordinate is in fact independent',
     &'24[LOCAT] Incorrect number of force constants.',
     &'43Cannot transform Cartesian FC into a redundant set of IC.',
     &'44Cannot transform Cartesian FC into a redundant set of IC.',
     &'14Keyword *FORC* before lower triangle of Cartesian FC absent.',
     &'15When internal coordinates are undefined, need *FORC* anf F.C.',
     &'45[TQL2] Error during diagonalization of F.C. matrix.',
     &'46The force constant matrix is not positively defined.',
     &'47Denerate matrix.',
     &'16Keyword *MASS* for defining atom masses for isotope missing.',
     &'25Atom in definition of the isotopic species has wrong index:--',
     &'26[ISOTOP] Mass number of the isotope was not recognized.',
     &'48[JACOBI] Error during diagonalization.',
     &'27Error in definition of scale factors.',
     &'28No scale factor or more than one is associated with IC.',
     &'  *',
     &'  *',
     &'  *',
     &'  *'/

      CALL TIMDAT (NOUT)

      DO 100 I = 1, 30
         IF (MESSAGE(I)(1 : 2) .EQ. CODE) THEN
            WRITE (NOUT,1000) MESSAGE(I)(3 : 80)
            WRITE (*   ,1000) MESSAGE(I)(3 : 80)
            STOP
         ENDIF
  100 CONTINUE
      WRITE (NOUT,1000) CODE
      WRITE (*   ,1000) CODE
      IF (IEXIT .GT. 0) THEN
         WRITE (NOUT,1010) IEXIT
         WRITE (*   ,1010) IEXIT
      ENDIF
      WRITE (NOUT,1020)
      WRITE (*   ,1020)
      STOP

 1000 FORMAT (/'Fatal error has been detected:'/A78)
 1010 FORMAT ('Exit code: ',I4)
 1020 FORMAT (/'***** Abnormal program termination *****'/)
      END



!-----------------------------------------------------------------------
!     Subroutine: 'RENAME'         FIRST: 29 Apr 2007  LAST: 30 Apr 2007
!
!     PURPOSE: System option -- changes names of older files
!
!     CALLED: MAIN (ANCO.for)
!
!     USAGE:
!     --------------------------
!     RENA
!     CHO
!     CH2O_DFT_6-311xxG(dp)
!     --------------------------
!
!     INPUT:
!     NX     -- the number of Cartesian coordinates
!     NAUX#  -- logical unit numbers for reading/writing *.Fch files
!
!     OUTPUT: None.
!
!     PRINTS:
!     No information is printed.
!
!     NOTES:
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE RENAME (NX,FILE1,FILE2,NAUX1,NAUX2)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  IOUT,NLOG,NINP,NOUT,NAUX,NSCR
      CHARACTER* 64 FILE1,FILE2,FILE
      CHARACTER*128 LINE
      CHARACTER*  8 INDEX,FORA

!-----------------------------------------------------------------------

      WRITE (Fmt = "('DX/',A3,'00000','.fch')", UNIT = FILE,
     +   IOSTAT = IOS) FILE1(1:3)
      OPEN (UNIT = NAUX1, FILE = FILE, ACTION = 'READ', ERR = 010)
      LEN = LENSTR (FILE2,64)

      FILE = FILE2 (1:MIN0(24,LEN)) // '.fch'
      WRITE (*,"('Copy file to: ',A)") FILE(1:LENSTR(FILE,64))

      FILE = 'DX/' // FILE2 (1:MIN0(24,LEN)) // '.fch'
      OPEN (UNIT = NAUX2, FILE = FILE, ERR = 010)

      DO WHILE (.NOT. EOF (NAUX1))
         READ (NAUX1,"(A128)") LINE
         DO L = 128, 1, -1
            IF (LINE(L:L) .NE. ' ') EXIT
         ENDDO
         WRITE (Fmt = "('(A',I3.3,')')", UNIT=FORA, IOSTAT=IOS) L
         WRITE (NAUX2,FORA) LINE(1:L)
      ENDDO
      CLOSE (NAUX1)
      CLOSE (NAUX2)
  010 CONTINUE

!-----------------------------------------------------------------------

      NFILE1 = 4 * NX
      DO 100 I = 1, NFILE1
         WRITE (Fmt = "('DX/',A3,'1',I4.4,'.fch')", UNIT = FILE,
     +      IOSTAT = IOS) FILE1(1:3), I
         OPEN (UNIT = NAUX1, FILE = FILE, ACTION = 'READ', ERR = 110)

         LEN = LENSTR (FILE2,64)
         WRITE (Fmt = "('_1_',I5.5)", UNIT=INDEX, IOSTAT=IOS) I

         FILE = FILE2 (1:MIN0(24,LEN)) // INDEX // '.fch'
         WRITE (*,"('Copy file to: ',A)") FILE(1:LENSTR(FILE,64))

         FILE = 'DX/' // FILE2 (1:MIN0(24,LEN)) // INDEX // '.fch'
         OPEN (UNIT = NAUX2, FILE = FILE, ERR = 110)

         DO WHILE (.NOT. EOF(NAUX1))
            READ (NAUX1,"(A128)") LINE
            DO L = 128, 1, -1
               IF (LINE(L:L) .NE. ' ') EXIT
            ENDDO
            WRITE (Fmt = "('(A',I3.3,')')", UNIT=FORA, IOSTAT=IOS) L
            WRITE (NAUX2,FORA) LINE(1:L)
         ENDDO

         CLOSE (NAUX1)
         CLOSE (NAUX2)
  100 CONTINUE
  110 CONTINUE

!-----------------------------------------------------------------------

      NFILE2 = 1 + 2 * NX ** 2
      DO 200 I = 1, NFILE2
         WRITE (Fmt = "('DX/',A3,'2',I4.4,'.fch')", UNIT = FILE,
     +      IOSTAT = IOS) FILE1(1:3), I
         OPEN (UNIT = NAUX1, FILE = FILE, ACTION = 'READ', ERR = 210)

         WRITE (Fmt = "('_2_',I5.5)", Unit = INDEX, IOStat = IOS) I

         LEN = LENSTR (FILE2,64)
         FILE = FILE2 (1:MIN0(24,LEN)) // INDEX // '.fch'
         WRITE (*,"('Copy file to: ',A)") FILE(1 : LENSTR(FILE,64))

         FILE = 'DX/' // FILE2 (1:MIN0(24,LEN)) // INDEX // '.fch'
         OPEN (UNIT = NAUX2, FILE = FILE, ERR = 210)

         DO WHILE (.NOT. EOF(NAUX1))
            READ (NAUX1,"(A128)") LINE
            DO L = 128, 1, -1
               IF (LINE(L:L) .NE. ' ') EXIT
            ENDDO
            WRITE (Fmt = "('(A',I3.3,')')", UNIT=FORA, IOSTAT=IOS) L
            WRITE (NAUX2,FORA) LINE(1:L)
         ENDDO

         CLOSE (NAUX1)
         CLOSE (NAUX2)
  200 CONTINUE
  210 CONTINUE

      RETURN
      END



!-----------------------------------------------------------------------
!     BLOCK DATA                                  LAST EDIT: 24 Sep 2018
!
!     PURPOSE:
!     Stores in two COMMON blocks MENDEL and ISOTAB the chemical symbols
!     and atomic masses of most common isotopes, plus masses of selected
!     isotopic species
!
!     ALGORITHM:
!     ISOTI holds for (I)-th element atomic charge as ISOTI(I)/1000 and
!     nearest mass as ISOTI(I)-(ISOTI(I)/1000)*1000. E.g., (37)Cl=17037.
!
!     REFERENCE(S):
!     1. Gurvich
!
!     OUTPUT:
!     WEIGHT --
!     CHEM   --
!     MISOT  --
!     ISOTI  --
!     WISOT  --
!
!     NOTE(S):
!     1. https://software.intel.com/en-us/articles/fdiag6375
!-----------------------------------------------------------------------
      BLOCK DATA
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /MENDEL/ WEIGHT(100),CHEM(100)
      COMMON /ISOTAB/ WISOT(12),MISOT,ISOTI(12)
      COMMON /CHBOND/ BONDRANG,VALENTYP,NRECORD,IATNBOND,BONDTYPE
!----    ALSO: Modify this COMMON block everywhere
      DIMENSION IATNBOND(2,17),BONDRANG(2,17),VALENTYP(17)
      CHARACTER*12 BONDTYPE(17)
      CHARACTER* 2 CHEM

      DATA MISOT /8/
      DATA ISOTI /1002,         1003,         5010,         6013,
     &            7015,         8017,         8018,        17037,
     &               0,            0,            0,            0/
      DATA WISOT /
     &    2.01410222D0,   3.01604972D0,  10.0129369 D0,  13.00335508D0,
     &   15.00010930D0,  16.99913330D0,  17.99915996D0,  36.96590302D0,
     &    0.0       D0,   0.0       D0,   0.0       D0,   0.0       D0/
!----    http://www.ciaaw.org/boron.htm
      DATA WEIGHT /
     &    1.00782522D0,   2.01410222D0,   7.016005  D0,   9.012183  D0,
     &   11.00930517D0,  12.00000000D0,  14.003074  D0,  15.99491502D0,
     &   18.998405  D0,   0.0       D0,  22.989770  D0,  23.985044  D0,
     &   26.981541  D0,  27.976929  D0,  30.973763  D0,  31.972073  D0,
     &   34.96885359D0,   0.0       D0,  38.963709  D0,  39.962592  D0,
     &   44.955917  D0,  47.947949  D0,  50.943964  D0,  51.940510  D0,
     &   54.938046  D0,  55.934934  D0,  58.933188  D0,  57.935336  D0,
     &   62.929590  D0,  63.929140  D0,  68.925579  D0,  73.921179  D0,
     &   74.921600  D0,  79.916525  D0,  78.918332  D0,   0.0       D0,
     &   84.911799  D0,  87.905628  D0,  88.905866  D0,  89.904710  D0,
     &   92.906380  D0,  97.905410  D0,   0.0       D0, 101.90435   D0,
     &  102.90551   D0, 105.90349   D0, 106.90509   D0, 111.90276   D0,
     &  114.90387   D0, 119.90221   D0, 120.90382   D0, 129.90623   D0,
     &  126.90448   D0, 131.90415   D0, 132.90547   D0, 137.90523   D0,
     &  138.90640   D0, 139.90548   D0, 140.90770   D0, 141.90777   D0,
     &    0.0       D0, 151.91775   D0, 152.92126   D0, 157.92412   D0,
     &  158.92539   D0, 163.92922   D0, 164.93038   D0, 165.93033   D0,
     &  168.93424   D0, 173.93888   D0, 174.94080   D0, 179.94657   D0,
     &  180.94803   D0, 183.95097   D0, 186.95579   D0, 191.96151   D0,
     &  192.96296   D0, 194.96480   D0, 196.96655   D0, 201.97064   D0,
     &  204.97444   D0, 207.97666   D0, 208.98040   D0,   0.00000   D0,
     &    0.00000   D0,   0.00000   D0,   0.00000   D0,   0.00000   D0,
     &    0.00000   D0, 232.03807   D0,   0.00000   D0, 238.05082   D0,
     &    0.00000   D0,   0.00000   D0,   0.00000   D0,   0.00000   D0,
     &    0.00000   D0,   0.00000   D0,   0.00000   D0,   0.00000   D0/
      DATA CHEM /
     &   ' H',' D','Li','Be',' B',
     &   ' C',' N',' O',' F','  ',
     &   'Na','Mg','Al','Si',' P',
     &   ' S','Cl','  ',' K','Ca',
     &   'Sc','Ti',' V','Cr','Mn',
     &   'Fe','Co','Ni','Cu','Zn',
     &   'Ga','Ge','As','Se','Br',
     &   '  ','Rb','Sr',' Y','Zr',
     &   'Nb','Mo','Tc','Ru','Rh',
     &   'Pd','Ag','Cd','In','Sn',
     &   'Sb','Te',' I','Xe','Cs',
     &   'Ba','La','Ce','Pr','Nd',
     &   'Pm','Sm','Eu','Gd','Tb',
     &   'Dy','Ho','Er','Tm','Yb',
     &   'Lu','Hf','Ta',' W','Re',
     &   'Os','Ir','Pt','Au','Hg',
     &   'Tl','Pb','Bi','Po','At',
     &   'Rn','Fr','Ra','Ac','Th',
     &   'Pa',' U','Np','Pu','Am',
     &   'Cm','Bk','Cf','Es','Fm'/

      DATA NRECORD /17/
      DATA IATNBOND /1,1,  1,6,  1,7,  1,8,  6,6,
     &               6,6,  6,6,  6,6,  6,7,  6,7,
     &               6,7,  6,8,  6,8,  6,9,  6,17,
     &               6,53, 1,9/
      DATA BONDRANG /0.70 D0,0.80 D0, 1.05 D0,1.15 D0, 1.00 D0,1.10 D0,
     &               0.95 D0,1.00 D0, 1.45 D0,1.60 D0, 1.375D0,1.45 D0,
     &               1.30 D0,1.375D0, 1.20 D0,1.30 D0, 1.40 D0,1.55 D0,
     &               1.30 D0,1.40 D0, 1.20 D0,1.30 D0, 1.25 D0,1.35 D0,
     &               1.15 D0,1.25 D0, 1.30 D0,1.40 D0, 1.70 D0,1.80 D0,
     &               2.10 D0,2.20 D0, 0.90 D0,1.00 D0/
!----    C-C = 1.532 (Ethane)    1.45  - 1.60
!----    C^C = 1.395 (Benzene)   1.375 - 1.45
!----    C=C = 1.337 (Ethylene)  1.30  - 1.375
!----    C*C = 1.212 (Acetylene) 1.20  - 1.30
      DATA BONDTYPE /'H-H single',   'C-H single',   'N-H single',
     &               'O-H single',   'C-C single',   'C^C aromat',
     &               'C=C double',   'C*C triple',   'C-N single',
     &               'C=N double',   'C*N triple',   'C-O single',
     &               'C=O double',   'C-F single',   'C-Cl single',
     &               'C-I single',   'F-H single'/
      DATA VALENTYP /1.0D0, 1.0D0, 1.0D0, 1.0D0, 1.0D0,
     &               1.5D0, 2.0D0, 3.0D0, 1.0D0, 2.0D0,
     &               3.0D0, 1.0D0, 2.0D0, 1.0D0, 1.0D0,
     &               1.0D0, 1.0D0/
      END



      SUBROUTINE MEMORY (ISIZE)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      DIMENSION DYNAMIC (ISIZE)
      RETURN
      END



      INTEGER*4 FUNCTION ICHECK (VAL,TOL)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)

      ICHECK = 0
      IF (ABS (VAL) .GT. TOL) ICHECK = 1

      RETURN
      END


