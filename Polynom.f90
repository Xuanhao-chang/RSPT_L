!-----------------------------------------------------------------------
!     Subroutine: 'COPYPOL'   FIRST: 22 Dec 2009  LAST EDIT: 11 Feb 2020
!
!     PURPOSE:
!     Copy [NTER] terms of the operator polynomial from one file to
!     another.
!
!     CALLED: MAIN (ANCO.for)
!
!     INPUT:
!     NQ     -- The number of vibrational degrees of freedom;
!     NTER   --
!     FILE_SRC --
!     FILE_DST --
!
!     NOTE(S):
!-----------------------------------------------------------------------
      SUBROUTINE COPYPOL (NQ,NTER,FILE_SRC,FILE_DST)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  LOUT,NLOG,NINP,NOUT,NAUX,NSCR
      CHARACTER*80 FILE_SRC,FILE_DST
      INTEGER*1 IPOW(NQ,2)
      INTEGER*4 Q
      DATA NOPER /2/

      IF (NTER .EQ. 0) THEN
         IF (LOUT .GT. 1) WRITE (NOUT,1000)
         RETURN
      ENDIF

      IF (LOUT .GT. 1) WRITE (NOUT,1100) NTER,FILE_SRC,FILE_DST

      OPEN (UNIT = NAUX, FILE = FILE_SRC,       &
     &   ACTION = 'READ',  FORM = 'FORMATTED')
      OPEN (UNIT = NSCR, FILE = FILE_DST, DISPOSE = 'SAVE',      &
     &   ACTION = 'WRITE', FORM = 'UNFORMATTED', BUFFERED = 'YES')

      DO 100 I = 1, NTER
         READ  (NAUX,110) N, COEF,((IPOW(Q,J),J=1,NOPER),Q=1,NQ)
         WRITE (NSCR)        COEF,((IPOW(Q,J),J=1,NOPER),Q=1,NQ)
  100 CONTINUE

      CLOSE (NAUX)
      CLOSE (NSCR)

  110 FORMAT(I4,1X,F32.16,20(1XI1))  
      
      RETURN
 1000 FORMAT (/'[COPYPOL] Operator Polynomial Has Zero Length.')
 1100 FORMAT (/'Copy Polynomial from One File to Another ', &
     &   '(The Number of Terms = ',I8,'):'/                 &
     &   'Source      File: ',A64/'Destination File: ',A64)
    END

!-----------------------------------------------------------------------
!     Subroutine: 'ADDPOLF'   FIRST: 01 Apr 2009  LAST EDIT: 06 Jan 2018
!
!     PURPOSE:
!     Evaluate a sum of two Operators Polynomials, save result in file.
!     Resulting file may override any of the input files.
!
!     ALGORITHM:
!     The memory for total numbers of terms for OP1 + OP2 is allocated,
!     all terms of OP1 are copied there, followed by terms of OP2.
!     Then terms are collected in memory using COLLECT ().
!     NTERS may coincide with NTER1 or NTER2.
!
!     CALLED: MAINCVPT ();
!
!     CALLS: COLLECT (), COLLECT3 ();
!
!     INPUT:
!     NQ     -- The number of vibrational degrees of freedom;
!     NTER1  -- The number of terms in the first operator polynomial;
!     FILE1  -- File name for the first operator polynomial;
!     FACT1  -- Common coefficient applied to the first polynomial;
!     NTER2  -- The number of terms in the second operator polynomial;
!     FILE2  -- File name for the second operator polynomial;
!     FACT2  -- Common coefficient applied to the second polynomial;
!     FILE_SUM: CHARACTER*80 file name for the resulting operator;
!               Resulting file may override any of input files.
!     TOL    -- Tolerance parameter determining the accuracy of
!               calculations. Terms with |COEF| < TOL are ignored.
!
!     OUTPUT:
!     NTERS  -- The number of terms in the resulting operator polynom.
!
!     FILE I/O:
!     Result of the sum evaluation is saved in unformatted file FILE12
!     in unformatted mode, LUN = NAUX.
!
!     PRINTS:
!     Single line announcement line and file name of the saved file.
!
!     NOTE(S):
!     1. 04/07/2011 -- Routine COLLECT4 () is added as an option.
!-----------------------------------------------------------------------
      SUBROUTINE ADDPOLF (NQ,NTERS,FILE_SUM,TOL,NTER1,FILE1,              &
     &   FACT1,NTER2,FILE2,FACT2)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  LOUT,NLOG,NINP,NOUT,NAUX,NSCR
      CHARACTER*64    PATH_CUR,PATH_PRO,PATH_OUT,PATH_BIN
      COMMON /PATHS/  PATH_CUR,PATH_PRO,PATH_OUT,PATH_BIN
      INTEGER*1 IPOW(NQ,2)
      DIMENSION NPOWI(0:12)  !  Suffix I -- Initial # of terms per power
      CHARACTER*80 FILE1,FILE2,FILE_RAW,FILE_SUM
      INTEGER*4 A,B,Q,S
      DATA A,B /1,2/

      IF (LOUT .GT. 1) THEN
         WRITE (NOUT,1000)
         WRITE (NOUT,1010) NTER1,FACT1,FILE1,NTER2,FACT2,FILE2
         WRITE (NOUT,1020) FILE_SUM
      ENDIF

      FILE_RAW = TRIM(PATH_BIN)// 'ADDPOL.BIN'
      OPEN (UNIT = NAUX, FILE = FILE_RAW, DISPOSE = 'SAVE', &
     &   BUFFERED = 'YES', FORM = 'UNFORMATTED')
!
!-----------------------------------------------------------------------
!     Determine total and individual powers of the operator product
!-----------------------------------------------------------------------
      MAXTOT = 0  !  Maximum total power in the whole operator product
      MAXPOW = 0  !  Maximum power for an individual operator
      NPOWI  = 0  !  Vector counting the number of terms per SUM(powers)

      IF (NTER1 .GT. 0) THEN
         OPEN (UNIT = NSCR, FILE = FILE1, ACTION = 'READ', &
     &      STATUS = 'OLD', BUFFERED = 'YES', FORM = 'UNFORMATTED')

         DO 110 I = 1, NTER1
            READ  (NSCR) COEF,(IPOW(Q,A),IPOW(Q,B),Q=1,NQ)
            COEF = FACT1 * COEF
            WRITE (NAUX) COEF,(IPOW(Q,A),IPOW(Q,B),Q=1,NQ)

!----          Evaluate total/individual powers of the operator product
            S = 0
            DO 100 Q = 1, NQ
               S = S + IPOW(Q,A) + IPOW(Q,B)
               MAXPOW = MAX (MAXPOW,IPOW(Q,A))
               MAXPOW = MAX (MAXPOW,IPOW(Q,B))
  100       CONTINUE
            MAXTOT = MAX (MAXTOT,S)
            NPOWI(S) = NPOWI(S) + 1
  110    CONTINUE

         CLOSE (NSCR)
      ENDIF

      IF (NTER2 .GT. 0) THEN
         OPEN (UNIT = NSCR, FILE = FILE2, ACTION = 'READ', &
     &      STATUS = 'OLD')                                                                 !, BUFFERED = 'YES', FORM = 'UNFORMATTED')

         DO 210 I = 1, NTER2
            READ  (NSCR,"(I4,1X,F32.16,20(1XI1))") N, COEF,(IPOW(Q,A),IPOW(Q,B),Q=1,NQ)
            COEF = FACT2 * COEF
            WRITE (NAUX) COEF,(IPOW(Q,A),IPOW(Q,B),Q=1,NQ)

!----          Evaluate total/individual powers of the operator product
            S = 0
            DO 200 Q = 1, NQ
               S = S + IPOW(Q,A) + IPOW(Q,B)
               MAXPOW = MAX (MAXPOW,IPOW(Q,A))
               MAXPOW = MAX (MAXPOW,IPOW(Q,B))
  200       CONTINUE
            MAXTOT = MAX (MAXTOT,S)
            NPOWI(S) = NPOWI(S) + 1
  210    CONTINUE

         CLOSE (NSCR)
      ENDIF
      CLOSE (NAUX)

!----    Trick: NTERS can coincide with NTER1/NTER2,
!----    must avoid updating NTERS at this stage

      NTOTAL = NTER1 + NTER2

!----    Disable removal of terms with MAXPOW, 'ICUT' = 1
      IF (NTERS .LE. 10000) THEN
         CALL COLLECT  (NQ,NTOTAL,FILE_RAW,FILE_SUM,TOL,IERR,1,0)
      ELSE
         CALL COLLECT4 (NQ,NTOTAL,FILE_RAW,FILE_SUM,TOL, &
     &      MAXTOT,MAXPOW,NPOWI,IERR,1,0)
      ENDIF

!----    Now can possibly override NTER1/NTER2
      NTERS = NTOTAL

      COEF = 100.0D0 * (1.0D0 - REAL (NTOTAL) / REAL (NTER1 + NTER2))
      IF (LOUT .GT. 1) WRITE (NOUT,1030) NTERS,COEF

      RETURN
 1000 FORMAT (/'[ADDPOLF] -- Summation of Two Operator Polynomials ', &
     &   'Stored in Files:')
 1010 FORMAT ('Operand 1 (',I9,' terms): ',F8.4,2X,A64/ &
     &   'Operand 2 (',I9,' terms): ',F8.4,2X,A64)
 1020 FORMAT ('Result of Summation is Here: ',A64)
 1030 FORMAT ('Final number of terms:',I9, &
     &   ',  Compression factor =',F6.1,'%')
    END

    
!-----------------------------------------------------------------------
!     Subroutine: 'COLLECT'   FIRST: 23 Jan 2010  LAST EDIT: 12 Feb 2013
!
!     PURPOSE:
!     Collect terms of an operator polynomial with identical operators
!     with removal of terms with zero coefficients. Save result in file.
!
!     FEATURES:
!     1. SINGLE PASS
!        This version is using single-pass algorithm with reallocations
!     2. DOUBLE ENCODING
!        Each operator is represented using a pair (CODE1,CODE2) of
!        long integer numbers INT*8 for fast comparison.
!     3. QUICK SORT
!        Resulting operator is sorted by coefficients using RQSORT ().
!     4. MINIMIZED INPUT
!        No additonal information about polynomial (MAXTOT, NPOWINI)
!        is required on entry to the routine.
!
!     THEORY:
!     It was empirically found, that for collecting terms appeared from
!     commutators, terms with total power = MAXTOT cancel each other.
!
!     ALGORITHM:
!     Operator polynomial to be simplified is kept in a file LUN = NAUX.
!     Each read term is encoded into pair (CODE1,CODE2).
!
!     The algorithm of decoding:
!     Array A[i1,i2,..iM], Each dimension = K, Order = M, Size = K^M.
!
!     Inverse decoding (with special case mod()=0):
!     First index is slowest, last index is fastest.
!
!     L0 = K^(M-1)*(i1-1) + K^(M-2)*(i2-1) + .. + iM.
!     i1 = (L0 - mod(L0,K^(M-1))) / K^(M-1) + 1, mod() > 0, or
!     i1 = L0 / K^(M-1), mod() = 0;
!     L1 = L0 - K^(M-1) * (i1-1);
!     in = (L(n-1) - mod(L(n-1),K^(M-n))) / K^(M-n) + 1, mod() > 0, or
!     in = L(n-1) / K^(M-n), mod() = 0;
!     Ln = L(n-1) - K^(M-n) * (in-1);
!     iM = (L(n-1) - mod(L(n-1),K^(M-n))) / K^(M-n) + 1, mod() > 0, or
!     iM = L(n-1) / K^(M-n), mod() = 0.
!
!     REFERENCE(S):
!     [1] See My Journal 15/01/2010, page 25.
!
!     CALLED: COMMUTE (Sibert.for); H_VIBROT (Commute.for).
!
!     INPUT:
!     NTER   -- The number of primitive terms in operator polynomial;
!     NQ     -- The number of vibrational degrees of freedom;
!     FILE_IN-- CHARACTER*80 file name for the original operator;
!     FILE_SH-- CHARACTER*80 file name for the resulting operator;
!               These filenames may coincide.
!     TOL    -- Tolerance parameter determining the accuracy of
!               calculations. Terms with |COEF| < TOL are ignored;
!     ICUT   -- =1, collect all terms; =0, ignore total power = MAXTOT;
!     IOUT   -- =1, Request for output.
!
!     OUTPUT:
!     NTER   -- The number of primitive terms in Operator Polynomial
!               after Simplification (summation of equivalent terms);
!     <FILE_SH> This File contains the resulting operator.
!
!     NOTE(S):
!     1. 12/03/2010: Adding 'BUFFERED' = YES for OPEN () statements
!        dramatically improved performance.
!     2. Plan: Write terms with equal total powers into separate files
!        and then optimize them individually for saving memory.
!-----------------------------------------------------------------------
      SUBROUTINE COLLECT (NQ,NTER,FILE_IN,FILE_SH,TOL,IERR,ICUT,IOUT)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  LOUT,NLOG,NINP,NOUT,NAUX,NSCR
      COMMON /NUMBER/ PI,RADIAN,ZERO,HALF,ONE,TWO,THREE,FOUR,TEN(-16:16)
      COMMON /PATHS/  PATH_EXE,PATH_PRO,PATH_OUT,PATH_BIN
      CHARACTER*64 PATH_EXE,PATH_PRO,PATH_OUT,PATH_BIN
      CHARACTER*80 FILE_IN,FILE_SH

      CHARACTER*80 FILE_BUF,FILE_SRT
      LOGICAL FILESTAT
      INTEGER*1 IOPER(NQ,2)
      DIMENSION INDEX(NQ,2)
      DIMENSION NPOWI(0:12),NPOWF(0:12),NBASE(0:12)
      DIMENSION MEMBLK(0:12),LENBUF(0:12)
      DIMENSION MAXMEM(0:12),NALLOC(0:12)
      LOGICAL TAIL(0:12)
      ALLOCATABLE ARRAY(:),INDX(:)

      TYPE :: PTR_TERM
         REAL*16,    DIMENSION(:), POINTER :: COEF
         INTEGER*8, DIMENSION(:), POINTER :: CODE1,CODE2
      ENDTYPE PTR_TERM
      TYPE (PTR_TERM), ALLOCATABLE :: PTROPER(:)

      INTEGER*4 A,B,Q
      INTEGER*8 CODE1,CODE2,KK,LL,MODLK
      INTEGER*8 MAXVAL_8
      LOGICAL OVERFLOW
!-----------------------------------------------------------------------
      DATA A,B /1,2/

      IF (IOUT .GT. 0) THEN
         CALL WATCH (ISPLIT)
         WRITE (NOUT,1000)
      ENDIF
!
!-----------------------------------------------------------------------
!     Scan polynomial and find out:
!     1) Maximum total power of operators, MAXTOT;
!     2) The numbers of terms for each power, NPOWI(0..MAXTOT)
!-----------------------------------------------------------------------
      INQUIRE (UNIT = NAUX, OPENED = FILESTAT)
      IF (FILESTAT) STOP '[COLLECT] LUN = NAUX is engaged.'
      OPEN (UNIT = NAUX, FILE = FILE_IN, STATUS = 'OLD', &
     &   DISPOSE = 'SAVE', FORM = 'UNFORMATTED')

      MAXTOT = 0
      MAXPOW = 0
      NPOWI  = 0  !  Vector assignment

      DO 110 I = 1, NTER
         READ (NAUX, IOSTAT = IERR, ERR = 930, END = 930) &
     &      COPER,(IOPER(Q,A),IOPER(Q,B),Q=1,NQ)
         IF (IERR .NE. 0) THEN
            WRITE (*,"(/'Record Number = ',I12)") I
            WRITE (*,"(/'Error during READ, IOSTAT = ',I8)") IERR
            GO TO 930
         ENDIF
         IF (ABS (COPER) .LT. TOL) CYCLE

         M = 0
         DO 100 Q = 1, NQ
            M = M + IOPER(Q,A) + IOPER(Q,B)
            MAXPOW = MAX (MAXPOW,IOPER(Q,A))
            MAXPOW = MAX (MAXPOW,IOPER(Q,B))
  100    CONTINUE
         MAXTOT = MAX (MAXTOT,M)
         NPOWI(M) = NPOWI(M) + 1
  110 CONTINUE

      REWIND (NAUX)

!----    K^NQ < MAX = 2^63; NQ*LOG(K) < 63*LOG(2); NQ < 63*LOG(2)/LOG(K)
      IF (IOUT .GT. 0) THEN
         K = MAXTOT + 1
         MAXNQ = INT (63.0D0 * LOG (2.0D0) / LOG (REAL (K)))
         WRITE (NOUT,1010) NTER,MAXTOT,MAXNQ
         IF (ICUT .EQ. 0) THEN
            K = MAXTOT - 1
            MAXNQ = INT (63.0D0 * LOG (2.0D0) / LOG (REAL (K)))
            WRITE (NOUT,1020) MAXNQ
         ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!     Check for overflow and call COLLECT4 if necessary.
!     NOTE: The input file will be deleted inside COLLECT4
!-----------------------------------------------------------------------
      MAXVAL_8 = 2;  MAXVAL_8 = -((-MAXVAL_8) ** 63 + 1)
      MAXVAL_4 = 2;  MAXVAL_4 = -((-MAXVAL_4) ** 31 + 1)

      OVERFLOW = REAL (MAXTOT + 1) ** NQ .GT. REAL (MAXVAL_8)
      IF (OVERFLOW) THEN
         WRITE (NOUT,2100) NQ
         CLOSE (NAUX)
         CALL COLLECT4 (NQ,NTER,FILE_IN,FILE_SH,TOL, &
     &      MAXTOT,MAXPOW,NPOWI,IERR,ICUT,IOUT)
         GO TO 900
      ENDIF
 2100 FORMAT (/'COLLECT: Unable to handle NQ =',I3,', call COLLECT4.')
!
!-----------------------------------------------------------------------
!     Print summary table for all total powers M = 0..MAXTOT.
!     Find out how many cases are possible for total power = M
!-----------------------------------------------------------------------
      IF (IOUT .GT. 0) THEN
         WRITE (NOUT,1100)
         DO 120 M = 0, MAXTOT
            NBASE(M) = M + 1       !  0, 1, 2, .. M = M + 1 cases

            OVERFLOW = REAL (NBASE(M)) ** NQ .GT. REAL (MAXVAL_8)
            IF (.NOT. OVERFLOW) THEN
               KK = NBASE(M)       !  Convert to INTEGER*8
               KK = KK ** NQ       !  Max possible number of operators
            ELSE
               KK = -1
            ENDIF

            PART = 100.0D0 * REAL (NPOWI(M)) / REAL (NTER)
            WRITE (NOUT,1110) M,NBASE(M),NPOWI(M),PART,KK

            IF (ICUT .EQ. 0 .AND. M .EQ. MAXTOT) EXIT
            IF (OVERFLOW .AND. NPOWI(M) .GT. 0) GO TO 910
  120    CONTINUE
         WRITE (NOUT,1120)

         WRITE (NOUT,1130) REAL (MAXVAL_8),MAXVAL_8
         WRITE (NOUT,1140) REAL (MAXVAL_4),MAXVAL_4
      ENDIF
!
!-----------------------------------------------------------------------
!     Allocate memory for dynamic arrays holding pairs (COEF,OFFSET)
!-----------------------------------------------------------------------
      ALLOCATE (PTROPER(0:MAXTOT))

      MEMBLK = 0
      DO 150 M = 0, MAXTOT
         IF (NPOWI(M) .EQ. 0) CYCLE

         MEMBLK(M) = 1000  !  Initial size of the memory block
         MAXMEM(M) = MEMBLK(M)

         ALLOCATE (PTROPER(M)%COEF (MEMBLK(M)), STAT = IERR)
         IF (IERR .NE. 0) GO TO 920
         ALLOCATE (PTROPER(M)%CODE1(MEMBLK(M)), STAT = IERR)
         IF (IERR .NE. 0) GO TO 920
         ALLOCATE (PTROPER(M)%CODE2(MEMBLK(M)), STAT = IERR)
         IF (IERR .NE. 0) GO TO 920

         PTROPER(M)%COEF  = 0.0D0
         PTROPER(M)%CODE1 = 0
         PTROPER(M)%CODE2 = 0
  150 CONTINUE
!
!-----------------------------------------------------------------------
!     Main Loop over terms of polynomial
!-----------------------------------------------------------------------
      LENBUF = 0     !  The size of buffers for each total power
      MEMEXT = 1000  !  Memory block extension size added on overflow
      NALLOC = 0     !  The number of memory reallocations
      TAIL = .FALSE. !  Set the flag for treatment of the trailing zeros

      INQUIRE (UNIT = NSCR, OPENED = FILESTAT)
      IF (FILESTAT) STOP '[COLLECT] LUN = NSCR ENGAGED.'
      FILE_BUF = PATH_BIN(1:LENSTR(PATH_BIN,64)) // 'REALLOC.BIN'

      DO 600 II = 1, NTER
         READ (NAUX) COPER,(IOPER(Q,A),IOPER(Q,B),Q=1,NQ)
         IF (ABS (COPER) .LT. TOL) CYCLE
!
!-----------------------------------------------------------------------
!        Calculate total power of operators in term = M.
!        Encode operator into set [POWER = M].(COEF,CODE1,CODE2):
!-----------------------------------------------------------------------
         M = 0
         DO 200 Q = 1, NQ
            M = M + IOPER(Q,A) + IOPER(Q,B)
!----       INDEX(Q,A) = IOPER(Q,A) + 1
!----       INDEX(Q,B) = IOPER(Q,B) + 1
  200    CONTINUE

         IF (ICUT .EQ. 0 .AND. M .EQ. MAXTOT) CYCLE

         KK = M + 1  !  INTEGER*8 KK;  KK = NBASE(M) = M + 1
         KK = KK ** NQ
         CODE1 = 1
         CODE2 = 1
         DO 250 Q = 1, NQ
            KK = KK / (M + 1)  !  KK = NBASE(M) ** (NQ - Q)
!----       CODE1 = CODE1 + KK * (INDEX(Q,A) - 1)
!----       CODE2 = CODE2 + KK * (INDEX(Q,B) - 1)
            CODE1 = CODE1 + KK * IOPER(Q,A)
            CODE2 = CODE2 + KK * IOPER(Q,B)
  250    CONTINUE
!
!-----------------------------------------------------------------------
!        Check if the new operator already exists in the buffer
!-----------------------------------------------------------------------
         IF (LENBUF(M) .GT. 0) THEN

!----          LENBUF(M) may shrink afterwards if trailing zeros occur

            LBUF = LENBUF(M)
            DO 320 I = LENBUF(M), 1, -1
!----             Check if inspection of trailing zeros is enabled
               IF (TAIL(M)) THEN
                  IF (ABS (PTROPER(M)%COEF(I)) .LT. TOL) THEN
                     LBUF = I - 1       !  Cut trailing zeros
                     CYCLE
                  ELSE
                     TAIL(M) = .FALSE.  !  Disable further check
                  ENDIF
               ENDIF

               IF (CODE1 .EQ. PTROPER(M)%CODE1(I) .AND. &
     &             CODE2 .EQ. PTROPER(M)%CODE2(I)) THEN

                  PTROPER(M)%COEF(I) = PTROPER(M)%COEF(I) + COPER
                  IF (I .EQ. LENBUF(M)) TAIL(M) = .TRUE.
                  GO TO 600  !  Global DO-loop over terms

               ENDIF
  320       CONTINUE
            LENBUF(M) = LBUF
            TAIL(M) = .FALSE.
         ENDIF
!
!-----------------------------------------------------------------------
!        Current operator was never encountered, create the new entry
!-----------------------------------------------------------------------
         LENBUF(M) = LENBUF(M) + 1
         I = LENBUF(M)
         PTROPER(M)%COEF(I)  = COPER
         PTROPER(M)%CODE1(I) = CODE1
         PTROPER(M)%CODE2(I) = CODE2
         TAIL(M) = .FALSE.
!
!-----------------------------------------------------------------------
!        Check if the number of terms reached the size of current block.
!        Then save it to the file, allocate extended size, upload to RAM
!-----------------------------------------------------------------------
         IF (LENBUF(M) .EQ. MEMBLK(M)) THEN
            OPEN (UNIT = NSCR, FILE = FILE_BUF, DISPOSE = 'DELETE',&
     &         BUFFERED = 'YES', FORM = 'UNFORMATTED')

            L = 0
            DO 510 I = 1, LENBUF(M)
               IF (ABS (PTROPER(M)%COEF(I)) .LT. TOL) CYCLE
               L = L + 1
               WRITE (NSCR) PTROPER(M)%COEF(I), &
     &            PTROPER(M)%CODE1(I),PTROPER(M)%CODE2(I)
  510       CONTINUE
            LENBUF(M) = L

            REWIND (NSCR)
            DEALLOCATE (PTROPER(M)%COEF)
            DEALLOCATE (PTROPER(M)%CODE1,PTROPER(M)%CODE2)

            MEMBLK(M) = LENBUF(M) + MEMEXT
            MAXMEM(M) = MAX0 (MEMBLK(M),MAXMEM(M))
            ALLOCATE (PTROPER(M)%COEF (MEMBLK(M)), STAT = IERR)
            IF (IERR .NE. 0) GO TO 920
            ALLOCATE (PTROPER(M)%CODE1(MEMBLK(M)), STAT = IERR)
            IF (IERR .NE. 0) GO TO 920
            ALLOCATE (PTROPER(M)%CODE2(MEMBLK(M)), STAT = IERR)
            IF (IERR .NE. 0) GO TO 920

            DO 520 I = 1, LENBUF(M)
               READ (NSCR) PTROPER(M)%COEF(I), &
     &            PTROPER(M)%CODE1(I),PTROPER(M)%CODE2(I)
  520       CONTINUE

            CLOSE (NSCR)
            NALLOC(M) = NALLOC(M) + 1
         ENDIF
  600 CONTINUE
      CLOSE (NAUX)
!
!-----------------------------------------------------------------------
!     Arrange terms in descending order using quick-sort algorithm.
!-----------------------------------------------------------------------
      FILE_SRT = PATH_BIN(1:LENSTR(PATH_BIN,64)) // 'SORTING.BIN'
      OPEN (UNIT = NSCR, FILE = FILE_SRT, DISPOSE = 'DELETE', &
     &   BUFFERED = 'YES', FORM = 'UNFORMATTED')

      DO 640 M = MAXTOT, 0, -1
         LBUF = LENBUF(M)
         IF (LBUF .LT. 1) CYCLE

         ALLOCATE (ARRAY(LBUF), STAT = IERR)
         IF (IERR .NE. 0) GO TO 920
         ALLOCATE (INDX(LBUF),  STAT = IERR)
         IF (IERR .NE. 0) GO TO 920

         DO 610 I = 1, LBUF
            ARRAY(I) = ABS (PTROPER(M)%COEF(I))
  610    CONTINUE

         CALL RQSORT (LBUF,ARRAY,INDX)

!----       Also delete terms |COEF| < TOL and update LENBUF
         REWIND (NSCR)
         DO 620 I = LBUF, 1, -1
            K = INDX(I)
            IF (ABS (PTROPER(M)%COEF(K)) .LT. TOL) THEN
               LENBUF(M) = LBUF - I
               EXIT
            ENDIF
            WRITE (NSCR) PTROPER(M)%COEF(K), &
     &         PTROPER(M)%CODE1(K),PTROPER(M)%CODE2(K)
  620    CONTINUE

         REWIND (NSCR)
         DO 630 I = 1, LENBUF(M)
            READ (NSCR) PTROPER(M)%COEF(I), &
     &         PTROPER(M)%CODE1(I),PTROPER(M)%CODE2(I)
  630    CONTINUE

         DEALLOCATE (ARRAY,INDX)
  640 CONTINUE
      CLOSE (NSCR)
!
!-----------------------------------------------------------------------
!     Decode OFFSETs into operator powers: a^m b^n
!-----------------------------------------------------------------------
      OPEN (UNIT = NAUX, FILE = FILE_SH, DISPOSE = 'SAVE', &
     &   ACTION = 'WRITE', BUFFERED = 'YES', FORM = 'UNFORMATTED')

      ICOUNT = 0
      NPOWF  = 0
      CMAX   = ZERO
      DO 740 M = MAXTOT, 0, -1  !  Write in descending order of powers
         IF (NPOWI(M) .EQ. 0) CYCLE
         NBASE(M) = M + 1  !  Not needed, already assigned

         DO 730 I = 1, LENBUF(M)
            COPER = PTROPER(M)%COEF(I)

            IF (ABS (COPER) .LT. TOL) CYCLE
            CMAX = QMAX1 (CMAX, ABS (COPER))

            DO 720 L = 1, 2
               IF (L .EQ. 1) LL = PTROPER(M)%CODE1(I)
               IF (L .EQ. 2) LL = PTROPER(M)%CODE2(I)
               KK = M + 1  !  KK = NBASE(M)
               KK = KK ** NQ
               DO 710 Q = 1, NQ
                  IF (Q .LT. NQ) THEN
                     KK = KK / (M + 1)  !  = KK ** (NQ - Q)
                     MODLK = MOD(LL, KK)
                     IF (MODLK .EQ. 0) THEN
                        INDEX(Q,L) = LL / KK
                     ELSE
                        INDEX(Q,L) = (LL - MODLK) / KK + 1
                     ENDIF
                     LL = LL - KK * (INDEX(Q,L) - 1)
                  ELSE
                     INDEX(Q,L) = LL
                  ENDIF
                  IOPER(Q,L) = INDEX(Q,L) - 1
  710          CONTINUE
  720       CONTINUE

            WRITE (NAUX) COPER,(IOPER(Q,A),IOPER(Q,B),Q=1,NQ)
            ICOUNT   = ICOUNT + 1
            NPOWF(M) = NPOWF(M) + 1
  730    CONTINUE

  740 CONTINUE

      CLOSE (NAUX)

!----    Delete the original file if it is different from the output
!        NOTE: Collection of garbage is performed later in SIBERT ().
!     IF (FILE_IN .NE. FILE_SH) THEN
!        OPEN (UNIT = NAUX, FILE = FILE_IN, STATUS = 'OLD',
!    &      DISPOSE = 'DELETE', FORM = 'UNFORMATTED')
!        CLOSE (NAUX)
!     ENDIF

      NINIT = NTER
      NTER  = ICOUNT

      PART = ZERO
      IF (NINIT .GT. 0) PART = 100.0D0 * REAL (NTER) / REAL (NINIT)
      IF (IOUT  .GT. 0) WRITE (NOUT,1300) NINIT,NTER,PART,CMAX
!
!-----------------------------------------------------------------------
!     Release allocated memory
!-----------------------------------------------------------------------
      DO 800 M = 0, MAXTOT
         IF (NPOWI(M) .EQ. 0) CYCLE
         DEALLOCATE (PTROPER(M)%COEF)
         DEALLOCATE (PTROPER(M)%CODE1,PTROPER(M)%CODE2)
  800 CONTINUE
      DEALLOCATE (PTROPER)

      IF (IOUT .GT. 0) THEN
      WRITE (NOUT,1200)
      DO 810 M = 0, MAXTOT
         IF (NPOWI(M) .EQ. 0) CYCLE
         PART = 100.0D0 * (ONE - REAL (NPOWF(M)) / REAL (NPOWI(M)))
         WRITE (NOUT,1210) M,NPOWI(M),NPOWF(M),PART,NALLOC(M),MAXMEM(M)
  810 CONTINUE
      WRITE (NOUT,1220)

      CALL WATCH1 (ISPLIT,IThour,ITmin,ITsec,IT100th)
      WRITE (NOUT,2000) IThour,ITmin,ITsec,IT100th
 2000 FORMAT ('[COLLECT] Time Elapsed: ',I2,':',I2.2,':',I2.2,'.',I2.2)
      ENDIF

  900 CONTINUE  !  Successful return
      IERR = 0
      RETURN
  910 CONTINUE  !  Encoding overflow
      IERR = 1
      WRITE (*,*) 'ENCODING OVERFLOW'
      GO TO 990
  920 CONTINUE  !  Insufficient Memory
      IERR = 2
      WRITE (*,*) 'INSUFFICIENT MEMORY'
      NTER = 0
      GO TO 990
  930 CONTINUE  !  File I/O Error
      IERR = 3
      WRITE (*,*) 'READ ERROR / END OF FILE'
  990 STOP
      RETURN

 1000 FORMAT (/'[COLLECT]'/'Collect Terms of Operator Polynomial:')
 1010 FORMAT ('The Number of Terms in Polynomial =',I12/ &
     &   'Maximum Total Power of Operators in a Term = ',I2,     &
     &   ',  Max(NQ) =',I3)
 1020 FORMAT ('Option to ignore terms with max power is set up', &
     &   ',  Max(NQ) =',I3)

 1100 FORMAT (62('-')/'Power    Cases       Terms    Fraction',  &
     &   '  Max.Index = N(case)^NQ'/62('-'))
 1110 FORMAT (I4,I10,I12,F10.2,' %',I24)
 1120 FORMAT (62('-'))
 1130 FORMAT ('Maximum INTEGER*8 Value = ',ES12.4,I24)
 1140 FORMAT ('Maximum INTEGER*4 Value = ',ES12.4,I24)

 1200 FORMAT ('Summary Table after Collection of Terms:'/78('-')/&
     &   'Power       Original       Resulting   Reduction',     &
     &   '      Realloc.     Max. Buffer'/78('-'))
 1210 FORMAT (I4,I16,I16,F10.2,' %',I14,I16)
 1220 FORMAT (78('-'))

 1300 FORMAT ('Original Number of Terms Before Collecting = ',I8/& 
     &        'Resulting Number of Terms After Collecting = ',I8,&
     &   ' (',F5.1,'%)'/'Maximum Scalar Coefficient Found = ',ES18.8)

!9000 FORMAT ('Stage 3 of 4: Find out Maximum Power and Histogram ...'\)
!9100 FORMAT ('Stage 4 of 4: Collecting Terms with Same Operators ...'\)
END


!-----------------------------------------------------------------------
!     Subroutine: 'COLLECT4'  FIRST: 17 Jun 2011  LAST EDIT: 04 Apr 2018
!
!     PURPOSE:
!     Collect terms in long operator polynomials arising in commutators.
!     The terms with identical operators are collected with removal of
!     zero coefficients. The output operator is saved in file FILE_SH.
!
!     FEATURES (For more history, look at COLLECT2 in Commute(0).for):
!     1. Double encoding (CODE1,CODE2) is used for a/b vectors.
!        If overflow appears, longer encoding (CODE1-4) is possible.
!     2. Two passes are used, scanning terms first time with restricted
!        depth, second time doing full scan.
!     3. The polynomial is sorted to NQ parts, in each part 'Location N'
!        is the first one in the list of a(i)^n(i)*b(i)^m(i) terms, for
!        which the sum MAXS = n(i) + m(i) is maximum, and among them,
!        the power MAXT = n(i) is also maximum.
!     4. Version 3 is different from the Version 2 by the method of
!        working with terms, splitted into NQ Locations. In Version 2,
!        after the first pass, the optimized parts of the polynomial are
!        kept in memory. This is inefficient for the bigger molecules.
!        In this Version 3, each Location is preliminary saved in files
!        'COLLECTxx.BIN' and treated in a loop.
!     5. Working with the tail at the Stage 1 is optimized. After adding
!        a new term, the buffer is scanned from the end for zeros.
!     6. During Stage 2, when the term is summed with the previous one,
!        the coefficient is checked for zero. If it becomes zero, the
!        last term is moved to replace the hole and the LENGTH shrinks.
!     7. This Version is MPI-ready, as each file can be treated
!        separately.
!     8. Double encoding of long products of PRO[i]a(i)^m(i)*b(i)^n(i).
!        This feature is specific for COLLECT4.
!
!     DEVELOPMENT:
!     1. Secondary splitting of terms sorted into NQ groups is possible.
!     Among a group sorted on i-th Q, split terms according to the first
!     b(j)^m(j)-operator with maximum power m(j). These subgroups can be
!     kept in memory, look at COLLECT2 ().
!     2. The secondary optimization is possible. Need to print each
!     location and analyze the structure of operator a/b powers.
!     Must look for structure of powers of b(i)^m(i).
!     IDEA: The first operator b(i)^m(i) with maximum m(i).
!
!     THEORY:
!     1. Total powers in individual terms are all odd (0,2,4,6,8,...) or
!     all even (1,3,5,7,...), as empirically found.
!
!     2. It was empirically found, that terms with maximum total power
!     (MAXTOT) cancel each other. In this version the flag ICUT is used,
!     it enables or disables automatic removal of such terms.
!
!     3. Instead of comparing two pair of products,
!        Product1 = PRO[i=1,NQ] a(i)^n1(i) * b(i)^m1(i), and
!        Product2 = PRO[i=1,NQ] a(i)^n2(i) * b(i)^m2(i),
!     which would take longer time, each product is encoded in two long
!     integer number (8 bytes) and for comparison of two products it is
!     necessary to compare just pairs of integers.
!     Each product is naturally represented as two lists,
!        {n(1),n(2),..,n(NQ)} and {m(1),m(2),..,m(NQ)}
!     which are converted to long integer numbers INTEGER*8.
!
!     4. The algorithm of encoding:
!
!     MAXPOW = MAX (MAX (n(i)), MAX(m(i))),
!
!     K = MAXPOW + 1 is used as the base of the numerical system.
!
!     Each list of numbers {n(1),n(2),..,n(NQ)} and {m(1),m(2),..,m(NQ)}
!     is converted to an INT*8 positive integer number using:
!
!     Code1 = K^(NQ-1)*n(1) + K^(NQ-2)*n(2) + ... + n(NQ)
!     Code2 = K^(NQ-1)*m(1) + K^(NQ-2)*m(2) + ... + m(NQ)
!
!     Numbers Code1,2 will be converted back to lists at the end.
!
!     5. The algorithm of decoding:
!     Vector {n(1),n(2),...,n(M)} was encoded to LongN number using
!     K as the base of the numbering system, where K = MAX(n(i)) + 1.
!
!     Inverse decoding (with special treatment of case for mod() = 0):
!
! (0) L(0) = Code(1/2);
!
! (1) n(1) = (L(0) - mod(L(0),K^(M-1))) / K^(M-1);
!     L(1) = L(0) - K^(M-1) * n(1);
!
! (k) n(k) = (L(k-1) - mod(L(k-1),K^(M-k))) / K^(M-k);
!     L(k) = L(k-1) - K^(M-k) * n(k);
!
! (M) n(M) = (L(M-1) - mod(L(M-1),1));
!     n(M) = L(M-1) - n(k).
!
!     6. For simplicity, instead of using K = MAXPOW + 1 as the base of
!     the numerical system, K = MAXTOT + 1 is used, to guarantee that
!     an individual power of an operator will not be bigger than K.
!     Although, actual maximum power of an operator can be smaller.
!     Here MAXTOT is used instead if MAXPOW for compatibility.
!
!     7. It was empirically found out that:
!     (a) MAXPOW = MAXTOT - 1 or - 2;
!     (b) maximum power is equal to total power except the terms where
!         total power = MAXTOT.
!
!     8. In Version 3, the value MAXTOT is used for two purposes:
!     (a) automatic removal of terms with total power = MAXTOT;
!     (b) as the base for numbering system - for encoding.
!
!     9. Conclusion: the basis of numbering system NBASE must be equal
!     to the maximum power of an individual operator a(i)/b(i) for
!     those terms that do not vanish, i.e. to include powers MAXTOT - 2.
!     RESULT: The sole basis of the numbering system can be chosen as
!        NBASE = (MAXTOT - 2) + 1 = MAXTOT - 1.
!     In this case MAXTOT terms must be necessarily cut.
!
!     REFERENCE(S):
!     [1] See My Journal 15/01/2010, pages 25,68.
!
!     CALLED: COMMUTE (Sibert.for), ADDPOLF (ToolsCPT.for),
!             H_VIBROT (Commute.for).
!
!     INPUT:
!     NTER   -- The number of primitive terms in operator polynomial;
!     NQ     -- The number of vibrational degrees of freedom;
!     FILE_IN-- CHARACTER*80 file name for the original operator;
!     FILE_SH-- CHARACTER*80 file name for the resulting operator;
!     TOL    -- Tolerance parameter determining the accuracy of
!               calculations. Terms with |COEF| < TOL are ignored;
!     MAXTOT -- Maximum total power of all operators in an operator
!               product;
!     MAXPOW -- Maximum power of an individual operator in all products;
!     NPOWI  -- Array holding the number of terms for all total powers;
!     ICUT   -- =1, collect all terms; =0, ignore powers = MAXTOT;
!     IOUT   -- Flag for console output disable/enable 0/1.
!
!     OUTPUT:
!     NTER   -- The number of primitive terms in Operator Polynomial
!               after Simplification (summation of equivalent terms);
!     <FILE_IN> The file containing the original operator is deleted
!               if it does not coincide with the output file;
!     <FILE_SH> File containing the resulting operator is saved.
!
!     FILE I/O:
!     Logical Unit Numbers Used: NAUX, NSCR.
!
!     VARIABLES:
!     MEMBLK -- ...
!     LENBUF --
!     NALLOC --
!     MAXMEM --
!     ...
!
!     NOTE(S):
!     1. This is a modified version of COLLECT3.
!     2. Versions 2,3 of this routine is saved in Collect(0).for.
!     3. 27/08/2011 -- If input/output file names coincide, an error
!        occurs, the resulting file is deleted. Now file names are
!        compared, but deletion may be excluded and done by a separate
!        small routine if necessary. This routine is used in various
!        cases like V-R_Hamiltonian and commutators, where post-actions
!        are different.
!-----------------------------------------------------------------------
      SUBROUTINE COLLECT4 (NQ,NTER,FILE_IN,FILE_SH,TOL, &
     &   MAXTOT,MAXPOW,NPOWI,IERR,ICUT,IOUT)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  LOUT,NLOG,NINP,NOUT,NAUX,NSCR
      COMMON /NUMBER/ PI,RADIAN,ZERO,HALF,ONE,TWO,THREE,FOUR,TEN(-16:16)
      COMMON /PATHS/  PATH_EXE,PATH_PRO,PATH_OUT,PATH_BIN
      CHARACTER*64 PATH_EXE,PATH_PRO,PATH_OUT,PATH_BIN
      CHARACTER*80 FILE_IN,FILE_SH
      DIMENSION NPOWI(0:12)  !  Suffix I -- Initial # of terms per power

      CHARACTER*80 FILE_BUF,FILE(NQ),FILENAME
      DIMENSION MEMBLK(NQ),LENBUF(NQ),NALLOC(NQ),MAXMEM(NQ)
      DIMENSION MASK(NQ),NLOC(NQ),NLOC1(NQ),NLOC2(NQ)
      DIMENSION CBUF(100)
      INTEGER*1 IOPER(NQ,2),IBUF(100,NQ,2)
      INTEGER*1 jOPER(NQ,2)

      TYPE :: PTR_TERM
         REAL*16,    DIMENSION(:), POINTER :: COEF
         INTEGER*8, DIMENSION(:), POINTER :: CODE1,CODE2,CODE3,CODE4
      ENDTYPE PTR_TERM
      TYPE (PTR_TERM), ALLOCATABLE :: PTROPER(:)
      TYPE (PTR_TERM) PTROPER1

      INTEGER*4 A,B,Q,S
      INTEGER*8 MAXVAL_8,MAXNUM,KK,LL,MODLK,KSCALE1(NQ),KSCALE2(NQ)
      INTEGER*8 CODE1,CODE2,CODE3,CODE4
      INTEGER*8 LSPLIT,LSPLIT1,LSPLIT2
      CHARACTER*16 TIMETEXT
      LOGICAL OVERFLOW
      CHARACTER*12 NUMSTR  !  FUNCTION
!-----------------------------------------------------------------------
      DATA A,B /1,2/
      DATA LMAX /30/     !  The max number in operator terms to print
      DATA LBUF /100/    !  I/O Buffer for performance optimization
      DATA MDEPTH /200/  !  The depth of look-up list for Stage 1

      CALL LWATCH (LSPLIT)
      WRITE (NOUT,1000)
      IF (IOUT .GT. 0) WRITE (*,9000) NUMSTR(NTER)

 1000 FORMAT (/'[COLLECT4] -- Collect Terms of Operator Polynomial:')
!
!-----------------------------------------------------------------------
!     The value of maximum possible NQ is defined via: MAX(INT*8) = 2^63
!     K^NQ < MAX = 2^63; NQ*LOG(K) < 63*LOG(2); NQ < 63*LOG(2)/LOG(K).
!-----------------------------------------------------------------------
      WRITE (NOUT,1010) NTER

      IF (ICUT .EQ. 1) THEN
         NBASE = MAXTOT + 1
         WRITE (NOUT,1020)
      ELSE
         NBASE = MAXTOT - 1
! >>>    WRITE (NOUT,1025)
      ENDIF

      MAXNQ = INT (63.0D0 * LOG (2.0D0) / LOG (REAL (NBASE)))
      MAXVAL_4 = 2;  MAXVAL_4 = -((-MAXVAL_4) ** 31 + 1)
      MAXVAL_8 = 2;  MAXVAL_8 = -((-MAXVAL_8) ** 63 + 1)

      WRITE (NOUT,1030) MAXTOT,NQ,NBASE,MAXNQ
      WRITE (NOUT,1040) MAXVAL_4,REAL (MAXVAL_4)
      WRITE (NOUT,1050) MAXVAL_8,REAL (MAXVAL_8)

 1010 FORMAT (/'The Original Number of Terms in the Polynomial =',I14)
 1020 FORMAT ('All terms will be collected, including ones with the',   &
     &   'maximum total power.')
 1030 FORMAT (/'Maximum total power in operator products = ',I4/        &
     &         'The number of degrees of freedom         = ',I4/        &
     &         'The chosen base of numbering system      = ',I4/        &
     &         'Maximum possible degrees of freedom      = ',I4)
 1040 FORMAT (/'Maximum INTEGER*4 Value = ',I24,ES12.4)
 1050 FORMAT ( 'Maximum INTEGER*8 Value = ',I24,ES12.4)
!
!-----------------------------------------------------------------------
!     Split into two ranges: 1 ... NQ1, LQ2 ... NQ (in total = NQ2)
!-----------------------------------------------------------------------
      DMAXNUM1 = REAL (NBASE) ** NQ
      DMAXNUM2 = REAL (NBASE) ** ((NQ + 1) / 2)
      IF (DMAXNUM1 .LT. REAL (MAXVAL_8)) THEN
         LONG = 1
         NQ1  = NQ
         NQ2  = 0
      ELSE IF (DMAXNUM2 .LT. REAL (MAXVAL_8)) THEN
         LONG = 2
         NQ1  = (NQ + 1) / 2
         NQ2  = NQ - NQ1
         LQ1  = 1
         LQ2  = NQ1 + 1
         WRITE (NOUT,1060) NQ1,LQ2,NQ
      ELSE
         LONG = 0
         WRITE (NOUT,1070) DMAXNUM2
         GO TO 910
      ENDIF

!==== Test using smaller molecule:
!     LONG = 2
!     WRITE (NOUT,1060) NQ1,LQ2,NQ
!====

!----    NQ1 is used as a default value of NQ

      DMAXNUM1 = REAL (NBASE) ** NQ1  !  The value must be representable
      MAXNUM   = NBASE                !  INTEGER*8 MAXNUM
      MAXNUM   = MAXNUM ** NQ1        !  In two steps to avoid overflow

      WRITE (NOUT,1080) MAXNUM,DMAXNUM1

 1060 FORMAT ( 'These ranges of normal coordinates are compressed ', &
     &   'separately:'/                                              &
     &   'First  range:  1 to ',I2/'Second range: ',I2,' to ',I2)
 1070 FORMAT ('Maximum Value NBASE^((NQ + 1) / 2)  = ',ES12.4/       &
     &   'This combination CVPT order / NQ may not handled.')
 1080 FORMAT ('Max Value of N(base)^NQ = ',I24,ES12.4)
!
!-----------------------------------------------------------------------
!     Demonstration of maximum numbers needed for encoding
!-----------------------------------------------------------------------
      WRITE (NOUT,1210)
      DO 100 M = 0, MAXTOT
         MBASE = M + 1
         MAXNUM = MBASE;  MAXNUM = MAXNUM ** NQ1  !  To avoid overflow
         PART = 100.0D0 * REAL (NPOWI(M)) / REAL (NTER)
         WRITE (NOUT,1220) M,MBASE,NPOWI(M),PART,MAXNUM
  100 CONTINUE
      WRITE (NOUT,1230)

      WRITE (NOUT,1050) MAXVAL_8,REAL (MAXVAL_8)
!
!-----------------------------------------------------------------------
!     Allocate the memory for dynamic arrays holding pairs (COEF,OFFSET)
!-----------------------------------------------------------------------
      ALLOCATE (PTROPER(NQ), STAT = IERR)
      IF (IERR .NE. 0) GO TO 920

      DO 120 Q = 1, NQ
         MEMBLK(Q) = 1000  !  The size of the memory block

         ALLOCATE (PTROPER(Q)%COEF (MEMBLK(Q)), STAT = IERR)
         IF (IERR .NE. 0) GO TO 920
         ALLOCATE (PTROPER(Q)%CODE1(MEMBLK(Q)), STAT = IERR)
         IF (IERR .NE. 0) GO TO 920
         ALLOCATE (PTROPER(Q)%CODE2(MEMBLK(Q)), STAT = IERR)
         IF (IERR .NE. 0) GO TO 920
         PTROPER(Q)%COEF  = 0.0D0
         PTROPER(Q)%CODE1 = 0
         PTROPER(Q)%CODE2 = 0

         IF (LONG .EQ. 2) THEN
            ALLOCATE (PTROPER(Q)%CODE3(MEMBLK(Q)), STAT = IERR)
            IF (IERR .NE. 0) GO TO 920
            ALLOCATE (PTROPER(Q)%CODE4(MEMBLK(Q)), STAT = IERR)
            IF (IERR .NE. 0) GO TO 920
            PTROPER(Q)%CODE3 = 0
            PTROPER(Q)%CODE4 = 0
         ENDIF
  120 CONTINUE
!
!=======================================================================
! (1) Main Loop over terms of polynomial - First Pass: Encoding and Sort
!=======================================================================
      OPEN (UNIT = NAUX, FILE = FILE_IN, STATUS = 'OLD',    &
     &   ACTION = 'READ', FORM = 'UNFORMATTED')

      DO 150 Q = 1, NQ
         WRITE (FMT = "('COLLECT',I2.2,'.BIN')", UNIT = FILENAME) Q 
         FILE(Q) = TRIM(PATH_BIN)// TRIM(FILENAME)
         OPEN (UNIT = NSCR, FILE = FILE(Q), DISPOSE = 'SAVE',           &
     &      STATUS = 'REPLACE', FORM = 'UNFORMATTED', BUFFERED = 'YES')
         CLOSE (NSCR)
  150 CONTINUE

      LENBUF = 0   !  The size of buffers for each LOCATOR
      NLOC   = 0   !  The initial number of terms per location
      NLOC1  = 0   !  The number of terms per location after Stage 1
      ICOUNT = 0   !  The counter for terms during the first pass
      LCOUNT = 0   !  The counter for terms excluding max powers
      MAXCUT = 0   !  Statistics: efficiency of cutting tail of 0's

!----    Prepare powers of NBASE for faster encoding

      KK = NBASE;  KK = KK ** NQ1      !  Use INT*8 To avoid overflow
      DO 160 Q = 1, NQ1
         KK = KK / NBASE               !  KK = NBASE ** (NQ1 - Q)
         KSCALE1(Q) = KK
  160 CONTINUE

      IF (LONG .EQ. 2) THEN
      KK = NBASE;  KK = KK ** NQ2      !  Use INT*8 To avoid overflow
      DO 170 Q = 1, NQ2
         KK = KK / NBASE               !  KK = NBASE ** (NQ2 - Q)
         KSCALE2(Q) = KK
  170 CONTINUE
      ENDIF

      CALL WATCH  (ISPLIT)
      CALL LWATCH (LSPLIT1)
      ICENT0 = 0
      ICENT1 = 0
      IF (IOUT .GT. 0) THEN
         WRITE (*,9100)
         CALL COUNTER (0,0,1)
      ENDIF
!
!=======================================================================
!     Print Message:
!     The file with max size NTER*(REAL*16,2*INT*8) will be created,
!     HDD space estimation: XGbytes
!=======================================================================
      DO 400 II = 1, NTER
         IF (IOUT .GT. 0) THEN
            IF (NTER .GT. 1000) THEN
               ICENT1 = II / (NTER / 100)
            ELSE
               ICENT1 = 100 * II / NTER
            ENDIF
            IF (ICENT1 .NE. ICENT0) THEN
               CALL COUNTER (1,ICENT1,1)
               ICENT0 = ICENT1
            ENDIF
         ENDIF
!
!-----------------------------------------------------------------------
!        Read block of operator polynomial for performance optimization
!        NOTE: READ (..., I=1,NSIZE) does not work, need a solution
!-----------------------------------------------------------------------
         IF (MOD (II,LBUF) .EQ. 1) THEN
            NSIZE = MIN0 (II + LBUF - 1, NTER) - II + 1
            DO 200 I = 1, NSIZE
               READ (NAUX, ERR = 930, END = 940)            &
     &            CBUF(I),(IBUF(I,Q,A),IBUF(I,Q,B),Q=1,NQ)
  200       CONTINUE
         ENDIF

         K = MOD (II - 1, LBUF) + 1
         COPER = CBUF(K)
         DO 210 Q = 1, NQ
            IOPER(Q,A) = IBUF(K,Q,A)
            IOPER(Q,B) = IBUF(K,Q,B)
  210    CONTINUE

!        READ (NAUX, ERR = 930, END = 930)
!    &      COPER,(IOPER(Q,A),IOPER(Q,B),Q=1,NQ)
!
!-----------------------------------------------------------------------
!        Calculate total power of operators in term = M.
!        Encode operator into set (COEF,CODE1,CODE2):
!-----------------------------------------------------------------------
         IF (ABS (COPER) .LT. TOL) CYCLE

         S = 0
         DO 220 Q = 1, NQ
            S = S + IOPER(Q,A) + IOPER(Q,B)
  220    CONTINUE

!----       Skip the term if S = MAXTOT and enabled by flag ICUT

         IF (ICUT .EQ. 0 .AND. S .EQ. MAXTOT) CYCLE

         LCOUNT = LCOUNT + 1
!
!-----------------------------------------------------------------------
!        MASK -- Sum of powers (n+m) of a(i)^n(i) + b(i)^m(i);
!        MAXS -- Maximum sum of powers (n+m) of a(i)^n(i) + b(i)^m(i);
!        MAXT -- Maximum power of a(i)^n(i);
!-----------------------------------------------------------------------
         MAXS = 0
         DO 230 Q = 1, NQ
            MASK(Q) = IOPER(Q,A) + IOPER(Q,B)
            MAXS = MAX (MASK(Q), MAXS)
  230    CONTINUE

         MAXT = 0
         DO 240 Q = 1, NQ
            IF (MASK(Q) .LT. MAXS) CYCLE
            MAXT = MAX (MAXT, IOPER(Q,A))
  240    CONTINUE

         DO 250 Q = 1, NQ
            IF (MASK(Q) .EQ. MAXS .AND. IOPER(Q,A) .EQ. MAXT) THEN
               LOCATOR = Q
               EXIT
            ENDIF
  250    CONTINUE

         L = LOCATOR
         NLOC(L) = NLOC(L) + 1
!
!-----------------------------------------------------------------------
!     The algorithm of encoding (copy of the above):
!
!     MAXPOW = MAX (MAX (n(i)), MAX(m(i))),
!
!     K = MAXPOW + 1 is used as the base of the numerical system.
!
!     Each list of numbers {n(1),n(2),..,n(NQ)} and {m(1),m(2),..,m(NQ)}
!     is converted to an INT*8 positive integer number using:
!
!     Code1 = K^(NQ-1)*n(1) + K^(NQ-2)*n(2) + ... + n(NQ)
!     Code2 = K^(NQ-1)*m(1) + K^(NQ-2)*m(2) + ... + m(NQ)
!-----------------------------------------------------------------------
         CODE1 = 0
         CODE2 = 0
         DO 260 Q = 1, NQ1
            CODE1 = CODE1 + KSCALE1(Q) * IOPER(Q,A)
            CODE2 = CODE2 + KSCALE1(Q) * IOPER(Q,B)
  260    CONTINUE

         IF (LONG .EQ. 2) THEN
         CODE3 = 0
         CODE4 = 0
         DO 270 Q = LQ2, NQ
            M = Q - LQ2 + 1
            CODE3 = CODE3 + KSCALE2(M) * IOPER(Q,A)
            CODE4 = CODE4 + KSCALE2(M) * IOPER(Q,B)
  270    CONTINUE
         ENDIF
!
!-----------------------------------------------------------------------
!  (1/3) Check if the operator product term already exists in the buffer
!        NOTE: During first pass scan only recent entries, MDEPTH = 200.
!-----------------------------------------------------------------------
         IF (LENBUF(L) .GT. 0) THEN

            DO 290 I = MAX0 (LENBUF(L) - MDEPTH, 1), LENBUF(L)
               IF (CODE1 .NE. PTROPER(L)%CODE1(I)) CYCLE
               IF (CODE2 .NE. PTROPER(L)%CODE2(I)) CYCLE
               IF (LONG .EQ. 2) THEN
               IF (CODE3 .NE. PTROPER(L)%CODE3(I)) CYCLE
               IF (CODE4 .NE. PTROPER(L)%CODE4(I)) CYCLE
               ENDIF

!----             Identical term found, sum coefficients
               PTROPER(L)%COEF(I) = PTROPER(L)%COEF(I) + COPER

!----             Remove the trailing zeros, if found
               LENGTH = LENBUF(L)  !  Flexible length of the buffer
               DO 280 K = LENBUF(L), MAX0 (LENBUF(L) - MDEPTH, 1), -1
                  IF (ABS (PTROPER(L)%COEF(K)) .GT. TOL) EXIT
                  LENGTH = K - 1   !  Cut trailing zeros
  280          CONTINUE

!----             Update the buffer's size after removal of trailing 0's
               MAXCUT = MAX (MAXCUT, LENBUF(L) - LENGTH)
               LENBUF(L) = LENGTH

               GO TO 400  !  Global DO-loop over terms
  290       CONTINUE

         ENDIF
!
!-----------------------------------------------------------------------
!  (2/3) Current operator was never encountered, create the new entry
!-----------------------------------------------------------------------
         LENBUF(L) = LENBUF(L) + 1
         K = LENBUF(L)
         PTROPER(L)%COEF(K)  = COPER
         PTROPER(L)%CODE1(K) = CODE1
         PTROPER(L)%CODE2(K) = CODE2
         IF (LONG .EQ. 2) THEN
         PTROPER(L)%CODE3(K) = CODE3
         PTROPER(L)%CODE4(K) = CODE4
         ENDIF
!
!-----------------------------------------------------------------------
!  (3/3) ******** REALLOCATE IF THE BUFFER IS FULL ********
!        Check if the number of terms reached the size of memory block.
!        Save the buffer except the tail to file, removing zeros.
!  NOTE: May compress using zero coefficients, and no extra allocation
!        If compression is inefficient, save to file and reallocate
!-----------------------------------------------------------------------
         IF (LENBUF(L) .EQ. MEMBLK(L)) THEN

            OPEN (UNIT = NSCR, FILE = FILE(L), ACCESS = 'APPEND',       &
     &         DISPOSE = 'SAVE', STATUS = 'OLD', FORM = 'UNFORMATTED',  &
     &         BUFFERED = 'YES')

            DO 310 I = 1, LENBUF(L) - MDEPTH
               IF (ABS (PTROPER(L)%COEF(I)) .LT. TOL) CYCLE

               ICOUNT = ICOUNT + 1
               WRITE (NSCR) PTROPER(L)%COEF(I),                         &
     &            PTROPER(L)%CODE1(I),PTROPER(L)%CODE2(I)
               IF (LONG .EQ. 2)                                         &
     &         WRITE (NSCR) PTROPER(L)%CODE3(I),PTROPER(L)%CODE4(I)

               NLOC1(L) = NLOC1(L) + 1
  310       CONTINUE

            CLOSE (NSCR)

!----          Move tail to the bottom of the buffer, and remove zeros

            K = 0
            DO 320 I = LENBUF(L) - MDEPTH + 1, LENBUF(L)
               IF (ABS (PTROPER(L)%COEF(I)) .LT. TOL) CYCLE
               K = K + 1
               PTROPER(L)%COEF(K)  = PTROPER(L)%COEF(I)
               PTROPER(L)%CODE1(K) = PTROPER(L)%CODE1(I)
               PTROPER(L)%CODE2(K) = PTROPER(L)%CODE2(I)
               IF (LONG .EQ. 2) THEN
               PTROPER(L)%CODE3(K) = PTROPER(L)%CODE3(I)
               PTROPER(L)%CODE4(K) = PTROPER(L)%CODE4(I)
               ENDIF
  320       CONTINUE
            LENBUF(L) = K

         ENDIF

  400 CONTINUE  !  GO TO 400

      CLOSE (NAUX)  !  Close original file

!----    Delete the original file if it is different from the output
      IF (FILE_IN .NE. FILE_SH) THEN
         OPEN (UNIT = NAUX, FILE = FILE_IN, STATUS = 'OLD',         &
     &      DISPOSE = 'DELETE', FORM = 'UNFORMATTED')
         CLOSE (NAUX)
      ENDIF
!
!-----------------------------------------------------------------------
!     (4/4) Append the remaining terms from the buffers to the files
!-----------------------------------------------------------------------
      DO 420 L = 1, NQ
         OPEN (UNIT = NSCR, FILE = FILE(L), ACCESS = 'APPEND',      &
     &      DISPOSE = 'SAVE', STATUS = 'OLD', FORM = 'UNFORMATTED', &
     &      BUFFERED = 'YES')

         DO 410 I = 1, LENBUF(L)
            IF (ABS (PTROPER(L)%COEF(I)) .LT. TOL) CYCLE

            ICOUNT = ICOUNT + 1
            WRITE (NSCR) PTROPER(L)%COEF(I),                        &
     &         PTROPER(L)%CODE1(I),PTROPER(L)%CODE2(I)      
            IF (LONG .EQ. 2)                                        &
     &      WRITE (NSCR) PTROPER(L)%CODE3(I),PTROPER(L)%CODE4(I)

            NLOC1(L) = NLOC1(L) + 1
  410    CONTINUE

         CLOSE (NSCR)
  420 CONTINUE

      IF (IOUT .GT. 0) THEN
         CALL COUNTER (1,-1,1)
         CALL WATCH2 (ISPLIT)
      ENDIF
      CALL LWATCHTX (LSPLIT1,TIMETEXT)

!----    Release allocated memory

      DO 430 L = 1, NQ
         DEALLOCATE (PTROPER(L)%COEF)
         DEALLOCATE (PTROPER(L)%CODE1,PTROPER(L)%CODE2)
         IF (LONG .EQ. 2)                                       &
     &   DEALLOCATE (PTROPER(L)%CODE3,PTROPER(L)%CODE4)
  430 CONTINUE
      DEALLOCATE (PTROPER)
!
!-----------------------------------------------------------------------
!     Print Statistics about the Stage 1
!-----------------------------------------------------------------------
      IF (ICUT .EQ. 0) WRITE (NOUT,1600)

      PART = 100.0D0 * REAL (LCOUNT) / NTER
      WRITE (NOUT,1400) TIMETEXT,NTER,NTER-LCOUNT,LCOUNT,PART
      PART = 100.0D0 * REAL (ICOUNT) / LCOUNT
      WRITE (NOUT,1410) MDEPTH,LCOUNT,LCOUNT-ICOUNT,ICOUNT,PART
      WRITE (NOUT,1420) MAXCUT

      NTER0 = NTER
      NTER1 = ICOUNT
      NTER  = ICOUNT

      WRITE (NOUT,1430)
      DO 440 Q = 1, NQ
         PART  = 100.0D0 * REAL (NLOC (Q)) / LCOUNT
         PART1 = 100.0D0 * REAL (NLOC1(Q)) / NLOC(Q)
         WRITE (NOUT,1440) Q,NLOC(Q),PART,NLOC1(Q),PART1
  440 CONTINUE
      WRITE (NOUT,1450)
!
!=======================================================================
! (2) Main Loop over terms of polynomial - Second Pass
!=======================================================================
      OPEN (UNIT = NSCR, FILE = FILE_SH, DISPOSE = 'SAVE',          &
     &   STATUS = 'REPLACE', FORM = 'UNFORMATTED', BUFFERED = 'YES')
      CLOSE (NSCR)

      MBLOCK = 10000  !  Initial size of the allocated memory block
      MEMEXT = 10000  !  Memory block extension size added on overflow
      MAXMEM = 10000  !  ARRAY(NQ): For statistics: memory usage per Q

      CALL WATCH (ISPLIT)
      CALL LWATCH (LSPLIT2)
      ICENT0 = 0
      ICENT1 = 0
      IF (IOUT .GT. 0) THEN
         WRITE (*,9200)
         CALL COUNTER (0,0,1)
      ENDIF
!
!-----------------------------------------------------------------------
!     Loop over sorted terms
!-----------------------------------------------------------------------
      ICOUNT = 0  !  The counter for read terms
      LCOUNT = 0  !  The counter for saved terms

      DO 700 L = 1, NQ
      OPEN (UNIT = NAUX, FILE = FILE(L), STATUS = 'OLD',            &
     &   DISPOSE = 'DELETE', FORM = 'UNFORMATTED', BUFFERED = 'YES')

      ALLOCATE (PTROPER1%COEF (MBLOCK), STAT = IERR)
      IF (IERR .NE. 0) GO TO 920
      ALLOCATE (PTROPER1%CODE1(MBLOCK), STAT = IERR)
      IF (IERR .NE. 0) GO TO 920
      ALLOCATE (PTROPER1%CODE2(MBLOCK), STAT = IERR)
      IF (IERR .NE. 0) GO TO 920
      PTROPER1%COEF  = 0.0D0
      PTROPER1%CODE1 = 0
      PTROPER1%CODE2 = 0

      IF (LONG .EQ. 2) THEN
      ALLOCATE (PTROPER1%CODE3(MBLOCK), STAT = IERR)
      IF (IERR .NE. 0) GO TO 920
      ALLOCATE (PTROPER1%CODE4(MBLOCK), STAT = IERR)
      IF (IERR .NE. 0) GO TO 920
      PTROPER1%CODE3 = 0
      PTROPER1%CODE4 = 0
      ENDIF

      LENBUF1   = 0  !  The size of the buffer
      NALLOC(L) = 0  !  The number of memory reallocations per Q

      DO 600 II = 1, NLOC1(L)
         IF (IOUT .GT. 0) THEN
            IF (NTER .GT. 1000) THEN
               ICENT1 = ICOUNT / (NTER / 100)
            ELSE
               ICENT1 = 100 * ICOUNT / NTER
            ENDIF
            IF (ICENT1 .NE. ICENT0) THEN
               CALL COUNTER (1,ICENT1,1)
               ICENT0 = ICENT1
            ENDIF
         ENDIF

         ICOUNT = ICOUNT + 1
         READ (NAUX) COPER,CODE1,CODE2
         IF (LONG .EQ. 2) READ (NAUX) CODE3,CODE4
!
!-----------------------------------------------------------------------
!  (1/4) Check if the next operator already exists in the buffer
!-----------------------------------------------------------------------
         IF (LENBUF1 .GT. 0) THEN

            DO 510 I = 1, LENBUF1
               IF (CODE1 .NE. PTROPER1%CODE1(I)) CYCLE
               IF (CODE2 .NE. PTROPER1%CODE2(I)) CYCLE
               IF (LONG .EQ. 2) THEN
               IF (CODE3 .NE. PTROPER1%CODE3(I)) CYCLE
               IF (CODE4 .NE. PTROPER1%CODE4(I)) CYCLE
               ENDIF

               PTROPER1%COEF(I) = PTROPER1%COEF(I) + COPER

!----             If the hole created, move the last entry and LENBUF-1
               IF (ABS (PTROPER1%COEF(I)) .LT. TOL) THEN
                  IF (I .LT. LENBUF1) THEN
                     K = LENBUF1
                     PTROPER1%COEF(I) = PTROPER1%COEF(K)
                     PTROPER1%CODE1(I) = PTROPER1%CODE1(K)
                     PTROPER1%CODE2(I) = PTROPER1%CODE2(K)
                     IF (LONG .EQ. 2) THEN
                     PTROPER1%CODE3(I) = PTROPER1%CODE3(K)
                     PTROPER1%CODE4(I) = PTROPER1%CODE4(K)
                     ENDIF
                  ENDIF
                  LENBUF1 = LENBUF1 - 1
               ENDIF

               GO TO 600  !  Global DO-loop over terms
  510       CONTINUE

         ENDIF  !  IF (LENBUF1 .GT. 0)
!
!-----------------------------------------------------------------------
!  (2/4) Current operator was never encountered, create the new entry
!-----------------------------------------------------------------------
         LENBUF1 = LENBUF1 + 1
         K = LENBUF1
         PTROPER1%COEF(K)  = COPER
         PTROPER1%CODE1(K) = CODE1
         PTROPER1%CODE2(K) = CODE2
         IF (LONG .EQ. 2) THEN
         PTROPER1%CODE3(K) = CODE3
         PTROPER1%CODE4(K) = CODE4
         ENDIF
!
!-----------------------------------------------------------------------
!  (3/4) Check if the number of terms reached the size of current block.
!        Then save it to the file, allocate extended size, upload to RAM
!-----------------------------------------------------------------------
         IF (LENBUF1 .EQ. MBLOCK) THEN
            FILE_BUF = PATH_BIN(1:LENSTR(PATH_BIN,64)) // 'REALLOC.BIN'
            OPEN (UNIT = NSCR, FILE = FILE_BUF, DISPOSE = 'DELETE',     &
     &         BUFFERED = 'YES', FORM = 'UNFORMATTED')

            K = 0
            DO 520 I = 1, LENBUF1
               IF (ABS (PTROPER1%COEF(I)) .LT. TOL) CYCLE  !  Not needed, check
               K = K + 1
               WRITE (NSCR) PTROPER1%COEF(I),                           &
     &            PTROPER1%CODE1(I),PTROPER1%CODE2(I)
               IF (LONG .EQ. 2)                                         &
     &         WRITE (NSCR) PTROPER1%CODE3(I),PTROPER1%CODE4(I)
  520       CONTINUE
            LENBUF1 = K

            REWIND (NSCR)

            MBLOCK = LENBUF1 + MEMEXT
            MAXMEM(L) = MAX0 (MBLOCK, MAXMEM(L))

            DEALLOCATE (PTROPER1%COEF,PTROPER1%CODE1,PTROPER1%CODE2)
            ALLOCATE (PTROPER1%COEF (MBLOCK), STAT = IERR)
            IF (IERR .NE. 0) GO TO 920
            ALLOCATE (PTROPER1%CODE1(MBLOCK), STAT = IERR)
            IF (IERR .NE. 0) GO TO 920
            ALLOCATE (PTROPER1%CODE2(MBLOCK), STAT = IERR)
            IF (IERR .NE. 0) GO TO 920
            IF (LONG .EQ. 2) THEN
            ALLOCATE (PTROPER1%CODE3(MBLOCK), STAT = IERR)
            IF (IERR .NE. 0) GO TO 920
            ALLOCATE (PTROPER1%CODE4(MBLOCK), STAT = IERR)
            IF (IERR .NE. 0) GO TO 920
            ENDIF

            DO 530 I = 1, LENBUF1
               READ (NSCR) PTROPER1%COEF(I),                        &
     &            PTROPER1%CODE1(I),PTROPER1%CODE2(I)
               IF (LONG .EQ. 2)                                     &
     &         READ (NSCR) PTROPER1%CODE3(I),PTROPER1%CODE4(I)
  530       CONTINUE

            CLOSE (NSCR)
            NALLOC(L) = NALLOC(L) + 1
         ENDIF
  600 CONTINUE
!
!-----------------------------------------------------------------------
!     (4/4) Append the buffer to the destination file.
!-----------------------------------------------------------------------
      OPEN (UNIT = NSCR, FILE = FILE_SH, ACCESS = 'APPEND',         &
     &   STATUS = 'OLD', DISPOSE = 'SAVE', FORM = 'UNFORMATTED',    &
     &   BUFFERED = 'YES')

      DO 660 I = 1, LENBUF1
         COPER = PTROPER1%COEF(I)
         IF (ABS (COPER) .LT. TOL) CYCLE  !  Such cases do not occur?

!----       Decode OFFSETs (CODE1,CODE2) into operator powers: a^m b^n
         DO 630 J = 1, 2
            IF (J .EQ. 1) LL = PTROPER1%CODE1(I)
            IF (J .EQ. 2) LL = PTROPER1%CODE2(I)

            DO 620 Q = 1, NQ1
               KK = KSCALE1(Q)               !  K^(M-k), k = Q
               MODLK = MOD(LL, KK)           !  MOD = mod(L(k-1),K^(M-k))
               INDEX = (LL - MODLK) / KK     !  n(k) = (L(k-1) - MOD) / K^(M-k)
               LL = LL - KK * INDEX          !  L(k) = L(k-1) - K^(M-k) * n(k)
               IOPER(Q,J) = INDEX
  620       CONTINUE
  630    CONTINUE

!----       Decode OFFSETs (CODE3,CODE4) into operator powers: a^m b^n

         IF (LONG .EQ. 2) THEN
         DO 650 J = 1, 2
            IF (J .EQ. 1) LL = PTROPER1%CODE3(I)
            IF (J .EQ. 2) LL = PTROPER1%CODE4(I)

            DO 640 Q = LQ2, NQ
               M = Q - LQ2 + 1
               KK = KSCALE2(M)
               MODLK = MOD(LL, KK)
               INDEX = (LL - MODLK) / KK
               LL = LL - KK * INDEX
               IOPER(Q,J) = INDEX
  640       CONTINUE
  650    CONTINUE
         ENDIF

         LCOUNT = LCOUNT + 1  !  Counter for the final number of terms
         WRITE (NSCR) COPER,(IOPER(Q,A),IOPER(Q,B),Q=1,NQ)
  660 CONTINUE

      CLOSE (NSCR)

      NLOC2(L) = LENBUF1
      DEALLOCATE (PTROPER1%COEF,PTROPER1%CODE1,PTROPER1%CODE2)
      IF (LONG .EQ. 2) DEALLOCATE (PTROPER1%CODE3,PTROPER1%CODE4)

  700 CONTINUE

      CLOSE (NAUX)

      NTER2 = LCOUNT
      NTER  = LCOUNT  !  Output variable

      IF (IOUT .GT. 0) THEN
         CALL COUNTER (1,-1,1)
         CALL WATCH2 (ISPLIT)
      ENDIF
      CALL LWATCHTX (LSPLIT2,TIMETEXT)
!
!=======================================================================
!     Print Statistics and Release allocated memory
!=======================================================================
      PART = 100.0D0 * REAL (NTER2) / NTER1
      WRITE (NOUT,1500) TIMETEXT,NTER1,NTER1-NTER2,NTER2,PART

      WRITE (NOUT,1510)
      DO 810 L = 1, NQ
         PART = 100.0D0 * (ONE - REAL (NLOC2(L)) / REAL (NLOC1(L)))
         WRITE (NOUT,1520) L,NLOC1(L),NLOC2(L),PART,NALLOC(L),MAXMEM(L)
  810 CONTINUE
      WRITE (NOUT,1530)

      CALL LWATCHTX (LSPLIT,TIMETEXT)
      WRITE (NOUT,2100) TIMETEXT

      IERR = 0
      RETURN
  910 CONTINUE  !  Encoding overflow
      IERR = 1
      WRITE (*,*) 'ENCODING OVERFLOW'
      GO TO 990
  920 CONTINUE  !  Insufficient Memory
      IERR = 2
      WRITE (*,*) 'INSUFFICIENT MEMORY'
      NTER = 0
      GO TO 990
  930 CONTINUE  !  File I/O Error: Generic
      IERR = 3
      WRITE (*,*) 'READ ERROR'
      GO TO 990
  940 CONTINUE  !  File I/O Error: EOF
      IERR = 3
      WRITE (*,*) 'END OF FILE'
  990 STOP
      RETURN

!=======================================================================

 1210 FORMAT ('Breakdown of terms with different total powers:'/    &
     &   62('-')/'Power    N(Base)     Terms    Fraction',          &
     &   '   Max.Code = N(base)^NQ'/62('-'))
 1220 FORMAT (I4,I10,I12,F10.2,' %',I24)
 1230 FORMAT (62('-'))

 1600 FORMAT ('The terms with max total power will be ignored.')

 1400 FORMAT (/'Stage 1 Accomplished in ',A16,', Statistics Summary:'// &
     &   'The original number of terms   = ',I16/                       &
     &   'The number of max. power terms = ',I16/                       &
     &   'The remaining number of terms  = ',I16/                       &
     &   'The share of remaining terms   = ',F14.1,' %')
 1410 FORMAT (/                                                         &
     &   'The depth of scanning terms    = ',I16/                       &
     &   'The number of considered terms = ',I16/                       &
     &   'The number of removed terms    = ',I16/                       &
     &   'The remaining number of terms  = ',I16/                       &
     &   'The share of remaining terms   = ',F14.1,' %')
 1420 FORMAT ('The max length of zero chain   = ',I16)
 1430 FORMAT (/'Breakdown of terms per hash function (Q):'/60('-')/     &
     &   '   Q        Original    Fraction       Resulting    Fraction'/&
     &   60('-'))
 1440 FORMAT (I4,I16,F10.1,' %',I16,F10.1,' %')
 1450 FORMAT (60('-'))

 1500 FORMAT (/'Stage 2 Accomplished in ',A16,', Statistics Summary:'// &
     &   'The initial number of terms    = ',I16/                       &
     &   'The number of removed terms    = ',I16/                       &
     &   'The remaining number of terms  = ',I16/                       &
     &   'The share of remaining terms   = ',F14.1,' %')

 1510 FORMAT (/'Summary Table after Collection of Terms:'/78('-')/      &
     &   'Power       Original       Resulting   Reduction',            &
     &   '      Realloc.     Max. Buffer'/78('-'))
 1520 FORMAT (I4,I16,I16,F10.1,' %',I14,I16)
 1530 FORMAT (78('-'))

 2100 FORMAT ('[COLLECT4] -- return to the calling routine.'/           &
     &   'Total Time Elapsed: ',A16)

 9000 FORMAT ('Collect Terms in the Polynomial, Size :',A12)
 9100 FORMAT ('Stage 1 of 2: Collect Terms with Same Operators (1)...'\)
 9200 FORMAT ('Stage 2 of 2: Collect Terms with Same Operators (2)...'\)
    END

    
!-----------------------------------------------------------------------
!     Subroutine: 'UPLOAD'    FIRST: 27 Nov 2009  LAST EDIT: 06 Sep 2020
!
!     PURPOSE:
!     Read operator polynomial from external file and optionally print.
!
!     CALLED: MAINCVPT (Sibert.for)
!
!     CALLS: OPEROUT ();
!
!     INPUT:
!     NQ     -- The number of vibrational degrees of freedom;
!     NTERM  -- The number of terms in the Operator Polynomial;
!     FILNAM -- File name where the operator is stored (in binary form);
!     TOL    -- Tolerance parameter determining the accuracy of
!               calculations. Terms with |COEF| < TOL are ignored;
!     LMAX   -- The maximum number of terms to be printed;
!     IOUT   -- Flag indicating that uploaded Operator must be printed.
!
!     OUTPUT:
!     OPCOEF -- Scalar coefficients of terms in operator polynomial H;
!     IPOWHS -- Powers of creation/annihilation operators in H.
!
!     FILE I/O:
!     Reading NTERHS records from LUN = NAUX in unformatted mode.
!
!     NOTE(S):
!     1. Logical Unit Number NAUX is taken from COMMON block /IOLUN/.
!-----------------------------------------------------------------------
      SUBROUTINE UPLOAD (NQ,NTERM,FILNAM,OPCOEF,IPOWER,TOL,LMAX,IOUT)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  LOUT,NLOG,NINP,NOUT,NAUX,NSCR
      DIMENSION OPCOEF(NTERM)
      INTEGER*1 IPOWER(NQ,2,NTERM)
      CHARACTER*64 FILNAM
      DATA NOPER /2/

      IF (NTERM .EQ. 0) THEN
         WRITE (NOUT,1000)
         RETURN
      ENDIF

      OPEN (UNIT = NAUX, FILE = FILNAM, FORM = 'UNFORMATTED')
      DO 100 I = 1, NTERM
  100 READ (NAUX) OPCOEF(I),((IPOWER(M,L,I),L=1,NOPER),M=1,NQ)
      CLOSE (NAUX)

      IF (IOUT .GT. 0) THEN
         WRITE (NOUT,1100) FILNAM,NTERM
         CALL OPEROUT (NQ,NOPER,NTERM,OPCOEF,IPOWER,TOL,LMAX)
      ENDIF

      RETURN
 1000 FORMAT (/'[UPLOAD] -- Operator Polynomial Has Zero Length.')
 1100 FORMAT (/'[UPLOAD] -- Upload Ladder Operator Polynomial to RAM ',   &
     &   'from the file'/A/                                               &
     &   'The number of terms (products of Creation/Annihilation ',       &
     &   'operators) = ',I10)
    END
    
!-----------------------------------------------------------------------
!     Subroutine: 'SORTPOL'   FIRST: 13 Apr 2018  LAST EDIT:  6 Sep 2020
!
!     PURPOSE:
!     Arrange terms in descending order using quick-sort algorithm.
!     Save ordered terms to a file, then download in correct order.
!
!     NOTE: Logical I/O unit = NSCR must not be engaged before the call.
!
!     ALGORITHM:
!     Quick sort ("Numerical Recepies").
!
!     CALLED:
!
!     INPUT:
!     NQ     -- The number of vibrational degrees of freedom;
!     NOPER  -- The number of creation/annihilation operators in record;
!     NTERM  -- The number of terms in operator polynomial;
!     COEF   -- Coefficients of individual terms in operator polynomial;
!     IPOWER -- Powers of creation/annihilation operators in polynomial;
!     TOL    -- Tolerance parameter determining the accuracy of
!               calculations. Terms with |COEF| < TOL are ignored;
!
!     OUTPUT:
!     NTERM  -- The number of terms in operator polynomial after
!               collection of terms;
!     COEF   -- Coefficients of individual terms in operator polynomial;
!     IPOWER -- Powers of creation/annihilation operators in polynomial.
!     IERR   -- Return code, = 1 for correct work, = 0 otherwise.
!
!     FILE I/O:
!     Temporary file ../SORTING.BIN is created in ../BIN.. directory.
!
!     NOTE(S):
!     1. 14/03/2010: Quick sort is used instead of previous bubble sort.
!-----------------------------------------------------------------------
      SUBROUTINE SORTPOL (NQ,NOPER,NTERM,COEF,IPOWER,TOL,LIMIT,IERR)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  LOUT,NLOG,NINP,NOUT,NAUX,NSCR
      COMMON /PATHS/  PATH_EXE,PATH_PRO,PATH_OUT,PATH_BIN
      CHARACTER*64 PATH_EXE,PATH_PRO,PATH_OUT,PATH_BIN
      DIMENSION COEF(NTERM)
      INTEGER*1 IPOWER(NQ,NOPER,*)

      ALLOCATABLE ARRAY(:),IVIRT(:),ICONJ(:)
      LOGICAL DIAG,CONJ
      CHARACTER*64 FILE_SORT,FILEPATH

      DATA ZERO /0.0D0/
      DATA FILE_SORT /'Sorting.bin'/

      IF (LOUT .GT. 1) WRITE (NOUT,1000)

!-----------------------------------------------------------------------
!     Arrange terms in descending order using quick-sort algorithm.
!     Save ordered terms to a file, then download in correct order
!-----------------------------------------------------------------------
      ALLOCATE (ARRAY(NTERM),IVIRT(NTERM),ICONJ(NTERM))

      DO 110 I = 1, NTERM
         ARRAY(I) = ABS (COEF(I))
  110 CONTINUE

      CALL RQSORT (NTERM,ARRAY,IVIRT)

!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
      INQUIRE (UNIT = NSCR, OPENED = IOSTAT)
      IF (IOSTAT) STOP 'SORTPOL: I/O unit engaged.'

      FILEPATH = TRIM(PATH_BIN) // FILE_SORT
      OPEN (UNIT = NSCR, FILE = FILEPATH, DISPOSE = 'DELETE',              &
     &   BUFFERED = 'YES', FORM = 'UNFORMATTED')

      DO 120 J = NTERM, 1, -1
         I = IVIRT(J)
         WRITE (NSCR) COEF(I),((IPOWER(K,L,I),L=1,NOPER),K=1,NQ)
  120 CONTINUE

      REWIND (NSCR)
      DO 130 I = 1, NTERM
         READ  (NSCR) COEF(I),((IPOWER(K,L,I),L=1,NOPER),K=1,NQ)
  130 CONTINUE

!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
      DO 200 J = 1, NTERM
         ICONJ(J) = 0
         IVIRT(J) = J
  200 CONTINUE

      DELMAX = 0.0D0
      DO 300 J = 1, NTERM
         IF (ICONJ(J) .EQ. -1) CYCLE

         I = IVIRT(J)
         DIAG = .TRUE.
         DO 210 M = 1, NQ
            IF (IPOWER(M,1,I) .EQ. IPOWER(M,2,I)) CYCLE
            DIAG = .FALSE.
            EXIT
  210    CONTINUE
         IF (DIAG) CYCLE

         DO 230 L = 1, NTERM
            IF (L .EQ. J) CYCLE
            IF (ICONJ(L) .NE. 0) CYCLE

            K = IVIRT(L)
            CONJ = .TRUE.
            DO 220 M = 1, NQ
               IF (IPOWER(M,1,I) .EQ. IPOWER(M,2,K) .AND.                 &
     &             IPOWER(M,2,I) .EQ. IPOWER(M,1,K)) CYCLE
               CONJ = .FALSE.
               EXIT
  220       CONTINUE

            IF (CONJ) THEN
               ISWAP      = IVIRT(J+1)
               IVIRT(J+1) = IVIRT(L)
               IVIRT(L)   = ISWAP

               ICONJ(J)   =  1
               ICONJ(J+1) = -1
               EXIT
            ENDIF
  230    CONTINUE

         DELTA = ABS (COEF(I) - COEF(K))
         DELMAX = MAX (DELMAX, DELTA)
         IF (DELTA .GT. TOL) THEN
            WRITE (NOUT,1100) I,COEF(I),K,COEF(K)
            IERR = 1
            GO TO 310
         ELSE
            COEF(I) = (COEF(I) + COEF(K)) / 2.0D0
            COEF(K) = COEF(I)
         ENDIF
  300 CONTINUE

!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
      IERR = 0
  310 REWIND (NSCR)
      WRITE (NOUT,1200) DELMAX

      DO 320 I = 1, NTERM
         J = IVIRT(I)
         WRITE (NSCR) COEF(J),((IPOWER(K,L,J),L=1,NOPER),K=1,NQ)
  320 CONTINUE

      REWIND (NSCR)
      DO 330 I = 1, NTERM
         READ  (NSCR) COEF(I),((IPOWER(K,L,I),L=1,NOPER),K=1,NQ)
  330 CONTINUE
      CLOSE (NSCR)

      LIMIT1 = LIMIT
      IF (IERR .GT. 0) LIMIT1 = NTERM
      IF (LOUT .GT. 1 .OR. IERR .GT. 0)                                   &
     &   CALL OPEROUT (NQ,NOPER,NTERM,COEF,IPOWER,TOL,LIMIT1)

  900 DEALLOCATE (ARRAY,IVIRT,ICONJ)

      RETURN
 1000 FORMAT (/'[SORTPOL]'/                                               &
     &   'Organize operator polynomial in standard form:')
 1100 FORMAT (/'SORTPOL: Fatal error --'/I8,ES24.16,I8,ES24.16,ES10.2)
 1200 FORMAT (/'Maximum deviation between conjugated terms:',ES12.2)
    END
    
!-----------------------------------------------------------------------
!     Subroutine: 'OPEROUT'   FIRST: 21 Nov 2009  LAST EDIT: 14 Feb 2020
!
!     PURPOSE:
!     Print terms of Operator Polynomial, limited to LMAX terms.
!
!     CALLED: MAINCVPT ();
!
!     INPUT:
!     NQ     -- The number of vibrational degrees of freedom;
!     NOPER  -- The number of different types of creation/annihilation
!               operators;
!     NTERM  -- The number of operator terms;
!     HCOEF  -- Scalar coefficients of terms in operator polynomial H;
!     IPOWER -- Powers of Operators (A,A+);
!     TOL    -- Tolerance parameter determining the accuracy of
!               calculations. Terms with |COEF| < TOL are ignored.
!     LMAX   -- Maximum number of trems printed.
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------
      SUBROUTINE OPEROUT (NQ,NOPER,NTERM,HCOEF,IPOWER,TOL,LMAX)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  LOUT,NLOG,NINP,NOUT,NAUX,NSCR
      DIMENSION HCOEF(NTERM)
      INTEGER*1 IPOWER(NQ,NOPER,*)
      DIMENSION NPOWER(0:12),NDIAG(0:12)
      ALLOCATABLE OPER(:),INDQ(:),IPOW(:)
      CHARACTER*1 OPER,TYPE(2)
      INTEGER*4 Q
      LOGICAL DIAG,CONJ
      CHARACTER*16 TCOEF
      CHARACTER* 4 LABEL
      DATA TYPE /'a','b'/

      IF (NOPER .GT. 2) STOP '[OPEROUT] Wrong NOPER.'

      IF (NTERM .EQ. 0) RETURN

      NDIM = NQ * NOPER
      ALLOCATE (OPER(NDIM),INDQ(NDIM),IPOW(NDIM))

      WRITE (NOUT,1000)

      MAXPOW = 0
      NPOWER = 0
      VALMIN = ABS (HCOEF(1))
      N = 0  !  The number of coefficients greater than TOL
      NDIAG = 0

      DO 200 I = 1, NTERM
         IF (ABS (HCOEF(I)) .LT. TOL) CYCLE
         N = N + 1

         L = 0  !  The number of non-zero operators
         K = 0  !  Total power of operators
         DIAG = .TRUE.
         DO 120 Q = 1, NQ
            DO 100 J = 1, NOPER
               IF (IPOWER(Q,J,I) .EQ. 0) CYCLE
               L = L + 1
               K = K + IPOWER(Q,J,I)
               OPER(L) = TYPE(J)
               INDQ(L) = Q
               IPOW(L) = IPOWER(Q,J,I)
  100       CONTINUE
            DO 110 J = 1, NOPER - 1
               IF (IPOWER(Q,J,I) .NE. IPOWER(Q,J+1,I)) DIAG = .FALSE.
  110       CONTINUE
  120    CONTINUE

         MAXPOW = MAX (MAXPOW,K)
         NPOWER(K) = NPOWER(K) + 1
         VALMIN = QMIN1 (VALMIN, QABS (HCOEF(I)))

         IF (DIAG) THEN
            CONJ = .FALSE.
            IPAIR = I
         ELSE
            DO 140 J = 1, NTERM
               IF (J .EQ. I) CYCLE

               CONJ = .TRUE.
               DO 130 Q = 1, NQ
                  IF (IPOWER(Q,1,I) .EQ. IPOWER(Q,2,J) .AND.        &
     &                IPOWER(Q,2,I) .EQ. IPOWER(Q,1,J)) CYCLE
                  CONJ = .FALSE.
                  EXIT
  130          CONTINUE

               IF (CONJ) THEN
                  IPAIR = J
                  DELTA = HCOEF(I) - HCOEF(J)
                  EXIT
               ENDIF
  140       CONTINUE
         ENDIF

         IF (DIAG) NDIAG(K) = NDIAG(K) + 1

         IF (ABS (HCOEF(I)) .GE. 1.0D-8) THEN
            WRITE (FMT = "(SP, F16.8)", UNIT = TCOEF) HCOEF(I)
         ELSE
            WRITE (FMT = "(SP,ES16.6)", UNIT = TCOEF) HCOEF(I)
         ENDIF

         IF (DIAG) THEN
            WRITE (FMT = "('Diag')", UNIT = LABEL)
         ELSE
            IF (I .LT. IPAIR) THEN
               WRITE (FMT = "('Main')", UNIT = LABEL)
            ELSE
               WRITE (FMT = "('Conj')", UNIT = LABEL)
            ENDIF
         ENDIF

         IF (I .GT. LMAX) CYCLE

         WRITE (NOUT,1010) I,LABEL,IPAIR,K,TCOEF,                       &
     &      (OPER(J),INDQ(J),IPOW(J),J=1,L)

         IF (CONJ .AND. I .GT. IPAIR .AND. ABS (DELTA) .GT. 1.0D-8)     &
     &      WRITE (NOUT,1020) DELTA
  200 CONTINUE

      IF (N .EQ. 0) WRITE (NOUT,1100) TOL
      IF (N .GT. 0 .AND. NTERM .GT. LMAX)                               &
     &   WRITE (NOUT,1110) NTERM,LMAX,VALMIN

      IF (N .GT. 0) THEN
         WRITE (NOUT,1200)
         DO 210 I = 0, 12
            IF (NPOWER(I) .GT. 0) WRITE (NOUT,1210) I,NPOWER(I),NDIAG(I)
  210    CONTINUE
      ENDIF

      DEALLOCATE (OPER,INDQ,IPOW)

      RETURN
 1000 FORMAT (/'[OPEROUT] -- Print Ladder Operator Polynomial:'/80('-')/    &
     &   'Term  Attr  Ref.  Power   ',                                      &
     &   'Coefficient * Ladder operators: Ladder(mode)^power'/80('-'))
 1010 FORMAT (I4,2X,A4,' (',I4,')  ^',I1,A16 :                              &
     &        4(' * ',A1,'(',I2,')^',I1:)/                                  &
     &   (37X,4(' * ',A1,'(',I2,')^',I1:)))

 1020 FORMAT ('Difference  >>',SP,F16.8)

 1100 FORMAT ('Operator has no terms with coefficient over =',ES8.1)
 1110 FORMAT ('(...) Output truncated.'/                                    &
     &   'Actual number of operator terms = ',I8/                           &    
     &   'Given limit of output lines     = ',I8/                           &
     &   'Smallest operator coefficient   = ',ES8.1)

 1200 FORMAT (80('-'))
 1210 FORMAT ('Power = ',I2,':  Terms = ',I8,', incl. Diagonal = ',I8)
      END
