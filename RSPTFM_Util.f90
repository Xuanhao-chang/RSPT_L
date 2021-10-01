!-----------------------------------------------------------------------------------------------
!     Subroutine: 'RQSORT'    FIRST: 01 Mar 2010  LAST EDIT: 14 Mar 2010
!
!     PURPOSE:
!     Quick sort of REAL*16 values using "heap sort" algorithm in
!
!     ALGORITHM:
!     The index array INDEX is produced such that ARRAY(INDEX(i)) gives
!     the values of original ARRAY sorted in ascending order.
!
!     REFERENCE(S):
!     [1] William H. Press, Brian P. Flannery, Saul A. Teukolski,
!     William T. Vetterling / Numerical Recipes: The Art of Scientific
!     Computing / Cambridge University Press, Cambridge, 1986, 818 p.
!     See Page 232.
!
!     CALLED: COMPAC (CPTserv.for);
!
!     INPUT:
!     LENARR -- The dimension of input REAL*16 array;
!     ARRAY  -- The array of REAL*16 values to be sorted.
!
!     OUTPUT:
!     INDEX  -- The integer array containing indices of sorted values.
!
!     NOTE(S):
!     The algorithm was tested using internal check, see below.
!-----------------------------------------------------------------------------------------------
      SUBROUTINE RQSORT (LENARR,ARRAY,INDEX)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      DIMENSION ARRAY(LENARR)
      DIMENSION INDEX(LENARR)

      DO 100 J = 1, LENARR
         INDEX(J) = J
  100 CONTINUE

      IF (LENARR .LE. 1) RETURN

      L  = LENARR / 2 + 1
      IR = LENARR

  200 CONTINUE
         IF (L .GT. 1) THEN
            L = L - 1
            IND = INDEX(L)
            AUX = ARRAY(IND)
         ELSE
            IND = INDEX(IR)
            AUX = ARRAY(IND)
            INDEX(IR) = INDEX(1)
            IR = IR - 1
            IF (IR .EQ. 1) THEN
               INDEX(1) = IND
               GO TO 400
            ENDIF
         ENDIF

         I = L
         J = L + L
  300    CONTINUE
         IF (J .LE. IR) THEN
            IF (J .LT. IR) THEN
               IF (ARRAY(INDEX(J)) .LT. ARRAY(INDEX(J+1))) J = J + 1
            ENDIF

            IF (AUX .LT. ARRAY(INDEX(J))) THEN
               INDEX(I) = INDEX(J)
               I = J
               J = J + J
            ELSE
               J = IR + 1
            ENDIF
         GO TO 300
         ENDIF

         INDEX(I) = IND
      GO TO 200

  400 CONTINUE

      RETURN
    END

!-----------------------------------------------------------------------------------------------
! COPYRIGHT (c) 1980 AEA Technology
! Original date 1 December 1993
!       Toolpack tool decs employed.
!       Arg dimensions changed to *.
! 1/4/99 Size of MARK increased to 100.
! 13/3/02 Cosmetic changes applied to reduce single/double differences
!
! 12th July 2004 Version 1.0.0. Version numbering added.
!-----------------------------------------------------------------------------------------------
    SUBROUTINE KB07AD(COUNT,N,INDEX)
!
!             KB07AD      HANDLES DOUBLE PRECISION VARIABLES
!  THE WORK-SPACE 'MARK' OF LENGTH 100 PERMITS UP TO 2**50 NUMBERS
!  TO BE SORTED.

!     .. Scalar Arguments ..
    INTEGER*4 N
!     ..
!     .. Array Arguments ..
      
    REAL*16 COUNT(*)  
    INTEGER*4 INDEX(*)
!     ..
!     .. Local Scalars ..
    REAL*16 AV,X
    INTEGER*4 I,IF,IFK,IFKA,INT,INTEST,IP,IS,IS1,IY,J,K,K1,LA,LNGTH,M,MLOOP
!     ..
!     .. Local Arrays ..
    INTEGER*4 MARK(100)
!     ..
!     .. Executable Statements ..
!  SET INDEX ARRAY TO ORIGINAL ORDER .
    DO 10 I = 1,N
        INDEX(I) = I
10  CONTINUE
!  CHECK THAT A TRIVIAL CASE HAS NOT BEEN ENTERED .
    IF (N.EQ.1) GO TO 200
    IF (N.GE.1) GO TO 30
    WRITE (6,FMT=20)

20  FORMAT (/,/,/,20X,' ***KB07AD***NO NUMBERS TO BE SORTED ** ',       &
           & 'RETURN TO CALLING PROGRAM')
    GO TO 200
!  'M' IS THE LENGTH OF SEGMENT WHICH IS SHORT ENOUGH TO ENTER
!  THE FINAL SORTING ROUTINE. IT MAY BE EASILY CHANGED.
30  M = 12
!  SET UP INITIAL VALUES.
    LA = 2
    IS = 1
    IF = N
    DO 190 MLOOP = 1,N
!  IF SEGMENT IS SHORT ENOUGH SORT WITH FINAL SORTING ROUTINE .
      IFKA = IF - IS
      IF ((IFKA+1).GT.M) GO TO 70
!********* FINAL SORTING ***
!  ( A SIMPLE BUBBLE SORT )
      IS1 = IS + 1
      DO 60 J = IS1,IF
        I = J
40      IF (COUNT(I-1).LT.COUNT(I)) GO TO 60
        IF (COUNT(I-1).GT.COUNT(I)) GO TO 50
        IF (INDEX(I-1).LT.INDEX(I)) GO TO 60
50      AV = COUNT(I-1)
          COUNT(I-1) = COUNT(I)
          COUNT(I) = AV
          INT = INDEX(I-1)
          INDEX(I-1) = INDEX(I)
          INDEX(I) = INT
          I = I - 1
          IF (I.GT.IS) GO TO 40
60    CONTINUE
      LA = LA - 2
      GO TO 170
!             *******  QUICKSORT  ********
!  SELECT THE NUMBER IN THE CENTRAL POSITION IN THE SEGMENT AS
!  THE TEST NUMBER.REPLACE IT WITH THE NUMBER FROM THE SEGMENT'S
!  HIGHEST ADDRESS.
70    IY = (IS+IF)/2
      X = COUNT(IY)
      INTEST = INDEX(IY)
      COUNT(IY) = COUNT(IF)
      INDEX(IY) = INDEX(IF)
!  THE MARKERS 'I' AND 'IFK' ARE USED FOR THE BEGINNING AND END
!  OF THE SECTION NOT SO FAR TESTED AGAINST THE PRESENT VALUE
!  OF X .
        K = 1
        IFK = IF
!  WE ALTERNATE BETWEEN THE OUTER LOOP THAT INCREASES I AND THE
!  INNER LOOP THAT REDUCES IFK, MOVING NUMBERS AND INDICES AS
!  NECESSARY, UNTIL THEY MEET .
        DO 110 I = IS,IF
          IF (X.GT.COUNT(I)) GO TO 110
          IF (X.LT.COUNT(I)) GO TO 80
          IF (INTEST.GT.INDEX(I)) GO TO 110  
80        IF (I.GE.IFK) GO TO 120
          COUNT(IFK) = COUNT(I)
          INDEX(IFK) = INDEX(I)
          K1 = K
          DO 100 K = K1,IFKA
            IFK = IF - K
            IF (COUNT(IFK).GT.X) GO TO 100
            IF (COUNT(IFK).LT.X) GO TO 90
            IF (INTEST.LE.INDEX(IFK)) GO TO 100
90          IF (I.GE.IFK) GO TO 130
            COUNT(I) = COUNT(IFK)
            INDEX(I) = INDEX(IFK)
            GO TO 110
100       CONTINUE
          GO TO 120
110     CONTINUE
!  RETURN THE TEST NUMBER TO THE POSITION MARKED BY THE MARKER
!  WHICH DID NOT MOVE LAST. IT DIVIDES THE INITIAL SEGMENT INTO
!  2 PARTS. ANY ELEMENT IN THE FIRST PART IS LESS THAN OR EQUAL
!  TO ANY ELEMENT IN THE SECOND PART, AND THEY MAY NOW BE SORTED
!  INDEPENDENTLY .
120     COUNT(IFK) = X
        INDEX(IFK) = INTEST
        IP = IFK
        GO TO 140

130     COUNT(I) = X
        INDEX(I) = INTEST
        IP = I
!  STORE THE LONGER SUBDIVISION IN WORKSPACE.
140     IF ((IP-IS).GT. (IF-IP)) GO TO 150
        MARK(LA) = IF
        MARK(LA-1) = IP + 1
        IF = IP - 1
        GO TO 160

150     MARK(LA) = IP - 1
        MARK(LA-1) = IS
        IS = IP + 1
!  FIND THE LENGTH OF THE SHORTER SUBDIVISION.
160     LNGTH = IF - IS
        IF (LNGTH.LE.0) GO TO 180
!  IF IT CONTAINS MORE THAN ONE ELEMENT SUPPLY IT WITH WORKSPACE .
        LA = LA + 2
        GO TO 190
170     IF (LA.LE.0) GO TO 200
!  OBTAIN THE ADDRESS OF THE SHORTEST SEGMENT AWAITING QUICKSORT
180     IF = MARK(LA)
        IS = MARK(LA-1)
190     CONTINUE
200     RETURN

      END
    
    
!-----------------------------------------------------------------------------------------------
!     Subroutine: 'PSTATE'    FIRST: 13 Apr 2012  LAST EDIT: 14 Jun 2020
!
!     PURPOSE:
!     For a given polyad formula and polyad number value, find all
!     possible vibrational states (or basis functions, what is the same)
!     Return total number of found states (NSIZE) and write found states
!     in a file LUN = LUNPOL.
!     It is very important to remember that in order to obtain all
!     possible basis functions or vibrational states, it is necessary
!     to set MAXEXC = NPOLY, because the minimum polyad coefficient = 1.
!     MAXEXC is introduced for the reason that for bigger molecules the
!     search of states can take very long time and produce huge numbers.
!     The basis functions are grouped by the symmetry.
!
!     THEORY: or ALGORITHM:
!     N(t) >= 0,  N(t) = C1 * v(1) + C2 * v(2) + ... C(NQ) * v(NQ).
!     C(i) are non-negative integers.
!     NOTE: Zeros in C(i) mean that such modes produce infinite blocks
!
!     REFERENCE(S):
!     [1] William F. Polik and J. Ruud van Ommen "The Multiresonant
!     Hamiltonian Model and Polyad Quantum Numbers for Highly Excited
!     Vibrational States" ACS Symp. Ser, 1997, 678, p.51-68.
!
!     CALLED: RSPTFM_Proc ().
!
!     INPUT:
!     NQ     -- The number of vibrational degrees of freedom;
!     ISYMQ -- Index of Symmetry Species given the index of normal mode
!               in spectroscopic order;
!     KPOLY  -- Polyad formula, i.e. coefficients for vibrational modes;
!     NPOLY  -- Particular polyad number, for which the set of all
!               possible quantum number sets is sought;
!     MAXQUA -- Maximum excitation of vibrational modes;
!               IMPORTANT: To obtain ALL possible basis functions or
!               vibrational states, it is necessary to set MAXQUA=NPOLY;
!     MODRUN --
!     LIMIT  -- Printout truncation.
!
!     OUTPUT:
!     NSIZE  -- The size of polyad block;
!     MAXTOT -- Maximum total excitation in generated states;
! (-) NSTSYM -- The number of states per symmetry species;
!     <FILE> -- LUN = NSCR, created states are saved to <FILE>.
!
!     FILE I/O:
!     Created basis set is saved to a file with LUN = NSCR.
!
!     NOTES:
!     1. 08-06-2020: Remastered from POLBLK (ANCO/Diag.for).
!-----------------------------------------------------------------------------------------------
      SUBROUTINE PSTATE (NQ,WAVNUM,KPOLY,NPOLY,MAXQUA,MODRUN,       & !ISYMQ,
     &   NSIZE,LQUANT,MAXTOT,LIMIT,IOUT)
      IMPLICIT REAL*8 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  LOUT,NLOG,NINP,NOUT,NAUX,NSCR

!      CHARACTER* 4 LSYMSP
!      COMMON /SYMINF/ NSYMSP,LSYMSP(0:12),MSYMSP(0:12,0:12)

      REAL*16 WAVNUM(NQ)
      REAL*16 EZERO
      INTEGER*4 KPOLY(NQ),LQUANT(NQ,0:*)                               !ISYMQ(NQ),
        
      REAL*16, ALLOCATABLE :: ENERGY(:)
      INTEGER*4, ALLOCATABLE :: IORDER(:)
      INTEGER*4 IQUANT(NQ)
      INTEGER*4 IUPPER(NQ)                     !  DO-loop emulation
      INTEGER*4 IX(NQ),IQ(NQ)
      CHARACTER*80 FILE  !  Vibrational states for N(t) are saved here
      CHARACTER*48, ALLOCATABLE :: XSTID(:)    !  Extended state ID
      LOGICAL ZERPOI,UPDATE
!----    External function
!      INTEGER*4 ISYMSTAT

      IF (MAXQUA .LT. NPOLY .AND. MODRUN .EQ. 0)                          &
     &   WRITE (NOUT,1000)
 1000 FORMAT (/'Polyad block may be incomplete as maximum excitation ',   &
     &   'is less than N(T).')

!-----------------------------------------------------------------------------------------------
!     Initialize loop counters IQUANT and upper limits for them IUPPER
!-----------------------------------------------------------------------------------------------
      IQUANT = 0                       !  Array: NQ quantum numbers

      DO 100 I = 1, NQ
      DO 100 K = 0, MAXQUA
         IF (K * KPOLY(I) .LE. NPOLY) IUPPER(I) = K
  100 CONTINUE

      ZERPOI = .TRUE.                  !  To skip zero-point state
      UPDATE = .FALSE.                 !  Flag for full update of IPOLY

      NSIZE  = 0  !  The size of polyad block of states for N(T) = NPOLY
      IPOLY  = 0  !  Initialize polyad number for the zero-point state

!----    PSTATE () is called in a loop over NPOLY, rewind every time
      REWIND (NSCR)

!=========================== NESTED DO-LOOPS ===========================
!     DO 300 v(m) = 0, MAXQUA
!     ...
!     DO 300 v(2) = 0, MAXQUA
!     DO 300 v(1) = 0, MAXQUA
!=========================== EMULATION OF DO ===========================
  200 CONTINUE  !  GO TO
         IF (UPDATE) THEN
            IPOLY = 0
            DO 210 K = 1, NQ
               IPOLY = IPOLY + KPOLY(K) * IQUANT(K)
               IF (IPOLY .GT. NPOLY) GO TO 220
  210       CONTINUE
            UPDATE = .FALSE.
         ENDIF

         IF (IPOLY .EQ. 0 .AND. ZERPOI) THEN
            ZERPOI = .FALSE.
            GO TO 220
         ENDIF

!----       Save the set of quantum numbers to a dedicated file NSCR

         IF (IPOLY .EQ. NPOLY) THEN  !  NPOLY -- The polyad number N(T)
            WRITE (NSCR) IQUANT
            NSIZE = NSIZE + 1
         ENDIF

!----       Update counters

  220    CONTINUE
         DO 230 I = 1, NQ
            IF (IQUANT(I) .LT. IUPPER(I)) THEN
               IQUANT(I) = IQUANT(I) + 1
               IPOLY = IPOLY + KPOLY(I)
               GO TO 200
            ENDIF
            IQUANT(I) = 0
            UPDATE = .TRUE.
  230    CONTINUE

  300 CONTINUE  !  END-DO

!----    First time only NSIZE is returned
      IF (MODRUN .EQ. 0) RETURN

!-----------------------------------------------------------------------------------------------
!     Order Vibrational states by energy in ascending order
!-----------------------------------------------------------------------------------------------
      IF (NSIZE .EQ. 0) GO TO 900
      ALLOCATE (ENERGY(NSIZE),IORDER(NSIZE))

      REWIND (NSCR)
      DO 320 K = 1, NSIZE
         READ (NSCR) IQUANT
         DO 310 I = 1, NQ
  310        LQUANT(I,K) = IQUANT(I)
         CALL ENERHAOS (NQ,WAVNUM,IQUANT, EZERO, ENERGY(K))
         ENERGY(K) = ENERGY(K) - EZERO
  320 CONTINUE

!      CALL RQSORT    (NSIZE,ENERGY,IORDER)
      CALL KB07AD    (ENERGY,NSIZE,IORDER)

      REWIND (NSCR)
      DO 340 K = 1, NSIZE
         L = IORDER(K)
         DO 330 I = 1, NQ
330          IQUANT(I)  = LQUANT(I,L)
         WRITE (NSCR) IQUANT
  340 CONTINUE

      DEALLOCATE (ENERGY,IORDER)

      REWIND (NSCR)
      K = 0
      IQUANT = 0
      DO 420 N = 1, NSIZE
          READ (NSCR) IQUANT
          K = K + 1
          L = 0
          DO 410 I = 1, NQ
              L = L + IQUANT(I)
              LQUANT(I,K) = IQUANT(I)
410       CONTINUE   
420   CONTINUE
      
!-----------------------------------------------------------------------------------------------
!     Order Vibrational states by symmetry, write to temporary array
!-----------------------------------------------------------------------------------------------
!      K = 0
!      MAXTOT = 0
!      DO 430 M = 1, NSYMSP
!         REWIND (NSCR)
!
!         DO 420 N = 1, NSIZE
!            READ (NSCR) IQUANT
!----          Define the symmetry type of the vibrational state
!            ITOT = ISYMSTAT (NQ,IQUANT,ISYMQ)
!            IF (ITOT .NE. M) CYCLE
!
!            K = K + 1
!            L = 0
!            DO 410 I = 1, NQ
!               L = L + IQUANT(I)
!               LQUANT(I,K) = IQUANT(I)
!  410       CONTINUE
!
!            MAXTOT = MAX (MAXTOT, L)
!  420    CONTINUE
!  430 CONTINUE

!-----------------------------------------------------------------------------------------------
!     Print generated vibrational states
!-----------------------------------------------------------------------------------------------
      IF (IOUT .EQ. 0) GO TO 900

      IF (NSIZE .GT. 0) WRITE (NOUT,1100) NSIZE

      ALLOCATE (XSTID(NSIZE))
      DO 520 K = 1, NSIZE
         IF (K .GT. LIMIT) THEN
            WRITE (NOUT,1200) LIMIT
            EXIT
         ENDIF

         N = 0
         DO 510 I = 1, NQ
            IF (LQUANT(I,K) .EQ. 0) CYCLE
            N = N + 1
            IX(N) = I
            IQ(N) = LQUANT(I,K)
  510    CONTINUE
         N = MIN (6,N)

!----       Char*8 per mode: " 1*( 1), 1*( 2), 1*( 3)";
!----       If N > 6, extra states printout is simply cut

         WRITE (FMT = "(6(I1,'*(',I2,')':', '))",                         &
     &      UNIT = XSTID(K), IOSTAT = IOS) (IQ(I),IX(I),I=1,N)
         IF (N .GT. 6) XSTID(48:48) = '>'

         WRITE (NOUT,1120) K,XSTID(K)
  520 CONTINUE
      DEALLOCATE (XSTID)

  900 RETURN

 1100 FORMAT (/'Size of the block = ',I6,                                 &
     &   '.  The list of states satisfying this polyad number:')
 1120 FORMAT (I4,':   ',A48)
 1200 FORMAT ('...'/'Output truncated, exceeding the limit of',I6)
    END


!-----------------------------------------------------------------------------------------------
      SUBROUTINE ENERHAOS (NQ,W,IQUANT,EZERO,ESTATE)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      REAL*16 W(NQ)
      REAL*16 EZERO, ESTATE
      INTEGER*4 IQUANT(NQ)
      
      EZERO = 0.0D0
      SM = 0.0D0
      DO 100 R = 1, NQ
         SM = SM + W(R) / 2.0D0
  100 CONTINUE
      EZERO = SM  !  Includes ground state energy      
      
      ESTATE = 0.0D0
      DO 110 I = 1, NQ
         ESTATE = ESTATE + W(I) * (REAL (IQUANT(I)) + 0.5D0)
  110 CONTINUE

      RETURN
    END

!-----------------------------------------------------------------------------------------------
!     Calculate Harmonic Energy of molecule with Degenerate modes.
!     NQEX, NQIN    - Number of External and Internal Quantum Number (V, L)
!     W(NQEX)       - Harmonic Frequencies for External Q.N.
!     IQUANT(NQEX)  - External Q.N. of Reference State
!-----------------------------------------------------------------------------------------------    
      SUBROUTINE ENERHARMDEG (NQEX, NQIN, W,IQUANT,EZERO,ESTATE)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      REAL*16 W(NQEX)
      REAL*16 EZERO, ESTATE
      INTEGER*4 IQUANT(NQEX), NQEX, NQIN
      
      EZERO = 0.0D0
      SM = 0.0D0
      DO 100 R = 1, NQEX - NQIN
         SM = SM + W(R) / 2.0D0
100   CONTINUE
      DO 110 R = 1, NQIN
         SM = SM + W(NQEX - NQIN + R)
110   CONTINUE          
      EZERO = SM  !  Includes ground state energy      
      
      ESTATE = 0.0D0
      DO 120 I = 1, NQEX - NQIN
         ESTATE = ESTATE + W(I) * (REAL (IQUANT(I)) + 0.5D0)
120   CONTINUE
      DO 130 I = 1, NQIN
         ESTATE = ESTATE + W(NQEX - NQIN + I) * (REAL (IQUANT(NQEX - NQIN + I)) + 1.0D0)
130   CONTINUE
      
      RETURN
    END    
!-----------------------------------------------------------------------------------------------
!
!     Calculate harmonic anharmonic frequency using constants x(r,s) and
!     search for a proper eigenvector.
!     E/(hc)[1/Cm] = Y(0,0) + SUM[r=1,M] Omega(r)*(v(r)+1/2) +
!     + SUM[r=1,M;s=1,r] X(r,s)*(v(r)+1/2)*(v(s)+1/2).
!
!     NOTE(s):
!     1. Similar routine DPENER () exists.
!-----------------------------------------------------------------------------------------------
!      SUBROUTINE VIBRENER (MDIM,NQ,IQUANT,WAVNUM,XCON,                    &
!     &   EZERO,EHARM,EGROU,EVPT2)
!      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
!      INTEGER*4 IQUANT(NQ)
!      REAL*16 WAVNUM(NQ),XCON(MDIM,NQ)
!      INTEGER*4 R,S
!      REAL*16 QUANT(NQ)
!
!      SM = 0.0D0
!      DO 100 R = 1, NQ
!         SM = SM + WAVNUM(R) / 2.0D0
!  100 CONTINUE
!      EZERO = SM  !  Includes ground state energy
!
!      SM = 0.0D0
!      DO 110 R = 1, NQ
!         QUANT(R) = REAL (IQUANT(R)) + 0.5D0
!         SM = SM + WAVNUM(R) * QUANT(R)
!  110 CONTINUE
!      EHARM = SM  !  Includes ground state energy
!
!      SM = 0.0D0
!      DO 200 R = 1, NQ
!         SM = SM + WAVNUM(R) * 0.5D0
!      DO 200 S = 1, R
!         SM = SM + XCON(R,S) * 0.25D0
!  200 CONTINUE
!      EGROU = SM  !  Identical for all states
!
!      SM = 0.0D0
!      DO 210 R = 1, NQ
!         SM = SM + WAVNUM(R) * QUANT(R)
!      DO 210 S = 1, R
!         SM = SM + XCON(R,S) * QUANT(R) * QUANT(S)
!  210 CONTINUE
!      EVPT2 = SM  !  Includes ground state energy
!
!      RETURN
!      END

!-----------------------------------------------------------------------------------------------
!     Function: 'LADDCOEF'    FIRST: 07 Apr 2018  LAST EDIT: 21 Apr 2020
!
!     PURPOSE:
!     Initialize CLADD(m,n,ket) for fast evaluation of matrix elements
!     for the system of H.O. orthonormal basis functions:
!     CLADD(m,n,ket) = Coefficient ( a(+)^m * a(-)^n |ket> )
!
!     THEORY:
!     Matrix element for harmonic oscillator wave function basis set:
!
!     a^m * b^n | ket > = COEFLAD(m,n,ket) | ket(m-n) >
!
!     when preliminary conditions are obeyed:
!     (1) bra - ket = m - n,
!     (2) ket >= n.
!
!     using the rules for creation/annihilation operators:
!
!     a  | psi(v) > = (v + 1)^(1/2) | psi(v+1) >,
!     a* | psi(v) > = (v)^(1/2)     | psi(v-1) >.
!
!     CALLED: MAINVSCF ().
!
!     OUTPUT:
!     COEFLAD -- Array of coefficients, /COMMON/.
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------------------------------
      SUBROUTINE LADDCOEF
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /LADCOE/ COEFLADD
      REAL*16 COEFLADD(0:63,0:51)  !  COEFLADD(0:7,0:7,0:31)
      REAL*16 COEF,ARGU

      DO 200 IRIS = 0, 7
      DO 200 ILOW = 0, 7
         IND = 8 * IRIS + ILOW

      DO 200 KET = 0, 51
         IF (KET .LT. ILOW) THEN
            COEFLADD(IND,KET) = 0.0D0
            CYCLE
         ENDIF

         COEF = 1.0D0
         IQUANT = KET

!----       Lowering: a(-) | psi(v) > = (v)^(1/2) | psi(v-1) >

         DO 110 I = 1, ILOW
            ARGU = REAL (IQUANT)
            COEF = COEF * SQRT (ARGU)
            IQUANT = IQUANT - 1
  110    CONTINUE

!----       Rising: a(+) | psi(v) > = (v + 1)^(1/2) | psi(v+1) >

         DO 120 I = 1, IRIS
            ARGU = REAL (IQUANT + 1)
            COEF = COEF * SQRT (ARGU)
            IQUANT = IQUANT + 1
  120    CONTINUE

         COEFLADD(IND,KET) = COEF
  200 CONTINUE

      RETURN
    END

!-----------------------------------------------------------------------------------------------
!     Subroutine: 'HOSELE'    FIRST: 09 Feb 2020  LAST EDIT: 10 Feb 2020
!
!     PURPOSE:
!     Evaluate matrix element of the primitive Hamiltonian term in
!     the VSCF wave functions.
!
!     THEORY:
!
!         tot    M    (bra)        M    (ket)
!     E = SUM < PRO Psi    | H  | PRO Psi    > .
!          s    j=1    j      s   k=1    k
!
!
!     CALLED: MAINRSPT (*.for)
!
!     INPUT:
!     NQ     -- The number of vibrational degrees of freedom;
!     NTHAM  -- The number of terms in the Hamiltonian operator
!               polynomial;
!     HCOEF  -- 1D Array of numerical coefficients of the polynomial;
!     LADHAM -- Array [NQ,NOP,NTHAM] of powers of individual operators;
!               IMPORTANT: Quantum numbers are increased by 1:--
!     IOPBRA -- Array [1..NQ] of quantum number of < bra_j | operator;
!     IOPKET -- Array [1..NQ] of quantum number of < ket_j | operator;
!
!     OUTPUT:
!     HOSELE -- The value of the matrix element SUM(s) <bra| H(s) |ket>.
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------------------------------
      FUNCTION HOSELE (NQ,NTHAM,HCOEF,LADHAM,IOPBRA,IOPKET)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  LOUT,NLOG,NINP,NOUT,NAUX,NSCR
      COMMON /LADCOE/ COEFLADD
      DIMENSION COEFLADD(0:63,0:51)  !  COEFLADD(0:7,0:7,0:31)

      INTEGER*1 LADHAM(NQ,2,NTHAM)   !  2 >> NOPER
      DIMENSION HCOEF(NTHAM),IOPBRA(NQ),IOPKET(NQ)

      INTEGER*4 S,Q
      DATA IOPRIS /1/, IOPLOW /2/
      DATA ZERO /0.0D0/, ONE /1.0D0/

      ELEMAT = ZERO
      DO 200 S = 1, NTHAM
         PROD = ONE

         DO 100 Q = 1, NQ
            LADRIS = LADHAM(Q,IOPRIS,S)
            LADLOW = LADHAM(Q,IOPLOW,S)
            LADIND = 8 * LADRIS + LADLOW

            IF (IOPKET(Q) .LT. LADLOW) THEN
               PROD = ZERO
               EXIT
            ENDIF

            IF (LADRIS - LADLOW .EQ. IOPBRA(Q) - IOPKET(Q)) THEN
               PROD = PROD * COEFLADD(LADIND, IOPKET(Q) - 1)
            ELSE
               PROD = ZERO
               EXIT
            ENDIF
  100    CONTINUE

         IF (PROD .NE. ZERO) ELEMAT = ELEMAT + PROD * HCOEF(S)
  200 CONTINUE

      HOSELE = ELEMAT

      RETURN
    END
    
!-----------------------------------------------------------------------------------------------
!     Subroutine: 'LZELE'    FIRST: 20 Jul 2021  LAST EDIT: 20 Jul 2021
!
!     PURPOSE:
!     Evaluate matrix element of the Angular Momentum of Z Component term in
!     the Harmonic oscillator wave functions.
!
!     THEORY:
!
!           tot    M    (bra)          M    (ket)
!     L_z = SUM < PRO Psi    | L_z  | PRO Psi    > .
!            s    j=1    j      s     k=1    k
!
!
!     CALLED: RSPT_L (*.f90)
!
!     INPUT:
!     NQ     -- The number of vibrational degrees of freedom;
!     NTLZ   -- The number of terms in the Lz operator
!               polynomial;
!     LCOEF  -- 1D Array of numerical coefficients of the polynomial, COMPLEX*8
!     LADLZ  -- Array [NQ,NOP,NTLZ] of powers of individual operators;
!               IMPORTANT: Quantum numbers are increased by 1:--
!     IOPBRA -- Array [1..NQ] of quantum number of < bra_j | operator;
!     IOPKET -- Array [1..NQ] of quantum number of < ket_j | operator;
!
!     OUTPUT:
!     LZELE -- The value of the matrix element SUM(s) <bra| H(s) |ket>.
!
!     NOTE(S):
!     1.
!-----------------------------------------------------------------------------------------------
      FUNCTION LZELE (NQ,NTLZ,LCOEF,LADLZ,IOPBRA,IOPKET)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
      COMMON /IOLUN/  LOUT,NLOG,NINP,NOUT,NAUX,NSCR
      COMMON /LADCOE/ COEFLADD
      DIMENSION COEFLADD(0:63,0:51)  !  COEFLADD(0:7,0:7,0:31)

      INTEGER*1 LADLZ(NQ,2,NTLZ)   !  2 >> NOPER
      DIMENSION IOPBRA(NQ),IOPKET(NQ)
      COMPLEX*16 LZELE
      COMPLEX*16 LCOEF(NTLZ)
      COMPLEX*16 ELEMAT, PROD
      
      INTEGER*4 S,Q
      DATA IOPRIS /1/, IOPLOW /2/
      DATA ZERO /0.0D0/, ONE /1.0D0/

      ELEMAT = (0.0D0, 0.0D0)
      DO S = 1, NTLZ
         PROD = ( 1.0D0, 0.0D0)
         DO Q = 1, NQ
            LADRIS = LADLZ(Q,IOPRIS,S)
            LADLOW = LADLZ(Q,IOPLOW,S)
            LADIND = 8 * LADRIS + LADLOW
            IF (IOPKET(Q) .LT. LADLOW) THEN
               PROD = ZERO
               EXIT
            ENDIF
            IF (LADRIS - LADLOW .EQ. IOPBRA(Q) - IOPKET(Q)) THEN
               PROD = PROD * CMPLX(COEFLADD(LADIND, IOPKET(Q) - 1), 0.0D0, KIND = 8)
            ELSE
               PROD = ZERO
               EXIT
            ENDIF
         ENDDO
         IF (PROD .NE. ZERO) ELEMAT = ELEMAT + PROD * LCOEF(S)
      ENDDO
            
      LZELE = ELEMAT

      RETURN
    END    
    
    
!-----------------------------------------------------------------------------------------------
!     Subroutine: 'HAOSMP'    FIRST: 09 Feb 2020  LAST EDIT: 27 Apr 2020
!
!     PURPOSE:
!     Evaluate matrix element of the primitive Hamiltonian term in
!     the VSCF wave functions.
!
!     THEORY:
!
!         tot    M    (bra)        M    (ket)
!     E = SUM < PRO Psi    | H  | PRO Psi    > .
!          s    j=1    j      s   k=1    k
!
!     CALLED: MAINRSPT (*.for)
!
!     INPUT:
!     NQ     -- The number of vibrational degrees of freedom;
!     NTHAM  -- The number of terms in the Hamiltonian operator
!               polynomial;
!     HCOEF  -- 1D Array of numerical coefficients of the polynomial;
!     LADHAM -- Array [NQ,NOP,NTHAM] of powers of individual operators;
!               IMPORTANT: Quantum numbers are increased by 1:--
!     IOPBRA -- Array [1..NQ] of quantum number of < bra_j | operator;
!     IOPKET -- Array [1..NQ] of quantum number of < ket_j | operator;
!
!     OUTPUT:
!     HOSELE -- The value of the matrix element SUM(s) <bra| H(s) |ket>.
!
!     NOTE(S):
!     (1) 27-Apr-2020
!
!-----------------------------------------------------------------------------------------------
      FUNCTION HAOSMP (NQ,NTHAM,HCOEF,LADHAM,IOPBRA,IOPKET)
      IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)
 
      INTEGER*1 LADHAM(NQ,2,NTHAM)  !  2 >> NOPER
      INTEGER*4 IOPBRA(NQ),IOPKET(NQ)
      COMMON /LADCOE/ COEFLADD
      DIMENSION COEFLADD(0:63,0:51)  !  COEFLADD(0:7,0:7,0:31)
      DIMENSION HCOEF(NTHAM)
      
!----    Local variables must be saved
      INTEGER*4 S,Q
      DATA IOPRIS /1/, IOPLOW /2/, NDIM /8/

      ELEMAT = 0.0D0
      DO S = 1, NTHAM
         PROD = 1

         DO Q = 1, NQ
            LADRIS = LADHAM(Q,IOPRIS,S)
            LADLOW = LADHAM(Q,IOPLOW,S)
            LADIND = NDIM * LADRIS + LADLOW
            IF (IOPKET(Q) .LT. LADLOW) THEN
               PROD = 0
               EXIT
            ENDIF
            IF (LADRIS - LADLOW .EQ. IOPBRA(Q) - IOPKET(Q)) THEN
               PROD = PROD * COEFLADD(LADIND, IOPKET(Q) - 1)
            ELSE
               PROD = 0
               EXIT
            ENDIF
         ENDDO
         IF (PROD .NE. 0) ELEMAT = ELEMAT + PROD * HCOEF(S)
      ENDDO
      HAOSMP = ELEMAT
      
      RETURN  
    END
    
    
!-----------------------------------------------------------------------------------------------
    SUBROUTINE MLOOPASRCSV(NVARS, VMAXS, NTOT, ARRAY2)
    IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)

    INTEGER*4 NVARS                                                           ! Number of Loops
    INTEGER*4 VMAXS(NVARS)                                                    ! Array of Maxium Values for each Loop (from 1 to VMAXS(NVARS)
    INTEGER*4 NTOT                                                            ! Total Number of Cycle  ( PRODUCT_I ( VMAXS(I) )  
    INTEGER*4 ARRAY2(NTOT,NVARS)                                              ! Target Array
    
    INTEGER*4 :: ndims,ndim
    INTEGER*4 :: i,j,k,ii
    INTEGER,ALLOCATABLE ::array(:,:)
!---    
    
    ALLOCATE (ARRAY (NTOT, NVARS))            

    ARRAY = 0
    NDIM  = 1
    DO  K = 1, NVARS                                                          !将多重循环的所有循环变量的遍历值写入2维数组中，第一维是遍历数，第二维记录对应每一遍历数的训练变量
                                                                              !一个循环变量一个循环变量的填
        IF (K .EQ. 1) THEN                                                    !第一个循环变量填入第二维第一个位置
            DO I = 1, VMAXS(NVARS)
                ARRAY(I,:) = (/(1,II = 2, NVARS), I/)
            ENDDO
            NDIM = NDIM * VMAXS(NVARS)

        ELSE IF (K .LT. NVARS) THEN                                           !接下来的循环变量，填入第二个位置，前面位置的信息采用复制信息
            DO J = 1, VMAXS(NVARS - K + 1)
                DO I = 1, NDIM
                    ARRAY((J - 1) * NDIM + I, :) = (/(1, II = K + 1,NVARS), J, ARRAY(I,NVARS - K + 2: NVARS)/)
                ENDDO
            ENDDO
            NDIM = NDIM * VMAXS(NVARS - K + 1)
        ELSE
            DO J = 1, VMAXS(1)
                DO I = 1, NDIM
                    ARRAY((J - 1) * NDIM + I, :) = (/ J, ARRAY(I,2:NVARS)/)
                ENDDO
            ENDDO
        ENDIF
    ENDDO

    ARRAY2 = ARRAY

    END SUBROUTINE 