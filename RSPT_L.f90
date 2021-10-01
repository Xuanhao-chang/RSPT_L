!****************************************************************************
!
!  PROGRAM: RSPT_L
!
!  PURPOSE:  Entry point for the console application.
!
!  RSPT_L.f90
!
!  FUNCTIONS:
!  RSPT_L - Entry point of console application.
!
!****************************************************************************

    PROGRAM RSPT_L

 !   USE MKL_SCALAPACK

    IMPLICIT REAL*16 (A-H,O-Z), INTEGER*4 (I-N)

    INTEGER*4 LOUT,NLOG,NINP,NOUT, NAUX,NCSR,NTMP
    COMMON /IOLUN/ LOUT,NLOG,NINP,NOUT, NAUX,NSCR,NTMP

!    INTEGER*4 :: NQ

    REAL*8 ::  RE, IM                                                                                !
    COMPLEX*16 ELEMAT, LZELE, CZERO, CONE, TERM, SCPRO, SUMVIR, SUMORD                                               !
    REAL   *16 MATELE, MATELEHARM, MATELEPERT                                                        !
    DIMENSION   NHORD(0:4)                                                                           !  Number of Terms in Hamiltonian (i)
    DIMENSION   COEFLADD(0:63,0:51)                                                                  !
    COMMON /LADCOE/ COEFLADD                                                                         !
    ALLOCATABLE             :: HCOEF(:), VCOEF(:), HOCOEF(:)                                         !
    COMPLEX*16, ALLOCATABLE :: LCOEF(:), LZMAT(:,:)                                                  !
    COMPLEX*16, ALLOCATABLE :: WORK(:), TRIDIAG(:,:), TAU(:), EIGVECLZ(:,:), AUX(:,:)                !
    INTEGER *8, ALLOCATABLE :: IWORK(:)                                                              !
    REAL    *8, ALLOCATABLE :: DIAG(:), RWORK(:), EIGVAL(:) , OFFDIAG(:), EIGVALLZ(:)      !
    INTEGER *1, ALLOCATABLE :: LADHAM(:,:,:), LADHO(:,:,:), LADVP(:,:,:)                             !
    INTEGER *1, ALLOCATABLE :: LADLZ (:,:,:)                                                         !
    INTEGER *4, ALLOCATABLE :: ISTATE(:)                                                             !
    INTEGER *4, ALLOCATABLE :: IQUANT(:,:), JQUANT(:,:), IEXQUANT(:,:), JEXQUANT(:,:)                !
    INTEGER *4, ALLOCATABLE :: IOPBRA(:), IOPKET(:)                                                  !
    INTEGER *4, ALLOCATABLE :: IVIRTU(:,:), NQUPOL(:)                                                !
    INTEGER *4, ALLOCATABLE :: VMAXS(:), AUX1(:,:), AUX2(:,:)                                        !
    COMPLEX*16, ALLOCATABLE :: HARM(:,:), VPERT(:,:), HARMTRAN(:,:), VPERTRAN(:,:)                   !
    COMPLEX*16, ALLOCATABLE :: PROJECT(:,:), PROJTRAN(:,:), COPY1(:,:), COPY2(:,:), HAMATRE(:,:)     !
    COMPLEX*16, ALLOCATABLE :: AUX3(:,:), AUX4(:,:)                                                  !
    REAL   *16, ALLOCATABLE :: W(:), WEX(:)                                                          !
    ALLOCATABLE KPOLY(:), KEXPOLY(:)                                                                 !
    
    INTEGER *4, ALLOCATABLE :: IASSIG(:), IDEG(:)
    REAL   *16, ALLOCATABLE :: ENERHARM(:), ENERCI(:), ENERPT(:)                                      !    
    REAL    *8, ALLOCATABLE :: ENERDEG(:)                                                             !    
    REAL   *16, ALLOCATABLE :: EPTORD(:)
    COMPLEX*16, ALLOCATABLE :: PSIFUN(:,:), VPERTDEG(:,:)
    
    INTEGER*8 LSPLIT                                         !  Overall time for all states
    INTEGER*8 LSPLIT0                                        !  Overall time for a single state
    INTEGER*8 LSPLIT1                                        !  VCI calculation for a single state
    INTEGER*8 LSPLIT2                                        !  RSPT calculation for a single state
    CHARACTER*16 TIMTXT
    
    CHARACTER*80 :: FILE_H(0:4),FILE_LZ
    CHARACTER*80 :: FILE_HTOT, FILE_POLY, FILE_POLY2, FILE_HARM, FILE_PERT
    CHARACTER*80 :: QUANID
    CHARACTER*64    PATH_CUR,PATH_PRO,PATH_OUT,PATH_BIN
    COMMON /PATHS/  PATH_CUR,PATH_PRO,PATH_OUT,PATH_BIN

    CHARACTER*112 LINEWIDE
    CHARACTER* 64 FILE_CFG,FILE_MOL,FILE_INP,FILE_LST,FILE_STATE
    CHARACTER* 64 LINE,FILE,PATH
    CHARACTER* 16 LPES(4)
    CHARACTER*  8 KEYWOR
    CHARACTER* 16 KEYWOR16
    CHARACTER* 64 COMPUTER
    CHARACTER*300 LINEMAT

    CHARACTER* 6 SDTQ(0:4)

    CHARACTER*24 DATVER, MOLECULE
    CHARACTER* 2 VERTICL

    LOGICAL      :: FLAG, STATUS
    LOGICAL      :: CONVER

    DATA NLOG,NINP,NOUT,NAUX,NSCR,NTMP /1,2,3,4,7,8/
    DATA LPES /' ', '(Quartic PES)', ' ', '(Sextic PES)'/
    DATA SDTQ /'Polyad','Single','Double','Triple','Quadru'/
    DATA VERTICL /'|'/
    DATA LOUT /2/
    DATA NOPER /2/
    DATA TOLCUT /1.0D-6/
    DATA TOLOUT /1.0D-8/   !  Convergence criterion    
    DATA LIMIT /300/
    DATA ZERO,HALF,ONE,TWO,THREE,FOUR /0.0D0,0.5D0,1.0D0,2.0D0,3.0D0,4.0D0/
    DATA CZERO, CONE /(0.0D0,0.0D0), (1.0D0,0.0D0)/

    DATVER   = 'Jul 17, 2021, Sat 9:00'
    NUMVER   = 0100
    !MOLECULE = 'C2H2'!'2DIsoHO'        !'C2H2'           !'CO2'                       ! Type of Molecule
    !NQ       = 7                    ! 4                          ! Number of Normal Mode , 4 For CO2, 7 For C2H2
    !NSAYVETZ = 0                                             ! Sayvetz Condition, L = 0, 1, 2,...
    !ALLOCATE (   W  (NQ   ))
    !W(1) = 100.0D0
    !W(2) = 100.0D0
    !W(3) = 110.0D0
    !W(4) = 110.0D0
    !W(1) = 1.0D0
    !W(2) = 1.0D0
    !W(1) = 0.1353779746D+04     ! Normal Frequencies of CO2
    !W(2) = 0.2396532220D+04     !
    !W(3) = 0.6728925740D+03     !
    !W(4) = 0.6728925740D+03     !
    !W(1) = 0.35068570D+04       ! Normal Frequencies of C2H2
    !W(2) = 0.20111680D+04       !
    !W(3) = 0.34143540D+04       !
    !W(4) = 0.62168700D+03       !
    !W(5) = 0.62168700D+03       !
    !W(6) = 0.74861800D+03       !
    !W(7) = 0.74861800D+03       !

    PATH_CUR = 'C:/PROG/RSPT_L/'
    FILE_CFG = 'RSPT_L.ini'

!-----------------------------------------------------------------------------------------------
!     Read RSPT configuration file RSPT.dat:
!     1) Look for it in Current directory, otherwise in
!     2) C:/PROG/
!
!     RSPT working directory        = C:/PROG/RSPT/
!     ANCO import data file         = RSPT_H2O_MP2_cc-pVTZ.mol
!     RSPT input parameters file    = RSPT_H2O_MP2_cc-pVTZ.inp
!-----------------------------------------------------------------------------------------------

     OPEN (UNIT = NAUX, ACTION = 'READ', FORM = 'FORMATTED', FILE = FILE_CFG, ERR = 1000)
     GO TO 1010
     
1000 CONTINUE
     FILE_CFG = TRIM(PATH_CUR) // TRIM(FILE_CFG)
     WRITE (*,1020) FILE_CFG
     OPEN (UNIT = NAUX, ACTION = 'READ', FORM = 'FORMATTED', FILE = FILE_CFG, ERR = 910)
1010 CONTINUE

1020 FORMAT (/'RSPTFM: Configuration file RSPT.dat not found in ',       &
     &   'current directory,'/8X,'attempting alternative directory: ',A)

      DO WHILE (.NOT. EOF (NAUX))
         READ (NAUX,"(A16,32X,A64)") KEYWOR16,LINE
         IF (KEYWOR16(1:1) .EQ. '!' .OR. KEYWOR16 .EQ. ' ') CYCLE
         IF (KEYWOR16(1:10) .EQ. '# COMPUTER') COMPUTER = LINE                  !  Computer specifications

         IF (KEYWOR16(1:10) .EQ. '# PATH_CUR') PATH_CUR = LINE                  !  Read RSPT working directory
         IF (KEYWOR16(1:10) .EQ. '# PATH_OUT') PATH_OUT = LINE                  !  Taylor series output .dat directory
         IF (KEYWOR16(1:10) .EQ. '# PATH_BIN') PATH_BIN = LINE                  !  Working directory for binary data (operators), .bin

         IF (KEYWOR16(1:10) .EQ. '# PARA_MOL') FILE_MOL = LINE                 !  ANCO import data path for *.mol
         IF (KEYWOR16(1:10) .EQ. '# RSPT_INP') FILE_INP = LINE                  !  RSPT input parameters file, RSPT_mol_MP2_cc-pVTZ.inp
         IF (KEYWOR16(1:10) .EQ. '# RSPT_LST') FILE_LST = LINE                  !  RSPT results output file, RSPT_CO2.txt

         IF (KEYWOR16(1:10) .EQ. '# LIST_LVL')                            &
     &      READ (UNIT = LINE, FMT = "(I4)", IOSTAT = IOS) LOUT                 !  Output level (0 = min, 1 = regular, 2 = full)
         IF (KEYWOR16(1:10) .EQ. '# ACCURACY')                            &
     &      READ (UNIT = LINE, FMT = "(I4)", IOSTAT = IOS) NACCURMP            !  FM accuracy
      ENDDO
      REWIND (NAUX)

      OPEN (UNIT = NOUT, ACTION = 'WRITE', FORM = 'FORMATTED', FILE = FILE_LST, ERR = 930)

      WRITE (NOUT,1030) FILE_CFG
 1030 FORMAT (/'[RSPT_L]'//'Program configuration file: ',A64/112('-'))
      DO WHILE (.NOT. EOF (NAUX))
         READ  (NAUX,"(A)") LINEWIDE
         WRITE (NOUT,"(A)") LINEWIDE
      END DO
      CLOSE (NAUX)
      WRITE (NOUT,"(112('-'))")
!-----------------------------------------------------------------------------------------------
!     Read parameters in .mol file
!-----------------------------------------------------------------------------------------------
      FILE_MOL = TRIM(PATH_CUR) // TRIM(FILE_MOL)
      OPEN (UNIT = NAUX, ACTION = 'READ', FORM = 'FORMATTED', FILE = FILE_MOL, ERR = 920)

      DO WHILE (.NOT. EOF(NAUX))
          READ (NAUX,1200) KEYWOR, LINE
          IF (KEYWOR(1:1) .EQ. '!') CYCLE
          IF (KEYWOR .EQ. 'MOLE_NAM')      THEN
              MOLECULE = TRIM(LINE)
          ELSE IF (KEYWOR .EQ. 'SAYV_CON') THEN
              READ (UNIT = LINE, FMT = '(I4)') NSAYVETZ
          ELSE IF (KEYWOR .EQ. 'NORM_MOD') THEN
              READ (UNIT = LINE, FMT = '(I4)') NQ
              ALLOCATE(W(NQ))
              DO I = 1, NQ
                  READ (NAUX, '(8X,F24.16)') W(I)
              ENDDO
          ELSE IF (KEYWOR .EQ. 'END_FILE') THEN
              EXIT
          ENDIF    
      ENDDO      
      CLOSE (NAUX)      
!-----------------------------------------------------------------------------------------------
!     Check for existance of the input file:
!-----------------------------------------------------------------------------------------------
      FILE_INP = TRIM(PATH_CUR) // TRIM(FILE_INP)
      INQUIRE (FILE = FILE_INP, EXIST = STATUS)
      IF (STATUS) THEN
         OPEN (UNIT = NINP, ACTION = 'READ', FORM = 'FORMATTED', FILE = FILE_INP, ERR = 940)
      ELSE
         WRITE (*,1100) FILE_INP
         PAUSE 'RSPT: Error -- acknowledge before STOP: <ENTER>'
         STOP
      ENDIF

1100  FORMAT (/'Input data file: ',A/'-- not found, termination.')
!-----------------------------------------------------------------------------------------------
!     Input control parameters:
!     LUN = NINP can be available or not available
!-----------------------------------------------------------------------------------------------
      ALLOCATE ( KPOLY(NQ) )
      TOLCUT  = 1.0D-06  !  Tolerance for CVPT operator terms, 1/Cm
      TOLOUT  = 1.0D-16  !  RSPT convergence criterion
      MPES    =       2  !  Quartic or Sextic force field, 2/4
      MAX_PT  =     210  !  Maximum order of RSPT
      MAXVRT  =      10  !  Maximum mode excitation of virtual states
      MULMOD  =       3  !  Maximum modes involved in virtual states
      ENERMAX = 20000.0  !  Energy limit for a virtual state
      EFREMAX =  1000.0  !  Energy limit for reference states 
      NOPTST  =       4  !  Maximum total quanta of excitation
      NSTART  =       0  !  Maximum total quanta of excitation
      MAXPOL  =       0  !  Polyad structure is undefined
!----    IF (MAXQUA .EQ. 0) MAXQUA = MAXPOL
      MAXQUA  =       0  !  Total quanta of excitation in a polyad
      KPOLY   =       0  !  Polyad coefficients initialization (Array)
      NQEX = 0           !  The Number of External(NQEX, v_i) Quantum Number  
      NQIN = 0           !  The Number of Internal(NQIN, l_i) Quantum Number  

      INQUIRE (UNIT = NINP, OPENED = STATUS)
      IF (STATUS) THEN
         DO WHILE (.NOT. EOF (NINP))
            READ (NINP,1200) KEYWOR,LINE
            IF (KEYWOR(1:1) .EQ. '!' .OR. KEYWOR .EQ. ' ') CYCLE
            IF (KEYWOR(1:4) .EQ. 'PARA') EXIT                                   !  READ: 'PARA'
         END DO
         DO WHILE (.NOT. EOF (NINP))
            READ (NINP,1200) KEYWOR,LINE
            IF (KEYWOR(1:1) .EQ. '!' .OR. KEYWOR .EQ. ' ') CYCLE
            IF      (KEYWOR .EQ. 'CVPT_TOL') THEN
               READ (UNIT = LINE, FMT = "(F8.0)", IOSTAT = IOS) TOLCUT
            ELSE IF (KEYWOR .EQ. 'CONV_TOL') THEN
               READ (UNIT = LINE, FMT = "(F8.0)", IOSTAT = IOS) TOLOUT
            ELSE IF (KEYWOR .EQ. 'FORC_ORD') THEN
               READ (UNIT = LINE, FMT = "(I4)", IOSTAT = IOS) MPES
            ELSE IF (KEYWOR .EQ. 'STAT_OPT') THEN
               READ (UNIT = LINE, FMT = "(I4)", IOSTAT = IOS) NOPTST
            ELSE IF (KEYWOR .EQ. 'STAT_BEG') THEN
               READ (UNIT = LINE, FMT = "(I4)", IOSTAT = IOS) NSTART
            ELSE IF (KEYWOR .EQ. 'RSPT_MAX') THEN
               READ (UNIT = LINE, FMT = "(I4)", IOSTAT = IOS) MAX_PT
            ELSE IF (KEYWOR .EQ. 'VIRT_MAX') THEN
               READ (UNIT = LINE, FMT = "(I4)", IOSTAT = IOS) MAXVRT
            ELSE IF (KEYWOR .EQ. 'MULT_MOD') THEN
               READ (UNIT = LINE, FMT = "(I4)", IOSTAT = IOS) MULMOD
            ELSE IF (KEYWOR .EQ. 'ENER_MAX') THEN
               READ (UNIT = LINE, FMT = "(F8.0)", IOSTAT = IOS) ENERMAX
            ELSE IF (KEYWOR .EQ. 'EREF_MAX') THEN
               READ (UNIT = LINE, FMT = "(F8.0)", IOSTAT = IOS) EREFMAX   
            ELSE IF (KEYWOR .EQ. 'EXTR_QUA') THEN
               READ (UNIT = LINE, FMT = "(I4)", IOSTAT = IOS) NQEX
               ALLOCATE( KEXPOLY( NQEX ) )
            ELSE IF (KEYWOR .EQ. 'INTR_QUA') THEN
               READ (UNIT = LINE, FMT = "(I4)", IOSTAT = IOS) NQIN
            ELSE IF (KEYWOR .EQ. 'POLY_MAX') THEN
               READ (UNIT = LINE, FMT = "(I4)", IOSTAT = IOS) MAXPOL
               IF (MAXPOL .GT. 0) THEN
                   READ (NINP,"(15I4)") (KPOLY  (I),I=1,NQ)
                   READ (NINP,"(15I4)") (KEXPOLY(I),I=1,NQEX)
               ENDIF
            ELSE IF (KEYWOR .EQ. 'POLY_QUA') THEN
               READ (UNIT = LINE, FMT = "(I4)", IOSTAT = IOS) MAXQUA
            ENDIF
 !           IF (KEYWOR(1:4) .EQ. 'VIBR' .OR.                              &
 !    &          KEYWOR(1:4) .EQ. 'DATA') THEN
 !              BACKSPACE (NINP)
 !              EXIT
 !           ENDIF
 !           IF (KEYWOR(1:4) .EQ. 'TERM') THEN  !  Carry on reading input
 !              EXIT
 !           ENDIF
            IF (KEYWOR(1:4) .EQ. 'STOP') THEN  !  Close input file
               CLOSE (NINP)
               EXIT
            ENDIF
         END DO
      ELSE
         WRITE (*,1210)
      ENDIF
!----    LUN = NINP stopped at STOP/EXIT or before VIBR/DATA.

 1200 FORMAT (A8,3X,A64)                                                        !  1400
 1210 FORMAT (/'RSPT: Input not found, using default parameters.')

      MULMOD = MIN (NQ,MULMOD)                                                  !!  Attention should be paid in MIN

!----------------------------------------------------------------------------
      WRITE (NOUT,1300) REAL(NUMVER)/100.0,DATVER,MOLECULE
      CALL TIMDAT (NOUT)
      WRITE (*,1300) REAL(NUMVER)/100.0,DATVER,MOLECULE
      CALL TIMDAT (0)

!      INTENS  = IOPT(3)  !  >>>
!      ISCATT  = IOPT(4)  !  >>>

1300    FORMAT (/72('=')/                                                   &
     &   'Large-Order Vibrational Rayleigh-Schroedinger Perturbation ',   &
     &   'Theory'/                                                        &
     &   'for expanding energies and wave functions into Taylor ',        &
     &   'series: Linear molecule'/72('=')//                         &
     &   'Version : ',F6.2/                                               &
     &   'Date    : ',A24/                                                &
     &   'Molecule: ',A64/)

!----    Initialize filenames for different orders of the Hamiltonian
      NHORD = 0  !  Vector assignment
      DO N = 0, MPES
         WRITE (FMT = "('HAMILT_',A,'_H',I1,'.txt')", UNIT = FILE) TRIM(MOLECULE), N
         FILE_H(N) = TRIM(PATH_BIN)//TRIM(FILE)
      END DO
      WRITE (NOUT,1310)  !  Operators of Creation/Annihilation info.
      WRITE (NOUT,1320)  !  Operators of Creation/Annihilation info.

      WRITE (FMT = "('H_TOTAL.BIN')", UNIT = FILE)
      FILE_HTOT  =  TRIM(PATH_BIN)// TRIM(FILE)
1310  FORMAT (/'Second Quantization Hamiltonian will be input from ',     &
     &   'Wolfram Mathematica output File'                          )
1320  FORMAT (/'Express Hamiltonian in Creation/Annihilation ',           &
     &   'operators:'//                                                   &
     &   '(a) Creation     of Quantum: a(k) = (q(k)-i*p(k))/2^(1/2)'/     &
     &   '(b) Annihilation of Quantum: b(k) = (q(k)+i*p(k))/2^(1/2)')


      DO I = 0, MPES
        OPEN(UNIT=NINP, FILE=FILE_H(I), FORM = "FORMATTED", ACTION = "READ")
        NTERH = 0
        DO WHILE (.NOT. EOF(NINP))
            READ(NINP, "(I4,2X,F15.8)", IOSTAT = IOS) N, COEF
            NTERH = NTERH + 1
        END DO
        NHORD(I) = NTERH
      ENDDO
      NTHAM    = NHORD(0) + NHORD(1) + NHORD(2) + NHORD(3) + NHORD(4)
      NHCOR    = 0
      WRITE (NOUT,1330) (NHORD(I),I=0,2),NHCOR,(NHORD(I),I=3,4)
1330  FORMAT (/'The number of operator terms in the Hamiltonian:--'/      &
     &   'Zero   Order (Harmonic) = ',I8/                                 &
     &   'First  Order (Cubic)    = ',I8/                                 &
     &   'Second Order (Quartic)  = ',I8/                                 &
     &   'Second Order (Coriolis) = ',I8/                                 &
     &   'Third  Order (Quintic)  = ',I8/                                 &
     &   'Fourth Order (Sextic)   = ',I8)

!!---------------------------------------------------------------------------!!
!                                                                            !!
!!----    Assemble all Orders of the Hamiltonian in FILE_HTOT[1..NTHAM]      !!
!      CALL COPYPOL(NQ, NHORD(0), FILE_H(0), FILE_HTOT)                      !!
!!----    NOTE: Routine ADDPOLF also collects like terms                     !!
!      CALL ADDPOLF(NQ, NTHAM, FILE_HTOT, TOLCUT, NTHAM, FILE_HTOT, &        !!
!        & ONE, NHORD(1), FILE_H(1), ONE)                                    !!
!      CALL ADDPOLF(NQ, NTHAM, FILE_HTOT, TOLCUT, NTHAM, FILE_HTOT, &        !!
!        & ONE, NHORD(2), FILE_H(2), ONE)                                    !!
!!----    Append Coriolis Term           ! It will be done later             !!
!!----    Append Sextic Terms            ! It will be done later             !!
!      WRITE (NOUT,1340) MPES,NTHAM                                          !!
!1340  FORMAT (/80('-')//                                                  & !!
!     &   'The Vibrational Watson Hamiltonian (J = 0) is composed:'//      & !!
!     &   'The order of potential function =',I4/                          & !!
!     &   'Total number of primitive terms =',I12)                           !!
!                                                                            !!
!                                                                            !!
!!----    Allocate Memory and Upload the Hamiltonian                         !!
!      ALLOCATE (HCOEF(NTHAM),LADHAM(NQ,NOPER,NTHAM), STAT = IERR)           !!
!      LOCERR = 2;  IF (IERR .NE. 0) GO TO 990                               !!
!                                                                            !!
!                                                                            !!
!      CALL UPLOAD (NQ,NTHAM,FILE_HTOT,HCOEF,LADHAM,TOLCUT,LIMIT,0)          !!    
!!----    Arrange operator terms in the descending order of coefficients     !!
!      CALL SORTPOL (NQ,NOPER,NTHAM,HCOEF,LADHAM,TOLCUT,LIMIT,IERR)          !!    
!      IF (IERR .GT. 0) STOP '[SORTPOL] Returned error code.'                !!
!      CALL OPEROUT (NQ,NOPER,NTHAM,HCOEF,LADHAM,TOLCUT,LIMIT)               !!    
!----------------------------------------------------------------------------!!     

!----    Zero-order Hamiltonian
      ALLOCATE (HOCOEF(NHORD(0)), LADHO(NQ,NOPER,NHORD(0)), STAT = IERR)
      LOCERR = 14;  IF (IERR .NE. 0) GO TO 990

      FILE_HARM = TRIM(PATH_BIN) // 'H_HARM.BIN'
      OPEN (UNIT = NAUX, FILE = FILE_HARM, STATUS = 'REPLACE',            &
     &   ACTION = 'WRITE', BUFFERED = 'YES', FORM = 'UNFORMATTED')
      
      CALL COPYPOL(NQ, NHORD(0), FILE_H(0), FILE_HARM)

!----    UPLOAD () calls OPEROUT () if IOUT = 1:
      CALL UPLOAD  (NQ,NHORD(0),FILE_HARM,HOCOEF,LADHO,TOLCUT,LIMIT,1)             !  UPLOAD ()
      CALL SORTPOL (NQ,NOPER,NHORD(0),HOCOEF,LADHO,TOLCUT,LIMIT,IERR)              !  SORTPOL ()
      IF (IERR .GT. 0) STOP '[ARRANGE] Returned error code.'


!     WRITE(NOUT, ) 
!----    Pertubated Hamiltonian 
      NPERT = NHORD(1) + NHORD(2)                                                  ! + NHORD(3) + NHORD(4)
      ALLOCATE (VCOEF(NPERT), LADVP(NQ,NOPER,NPERT), STAT = IERR)
      LOCERR = 15;  IF (IERR .NE. 0) GO TO 990

      FILE_PERT = TRIM(PATH_BIN) // 'H_PERT.BIN'
      OPEN (UNIT = NAUX, FILE = FILE_PERT, STATUS = 'REPLACE',            &
     &   ACTION = 'WRITE', BUFFERED = 'YES', FORM = 'UNFORMATTED')
!----------------------------------------------------------------------------!!      
!      CALL COPYPOL(NQ, NTHAN, FILE_HTOT, FILE_PERT)                         !!   
!      CALL ADDPOLF(NQ, NPERT, FILE_PERT, TOLCUT,                          & !!
!     &   NTHAM, FILE_HTOT, ONE, NHORD(0), FILE_H(0), -ONE)                  !!
!----------------------------------------------------------------------------!!
     CALL COPYPOL(NQ, NHORD(1), FILE_H(1), FILE_PERT)      
     CALL ADDPOLF(NQ, NPERT, FILE_PERT, TOLCUT,                          &
     &   NHORD(1), FILE_PERT, ONE, NHORD(2), FILE_H(2), ONE)

!----    UPLOAD () calls OPEROUT () if IOUT = 1:
      CALL UPLOAD  (NQ,NPERT,FILE_PERT, VCOEF,LADVP,TOLCUT,LIMIT,1)             !  UPLOAD ()
      CALL SORTPOL (NQ,NOPER,NERPT,VCOEF,LADVP,TOLCUT,LIMIT,IERR)              !  SORTPOL ()
      IF (IERR .GT. 0) STOP '[ARRANGE] Returned error code.'
     
!-------------------------------------------------------------------------------
!     Prepare Angular Momentum L(z)
!     Read Lz file from Mathematica Output File
!-------------------------------------------------------------------------------
      WRITE (FMT = "('Lz_',A,'.txt')", UNIT = FILE) TRIM(MOLECULE)
      FILE_LZ    =  TRIM(PATH_BIN)// TRIM(FILE)
      OPEN(UNIT=NINP, FILE=FILE_LZ, FORM = "FORMATTED", ACTION = "READ")
      NTERL = 0
      DO WHILE (.NOT. EOF(NINP))
          READ(NINP, "(I4,2(1X,F32.16))", IOSTAT = IOS) N, RE, IM
          NTERL = NTERL + 1
      END DO
      REWIND(NINP)

      NTLZ = NTERL
      ALLOCATE(LCOEF (NTLZ     ),LADLZ(NQ,2,NTLZ ))
      NTERL = 1
      DO WHILE (.NOT. EOF(NINP))
          READ(NINP, "(I4,2(1X,F32.16),20(1X,I1))", IOSTAT = IOS) N, RE, IM, &
              & ((LADLZ(Q,J,NTERL),J=1,NOPER),Q=1,NQ)
          LCOEF(NTERL) = CMPLX(RE,IM, KIND = 8)
          NTERL        = NTERL + 1
      ENDDO
!-----------------------------------------------------------------------------------------------
!     Create/read specifications of vibrational states of interest:             !  2000
!     IQUANT(vector, j = 0..NSTATE)
!     j = 0: ground state
!     j > 0: j(polyad), polyad = 1..MAXPOL
!-----------------------------------------------------------------------------------------------
      IF (NOPTST .EQ. 0 .AND. MAXPOL .GT. 0) THEN
         FILE_POLY = TRIM (PATH_BIN) // 'POLYAD.BIN'
         OPEN (UNIT = NSCR, FILE = FILE_POLY, DISPOSE = 'DELETE',         &
     &      ACTION = 'READWRITE', FORM = 'UNFORMATTED')

         IF (MAXQUA .EQ. 0) MAXQUA = MAXPOL

         WRITE (NOUT,1410) MAXPOL,MAXQUA,(KPOLY(I),I=1,NQ)
         WRITE (NOUT,1420)

         NOPTST = 0
         NSTATE = 0
         MSIZE = 0  !  Maximum size of polyad blocks
         DO 250 MODE = 0, 1
            K = 0
            DO 240 NPOLY = 1, MAXPOL
!----             Create states for given NPOLY and save to file NSCR
               IOUT = 0  !  = MODE
                CALL PSTATE (NQ,W,KPOLY,NPOLY,MAXQUA,MODE,          &  !ISYMSP,
     &            NSIZE,JQUANT,MAXTOT,LIMIT,IOUT)
               IF (NSIZE .EQ. 0) CYCLE

               IF (MODE .EQ. 0) THEN
                  MSIZE = MAX (MSIZE, NSIZE)
                  NSTATE = NSTATE + NSIZE
                  CYCLE
               ENDIF

               DO 230 I = 1, NSIZE
                  K = K + 1
                  DO Q = 1, NQ
                     IQUANT(Q,K) = JQUANT(Q,I)
                  END DO
                  !ISP = ISYMSTAT (NQ,IQUANT(1,K),ISYMSP)
                  !CALL VIBRENER (MDIM,NQ,IQUANT(1,K),W,XCON,EZERO,EHARM,EGROU,EVPT2)
                  CALL ENERHAOS (NQ,W,IQUANT(1,K),EZERO,EHARM)
                  WRITE (UNIT = QUANID, FMT = "('(',24(I2:','))") (IQUANT(Q,K),Q=1,NQ)
                  QUANID = TRIM(QUANID) // ')'
                  WRITE (NOUT,1430) K,NPOLY,I,        &                         !LSYMSP(ISYMSP(ISP)),
     &               EHARM-EZERO,QUANID
  230          CONTINUE
               WRITE (NOUT,1440)
  240       CONTINUE

            IF (MODE .EQ. 0) THEN
               ALLOCATE (IQUANT(NQ,0:NSTATE),JQUANT(NQ,0:MSIZE))
               JQUANT = 0
               IQUANT = 0  !  Ground state is initialized here
               K = 0
               I = 1
               NPOLY = 0
               !ISP = ISYMSTAT (NQ,IQUANT(1,0),ISYMSP)
               !CALL VIBRENER (MDIM,NQ,IQUANT(1,K),W,XCON,EZERO,EHARM,EGROU,EVPT2)
               CALL ENERHAOS (NQ,W,IQUANT(1,K),EZERO,EHARM)
               WRITE (UNIT = QUANID, FMT = "('(',24(I2:','))") (IQUANT(Q,K),Q=1,NQ)
               QUANID = TRIM(QUANID) // ')'
               WRITE (NOUT,1430) K,NPOLY,I,           &                         !LSYMSP(ISYMSP(ISP)),
     &            EHARM-EZERO,QUANID
               WRITE (NOUT,1440)
            ENDIF
            IF (MODE .EQ. 1) DEALLOCATE (JQUANT)
  250    CONTINUE
         CLOSE (NSCR)
      ENDIF

 1410 FORMAT (/80('=')//'Generate states itemized by polyad number ',     &
     &   '--'//                                      &
     &   'Maximum value of the polyad number =',I4/                       &
     &   'Maximum quanta of state excitation =',I4/                       &
     &   'Declared polyad coefficients: ',(15I4))
 1420 FORMAT (/'Vibrational states itemized by polyad blocks and ',       &
     &   'symmetry species'/80('-')/                                      &
     &   '  ##  Pol. Loc.    Harm.Ener.   ',                              &
     &   'Vibrational State Quantum Numbers'/80('-'))
 1430 FORMAT (I4,I5,I5,2X,F12.4,3X,A)                                       !(I4,I5,I5,2X,A4,2F12.4,3X,A)
 1440  FORMAT (80('-'))
!-----------------------------------------------------------------------------------------------

!=======================================================================
!     Vibrational Configuration Interaction (VCI) Method
!=======================================================================
      WRITE (NOUT,1500)

 !     ALLOCATE (ENERHARM(0:NSTATE),ENERVPT2(0:NSTATE), STAT = IERR)             !  It will be used after VCI routine
 !     LOCERR = 6;  IF (IERR .NE. 0) GO TO 990
 !     ALLOCATE (ENERHO(0:NSTATE),ENERCI(0:NSTATE),ENERPT(0:NSTATE),       &
 !    &   ENERPH(0:NSTATE), STAT = IERR)
 !     LOCERR = 6;  IF (IERR .NE. 0) GO TO 990
      ALLOCATE (NQUPOL(0:NSTATE), STAT = IERR)
      LOCERR = 5;  IF (IERR .NE. 0) GO TO 990

      MAXPOL = 0
      DO 300 L = 0, NSTATE
         N = 0
         DO Q = 1, NQ
            N = N + KPOLY(Q) * IQUANT(Q,L)
         END DO
         NQUPOL(L) = N                                                          ! Polyad Quantum Number for each IQUANT state.
         MAXPOL = MAX (MAXPOL, NQUPOL(L))
300   CONTINUE

      IF ( NQEX + NQIN .NE. NQ) GOTO 950
      
      WRITE (NOUT,1510) NQ,NQEX,NQIN,MPES,LPES(MPES),TOLCUT,NTHAM,        &     ! MODREP,
     &   NOPTST,NSTATE,NSTART,MAXPOL,MAXVRT,MULMOD,ENERMAX,MAX_PT,        &     !  KIND
     &   KIND(ONE)                                                              !,MANT
      WRITE (*   ,1510) NQ,NQEX,NQIN,MPES,LPES(MPES),TOLCUT,NTHAM,        &     ! MODREP,
     &   NOPTST,NSTATE,NSTART,MAXPOL,MAXVRT,MULMOD,ENERMAX,MAX_PT,        &     !  KIND
     &   KIND(ONE)                                                              !,MANT

      IF (NSTART .GT. 0) THEN
         WRITE (*,1520) NSTART,NSTATE
         READ (*,"(I)") KEY
         IF (KEY .EQ. 0) NSTART = 0
      ENDIF

1500  FORMAT (/'Vibrational Rayleigh-Schrodinger Perturbation Theory:'/   &
     &   'Calculate Ground State Level and Fundamental States.')
     
1510 FORMAT (/'RSPT parameters:'/                                         &
     &   'The number of degrees of freedom      = ',I8/                   &
     &   'The number of External Quant.Num.(v)  = ',I8/                   &
     &   'The number of Internal Quant.Num.(l)  = ',I8/                   &
     &   'The order of potential function       = ',I8,2X,A16/            &
!     &   '(n)MR-PES (n-Mode Representation), n  = ',I8/                   &
     &   'Tolerance for CVPT operator terms     = ',ES8.1,' 1/Cm'/        &
     &   'The number of operator terms in H     = ',I8/                   &
     &   'Vibrational states set option, 1-4    = ',I8/                   &
     &   'The number of vibrational states      = ',I8/                   &
     &   'Declared resumed calculation from     = ',I8/                   &
     &   'Maximum polyad quantum number         = ',I8/                   &
     &   'Max mode excitation of virtual states = ',I8/                   &
     &   'Number of modes for virtual states    = ',I8/                   &
     &   'Energy limit for the virtual states   = ',F10.1/                &
     &   'Maximum Order of Perturbation Theory  = ',I8/                   &
     &   'Numerical accuracy: KIND(REAL)        = ',I8/                   )

!     &   'Numerical accuracy: Multi Precision   = ',I8)
1520 FORMAT (/'Declaration: Use saved data about',I4,' states of',I4/    &
     &   'Please acknowledge (1) or cancel (0) this option: '\)

      IF (NQEX .NE. 0) DEALLOCATE(KPOLY)
!-----------------------------------------------------------------------------------------------
      CALL    LADDCOEF
      KDIAG = 0                                !  Eigenvalue Problems #
      KSCAN = 0                                !  Hamiltonian scans
      KELEM = 0                                !  Matrix element evals
      IOPER = 0                                !  Value[*,0] is not used
      ALLOCATE (ISTATE(NQ))
!-----------------------------------------------------------------------------------------------
!     Create virtual states: single, double, triple & quadruple
!     (de-)excitations
!-----------------------------------------------------------------------------------------------
      WRITE (NOUT,1600) MULMOD,MAXVRT,ENERMAX

      IF (LOUT .GT. 0) WRITE (NOUT,1610)

      DO 400 IPASS = 1, 2

      IF (IPASS .EQ. 2) THEN
         DO 310 V = 0, NSTATE
         DO 310 Q = 1, NQ
            IVIRTU(Q,V) = IQUANT(Q,V)
  310    CONTINUE
      ENDIF
      NVIRT = NSTATE

!----    Generate virtual states: single (de-)excited configurations

      LOCAL = 0
      DO 340 NP = 1, NQ
         DO K = 1, MAXVRT
!----       Create single (de-)excited configuration
            ISTATE = 0
            ISTATE(NP) = K
!----       Skip polyad states
            DO V = 1, NSTATE
               J = 0
               DO I = 1, NQ
                  IF (ISTATE(I) .EQ. IQUANT(I,V)) J = J + 1
               END DO
               IF (J .EQ. NQ) EXIT
            END DO
            IF (J .EQ. NQ) CYCLE

            CALL ENERHAOS (NQ,W,ISTATE,EZERO, ESTATE)
            ESTATE = ESTATE - EZERO
            IF (ESTATE .GT. ENERMAX) EXIT

            NVIRT = NVIRT + 1
            LOCAL = LOCAL + 1
            IF (IPASS .EQ. 1) CYCLE

            DO J = 1, NQ
               IVIRTU(J,NVIRT) = ISTATE(J)
            END DO
!----          Used for ID labeling only
            NQUANT = IABS (K)
            IF (LOUT .GT. 0 .AND. LOCAL .LE. LIMIT) THEN
               WRITE (UNIT = QUANID, FMT = "('(',24(I2:','))") (ISTATE(J),J=1,NQ)
               QUANID = TRIM(QUANID) // ')'
               WRITE (NOUT,1620) NVIRT,SDTQ(1),NP,K,NQUANT,ESTATE,QUANID             !
            END IF
         END DO
  340 CONTINUE
      NVIRT1 = LOCAL

 1620 FORMAT (I4,3X,A6, 1X,I3,':',I2,22X,'=',I4,F13.2,4X,A80)

!----    Generate virtual states: double (de-)excited configurations

      LOCAL = 0
      DO 350 NP = 1, NQ - 1
      DO 350 MQ = NP + 1, NQ
         DO K = 1, MAXVRT
         DO L = 1, MAXVRT
!----       Create double (de-)excited configuration
            ISTATE = 0
            ISTATE(NP) = K
            ISTATE(MQ) = L
!----       Skip polyad states
            DO V = 1, NSTATE
               J = 0
               DO I = 1, NQ
                  IF (ISTATE(I) .EQ. IQUANT(I,V)) J = J + 1
               END DO
               IF (J .EQ. NQ) EXIT
            END DO
            IF (J .EQ. NQ) CYCLE

            CALL ENERHAOS (NQ,W,ISTATE,EZERO, ESTATE)
            ESTATE = ESTATE - EZERO
            IF (ESTATE .GT. ENERMAX) EXIT

            NVIRT = NVIRT + 1
            LOCAL = LOCAL + 1
            IF (IPASS .EQ. 1) CYCLE

            DO J = 1, NQ
               IVIRTU(J,NVIRT) = ISTATE(J)
            END DO
            NQUANT = IABS (K) + IABS (L)
            IF (LOUT .GT. 0 .AND. LOCAL .LE. LIMIT) THEN
               WRITE (UNIT = QUANID, FMT = "('(',24(I2:','))") (ISTATE(J),J=1,NQ)
               QUANID = TRIM(QUANID) // ')'
               WRITE (NOUT,1630) NVIRT,SDTQ(2),NP,K,MQ,L,NQUANT,ESTATE,QUANID
            END IF
         END DO; END DO
  350 CONTINUE
      NVIRT2 = LOCAL

 1630 FORMAT (I4,3X,A6,1X,I3,':',I2,',',I3,':',I2,15X,'=',I4,F13.2,4X,A80)

!----    Generate virtual states: triple (de-)excited configurations

      NVIRT3 = 0
      IF (MULMOD .LE. 2) GO TO 370

      LOCAL = 0
      DO 360 NP = 1, NQ - 2
      DO 360 MQ = NP + 1, NQ - 1
      DO 360 IR = MQ + 1, NQ
         DO K = 1, MAXVRT
         DO L = 1, MAXVRT
         DO M = 1, MAXVRT
!----       Create triple (de-)excited configuration
            ISTATE = 0
            ISTATE(NP) = K
            ISTATE(MQ) = L
            ISTATE(IR) = M
!----       Skip polyad states
            DO V = 1, NSTATE
               J = 0
               DO I = 1, NQ
                  IF (ISTATE(I) .EQ. IQUANT(I,V)) J = J + 1
               END DO
               IF (J .EQ. NQ) EXIT
            END DO
            IF (J .EQ. NQ) CYCLE

            CALL ENERHAOS (NQ,W,ISTATE,EZERO, ESTATE)
            ESTATE = ESTATE - EZERO
            IF (ESTATE .GT. ENERMAX) EXIT

            NVIRT = NVIRT + 1
            LOCAL = LOCAL + 1
            IF (IPASS .EQ. 1) CYCLE

            DO J = 1, NQ
               IVIRTU(J,NVIRT) = ISTATE(J)
            END DO
            NQUANT = IABS (K) + IABS (L) + IABS (M)
            IF (LOUT .GT. 0 .AND. LOCAL .LE. LIMIT) THEN
               WRITE (UNIT = QUANID, FMT = "('(',24(I2:','))") (ISTATE(J),J=1,NQ)
               QUANID = TRIM(QUANID) // ')'
               WRITE (NOUT,1640) NVIRT,SDTQ(3),NP,K,MQ,L,IR,M,NQUANT,ESTATE,QUANID
            END IF
         END DO; END DO; END DO
  360 CONTINUE
      NVIRT3 = LOCAL

 1640 FORMAT (I4,3X,A6,1X,2(I3,':',I2,','),I3,':',I2,8X,'=',I4,F13.2,4X,A80)

!----    Generate virtual states: quadruple (de-)excited configurations

  370 CONTINUE
      NVIRT4 = 0
      IF (MULMOD .LE. 3) GO TO 390

      LOCAL = 0
      DO 380 NP = 1, NQ - 3
      DO 380 MQ = NP + 1, NQ - 2
      DO 380 IR = MQ + 1, NQ - 1
      DO 380 JS = IR + 1, NQ
         DO K = 1, MAXVRT
         DO L = 1, MAXVRT
         DO M = 1, MAXVRT
         DO N = 1, MAXVRT
!----       Create quadruple (de-)excited configuration
            ISTATE = 0
            ISTATE(NP) = K
            ISTATE(MQ) = L
            ISTATE(IR) = M
            ISTATE(JS) = N
!----       Skip polyad states
            DO V = 1, NSTATE
               J = 0
               DO I = 1, NQ
                  IF (ISTATE(I) .EQ. IQUANT(I,V)) J = J + 1
               END DO
               IF (J .EQ. NQ) EXIT
            END DO
            IF (J .EQ. NQ) CYCLE

            CALL ENERHAOS (NQ,W,ISTATE,EZERO, ESTATE)
            ESTATE = ESTATE - EZERO
            IF (ESTATE .GT. ENERMAX) EXIT

            NVIRT = NVIRT + 1
            LOCAL = LOCAL + 1
            IF (IPASS .EQ. 1) CYCLE

            DO J = 1, NQ
               IVIRTU(J,NVIRT) = ISTATE(J)
            END DO
            NQUANT = IABS (K) + IABS (L) + IABS (M) + IABS (N)
            IF (LOUT .GT. 0 .AND. LOCAL .LE. LIMIT) THEN
               WRITE (UNIT = QUANID, FMT = "('(',24(I2:','))") (ISTATE(J),J=1,NQ)
               QUANID = TRIM(QUANID) // ')'
               WRITE (NOUT,1650) NVIRT,SDTQ(3),NP,K,MQ,L,IR,M,JS,N,NQUANT,ESTATE,QUANID
            END IF
         END DO; END DO; END DO; END DO
  380 CONTINUE
      NVIRT4 = LOCAL

 1650 FORMAT (I4,3X,A6,1X,3(I3,':',I2,','),I3,':',I2,' =',I4,F13.2,4X,A80)

  390 IF (IPASS .EQ. 1) THEN
         ALLOCATE (IVIRTU(NQ,0:NVIRT), STAT = IERR)
         LOCERR = 7;  IF (IERR .NE. 0) GO TO 990
!         ALLOCATE (EVIRTU(0:NVIRT), STAT = IERR)                               ! It will be used in RSPT routine
!         LOCERR = 8;  IF (IERR .NE. 0) GO TO 990
      ENDIF

  400 CONTINUE  !  IPASS = 1, 2

      WRITE (NOUT,1660) NVIRT1,NVIRT2,NVIRT3,NVIRT4,NVIRT

 1600 FORMAT (/'Generate virtual states for the Hamiltonian matrix:'/     &     !  2400
     &   'Maximum number of different modes, MULMOD =',I8/                &
     &   'Maximum mode quanta of excitation, MAXVRT =',I8/                &
     &   'Energy limit for a virtual state, EnerMax =',F8.1/64('=')/      &
     &   'Conditions: Energy of a virtual state <= EnerMax, and'/         &
     &   '            Max excitation on a single mode <= MAXVRT.')
 1610 FORMAT (/'Virtual States Specifications:'/80('-')/                  &
     &   ' No.    Type         Mode Excitations       Total    Energy',   &
     &   '     Quantum Numbers'/80('-'))
 1660 FORMAT (80('-')/                                                    &
     &   'Total number of 1-mode  virtual states =',I8/                   &
     &   'Total number of 2-mode  virtual states =',I8/                   &
     &   'Total number of 3-mode  virtual states =',I8/                   &
     &   'Total number of 4-mode  virtual states =',I8/48('=')/           &
     &   'Total number of created virtual states =',I8)
!-----------------------------------------------------------------------------------------------
!    Generate Lz Matrix. Note that Lz Matrix is Always Complex
!    LZELE and ELEMAT is DOUBLE COMPLEX Scalar
!    LZMAT            is DOUBLE COMPLEX Matrix
!-----------------------------------------------------------------------------------------------
     MVIRT = NVIRT + 1
     ALLOCATE( IOPBRA(NQ), IOPKET(NQ) )
     ALLOCATE( LZMAT (MVIRT,MVIRT), STAT = IERR)
     LOCERR =  9;  IF (IERR .NE. 0) GO TO 990

     ICOUNT = 0
     MCOUNT = MVIRT * (MVIRT + 1) / 2
     CALL LWATCH (LSPLIT1)   
     ICENT0 = 0
     CALL WATCH (ISPLIT)                                                     
     WRITE (*,405) NVIRT,NTLZ
     CALL COUNTER (0,0,1)                                                    
405  FORMAT (/'Evaluate L_z matrix:',I8,' states,',I8,' terms ...'\)      

     DO 420 K = 0, NVIRT
     DO 420 L = 0, K
         ICOUNT = ICOUNT + 1
         ICENT1 = 100 * ICOUNT / MCOUNT      
         IF (ICENT1 .NE. ICENT0) THEN
             CALL COUNTER (1,ICENT1,1)                                         
             ICENT0 = ICENT1 
         ENDIF
         DO 410 Q = 1, NQ
             IOPBRA(Q) = IVIRTU(Q,K) + 1
             IOPKET(Q) = IVIRTU(Q,L) + 1
410       CONTINUE
          ELEMAT = LZELE  (NQ,NTLZ,LCOEF,LADLZ,IOPBRA,IOPKET)
          LZMAT(K+1,L+1) = ELEMAT
          LZMAT(L+1,K+1) = ELEMAT
420  CONTINUE
      
     CALL COUNTER (1,-1,1) 
     CALL WATCH2 (ISPLIT)
     DEALLOCATE (LCOEF, LADLZ)

     IF (LOUT .EQ. 2) THEN
         WRITE(NOUT,"(/' Lz Matrix has the following form'/)")
         DO J = 1, MVIRT
             WRITE (NOUT,"(200(1X,F10.5))") (IMAG(LZMAT(I,J)), I=1,MVIRT)
         ENDDO
     ENDIF
!--------------------------------------------
!   LAPACK is Used in the VCI PROCEDURE
!
!   ZHETRD - Reduces a Complex Hermitian Matrix to Tridiagonal Form.
!   call Zhetrd(uplo, n, a, lda, d, e, tau, work, lwork, info)
!   ZUNGTR - Generate Unitary Matrix that Transfrom Hermitian Matrix to
!            Tridiagonal Form
!   call zungtr(uplo, n, a, lda, tau, work, lwork, info)
!
!   ZSTEQR - Computes all eigenvalues and eigenvectors of a symmetric
!            or Hermitian matrix reduced to tridiagonal form (QR algorithm).
!   call zsteqr(compz, n, d, e, z, ldz, work, info)
!   ZGEMM  Computes a Matrix-Matrix product with Complex matrices.
!--------------------------------------------
      ALLOCATE (TRIDIAG(MVIRT,MVIRT))
      ALLOCATE (TAU(MVIRT-1) ,WORK(MVIRT)     )
      ALLOCATE (OFFDIAG(MVIRT)  )
      ALLOCATE (DIAG(MVIRT)       )
      ALLOCATE (EIGVECLZ(MVIRT,MVIRT), AUX(MVIRT,MVIRT))
      CALL ZHETRD('U', MVIRT, LZMAT, MVIRT, DIAG, OFFDIAG, TAU, WORK, MVIRT, INFO)
      AUX = LZMAT
      CALL ZUNGTR('U', MVIRT, AUX, MVIRT, TAU, WORK, MVIRT, INFO)
      CALL ZSTEQR('I', MVIRT, DIAG, OFFDIAG, LZMAT, MVIRT, WORK,INFO)

!     EIGVECLZ = MATMUL(AUX, LZMAT)
      CALL ZGEMM('N', 'N', MVIRT, MVIRT, MVIRT, CONE, AUX, MVIRT, LZMAT, MVIRT, CZERO, EIGVECLZ, MVIRT)
      LZMAT    = EIGVECLZ

      DEALLOCATE (TRIDIAG, TAU, OFFDIAG, WORK, AUX, EIGVECLZ)

!-------------------------------------------------------------------------------------------
!   MATRIX OUTPUT
!-------------------------------------------------------------------------------------------
      IF ( LOUT .EQ. 2) THEN
          WRITE(NOUT,"(/'Eigenvalues of Lz : '/)")
          WRITE (NOUT,    "(200(1X,F10.5))") (DIAG(I), I=1,MVIRT)
          WRITE(NOUT,"(/'The Real Part of Lz Eigenvector : '/)")
          DO J = 1, MVIRT
              WRITE (NOUT,"(200(1X,F10.5))") (REAL(LZMAT(I,J)), I=1,MVIRT)
          ENDDO
          WRITE(NOUT,"(/'The Image Part of Lz Eigenvector : '/)")
          DO J = 1, MVIRT
              WRITE (NOUT,"(200(1X,F10.5))") (IMAG(LZMAT(I,J)), I=1,MVIRT)
          ENDDO
      ENDIF
!-------------------------------------------------------------------------------------------
!   Select Eigenvectors Satisfying The Sayvetz Condition
!   Use Eigenvectors to Form a Projector (PROJECT) and its Hermitian Conjugate (PROJTRAN),
!   which will be used to transform Hamiltonian Matrix
!
!   EIGVL Should be set to Chose Eigenvalue of Lz
!-------------------------------------------------------------------------------------------
      WRITE(NOUT,*)

      ALLOCATE(COPY1(MVIRT,1))
      IF (NQEX .EQ. 0) THEN
          DO MODE = 0, 1
              IVIRTUL = 0
              DO I = 1, MVIRT
                      IVIRTUL = IVIRTUL + 1                                               !
                      IF (MODE .EQ. 0 .AND. LOUT .EQ. 2) THEN                             !
                          WRITE (NOUT,"(   F10.5)")  DIAG(I)                              !
                          WRITE (NOUT,"(200(1X,F10.5))") (REAL(LZMAT(J,I)), J=1,MVIRT)    !
                          WRITE (NOUT,"(200(1X,F10.5))") (IMAG(LZMAT(J,I)), J=1,MVIRT)    !
                      ENDIF                                                               !
                      IF (MODE .EQ. 1) THEN                                               !
                          DO J = 1, MVIRT                                                 !
                              COPY1(J,1) = LZMAT(J, I)                                    !
                              PROJECT (IVIRTUL, J) = COPY1(J,1)                           !
                              PROJTRAN(J, IVIRTUL) = CONJG(COPY1(J,1))                    !
                          ENDDO                                                           !
                      ENDIF                                                               !                                                                  !
              ENDDO
              IF (MODE .EQ. 0 ) THEN
                  ALLOCATE(PROJECT (IVIRTUL, MVIRT))
                  ALLOCATE(PROJTRAN(MVIRT, IVIRTUL))
                  PROJECT  = 0
                  PROJTRAN = 0
              ENDIF
          ENDDO
      ELSE      
          DO MODE = 0, 1
              IVIRTUL = 0
              DO I = 1, MVIRT
                  EIGVL = REAL(NSAYVETZ, KIND=16) - ABS(DIAG(I))                          !
              IF ( ABS(EIGVL) < TOLCUT  ) THEN                                            ! Select Eigenstates with L = 0,1,2,...
                      IVIRTUL = IVIRTUL + 1                                               !
                      IF (MODE .EQ. 0 .AND. LOUT .EQ. 2) THEN                             !
                          WRITE (NOUT,"(   F10.5)")  DIAG(I)                              !
                          WRITE (NOUT,"(200(1X,F10.5))") (REAL(LZMAT(J,I)), J=1,MVIRT)         !
                          WRITE (NOUT,"(200(1X,F10.5))") (IMAG(LZMAT(J,I)), J=1,MVIRT)         !
                      ENDIF                                                               !
                      IF (MODE .EQ. 1) THEN                                               !
                          DO J = 1, MVIRT                                                 !
                              COPY1(J,1) = LZMAT(J, I)                                    ! For l=0, Eigenvector LZMAT is not always Real
                              PROJECT (IVIRTUL, J) = COPY1(J,1)                           !
                              PROJTRAN(J, IVIRTUL) = CONJG(COPY1(J,1))                    !
                          ENDDO                                                           !
                      ENDIF                                                               !
              ENDIF                                                                       !
              ENDDO
              IF (MODE .EQ. 0 ) THEN
                  ALLOCATE(PROJECT (IVIRTUL, MVIRT))
                  ALLOCATE(PROJTRAN(MVIRT, IVIRTUL))
                  PROJECT  = 0
                  PROJTRAN = 0
              ENDIF
          ENDDO 
      ENDIF
      
      DEALLOCATE(COPY1, LZMAT, DIAG)
!-----------------------------------------------------------------------------------------------!
!      WRITE(NOUT,"(/'Projector Opearator : '/)")                                               !
!      DO I = 1, IVIRTUL                                                                          !
!          WRITE(NOUT, "(60F15.5)") (REAL(PROJECT(I, J)), J = 1, MVIRT)                         !
!          WRITE(NOUT, "(60F15.5)") (IMAG(PROJECT(I, J)), J = 1, MVIRT)                         !
!          WRITE(NOUT, *)                                                                       !
!      ENDDO                                                                                    !
!                                                                                               !
!      WRITE(NOUT,"(/'Hermitian-Conjugate Projector Opearator : '/)")                           !
!      DO I = 1, IVIRTUL                                                                          !
!          WRITE(NOUT, "(60F15.5)") (REAL(PROJTRAN(J, I)), J = 1, MVIRT)                        !
!          WRITE(NOUT, "(60F15.5)") (IMAG(PROJTRAN(J, I)), J = 1, MVIRT)                        !
!          WRITE(NOUT, *)                                                                       !
!      ENDDO                                                                                    !
!-----------------------------------------------------------------------------------------------!
!===============================================================================================
!   Vibrational Configuration Interaction Method
!===============================================================================================
!-----------------------------------------------------------------------------------------------
!   Note:
!   HAMATR  is (Isomorphic) Hamilton Matrix in Representation of All Virtual States (DOUBLE COMPLEX)
!   COPY    is  Auxiliay Array that Storage tht Product of Project and HAMATR       (DOUBLE COMPLEX)
!   HAMATRE is the Transformed Hamiltonian                                          (DOUBLE COMPLEX)
!   EIGVAL  is the Final Eigenvalues                                                (DOUBLE REAL)
!-----------------------------------------------------------------------------------------------
      ALLOCATE (HARM(MVIRT, MVIRT) , VPERT  (MVIRT, MVIRT) )
      
      ICOUNT = 0
      MCOUNT = MVIRT * (MVIRT + 1) / 2
      CALL LWATCH (LSPLIT1)                                                     !  LWATCH

      ICENT0 = 0
      CALL WATCH (ISPLIT)                                                       !  WATCH
      WRITE (*,1700) NVIRT,NTHAM
      CALL COUNTER (0,0,1)                                                      !  COUNTER
1700  FORMAT (/'Evaluate VCI matrix:',I8,' states,',I8,' terms ...'\)           !  3000

      CHECKSUM = 0.0D0

      DO  K = 0, NVIRT
          DO  L = 0, K
              ICOUNT = ICOUNT + 1
              ICENT1 = 100 * ICOUNT / MCOUNT
              IF (ICENT1 .NE. ICENT0) THEN
                  CALL COUNTER (1,ICENT1,1)                                           !  COUNTER
                  ICENT0 = ICENT1
              ENDIF
              DO  Q = 1, NQ
                  IOPBRA(Q) = IVIRTU(Q,K) + 1
                  IOPKET(Q) = IVIRTU(Q,L) + 1
              ENDDO
              MATELEHARM = HAOSMP (NQ, NHORD(0), HOCOEF, LADHO, IOPBRA, IOPKET)
              MATELEPERT = HAOSMP (NQ,  NPERT,   VCOEF,  LADVP, IOPBRA, IOPKET)
              HARM  (K+1, L+1) = CMPLX(MATELEHARM, 0.0D0, KIND=8)
              HARM  (L+1, K+1) = CMPLX(MATELEHARM, 0.0D0, KIND=8)
              VPERT (K+1, L+1) = CMPLX(MATELEPERT, 0.0D0, KIND=8)
              VPERT (L+1, K+1) = CMPLX(MATELEPERT, 0.0D0, KIND=8)
              CHECKSUM = CHECKSUM + ABS (MATELE)
          ENDDO
      ENDDO

      CALL COUNTER (1,-1,1)                                                     !  COUNTER ()
      CALL WATCH2 (ISPLIT)                                                      !  WATCH2 ()
    
      IF (LOUT .EQ. 2) THEN      
          WRITE(NOUT,"(/'The Harmonic Hamilton Matrix : '/)")
          DO J = 1, MVIRT
              WRITE (NOUT,"(200(1XF10.5))") (REAL(HARM(I,J)  ), I=1,MVIRT)
          ENDDO
          
          WRITE(NOUT,"(/'The Perturbated Term Matrix : '/)")
          DO J = 1, MVIRT
              WRITE (NOUT,"(200(1XF10.5))") (REAL(VPERT(I,J) ), I=1,MVIRT)
          ENDDO
      ENDIF
      
      DEALLOCATE(IVIRTU, IQUANT, ISTATE)
!-----------------------------------------------------------------------------------------------
!   Transform Isotropic Hamiltonian to the Form, Satisfying Sayvetz Condition
!
!   IMPORTANT: The Precision of Hamiltonian Matrix has to be changed to KIND = 8
!
!   ZHEEVD Computes all eigenvalues and, optionally, eigenvectors of a Hermitian matrix.
!
!   ZGEMM  Computes a Matrix-Matrix product with Complex matrices.
!
!   Using MATMUL Function encouters with Stack Overflow Problem, when the Size of Matrix is Larger than 1000,
!   Therefore the Library Function ZGEMM was used hereinafter
!-----------------------------------------------------------------------------------------------
      ALLOCATE (  COPY1(IVIRTUL, MVIRT),     COPY2(IVIRTUL, MVIRT))
      ALLOCATE (HAMATRE(IVIRTUL, IVIRTUL),  HARMTRAN(IVIRTUL, IVIRTUL), VPERTRAN(IVIRTUL,IVIRTUL))

      CALL ZGEMM('N', 'N', IVIRTUL,   MVIRT, MVIRT, CONE, PROJECT, IVIRTUL,    HARM, MVIRT, CZERO,    COPY1, IVIRTUL)
      CALL ZGEMM('N', 'N', IVIRTUL, IVIRTUL, MVIRT, CONE,  COPY1, IVIRTUL, PROJTRAN, MVIRT, CZERO, HARMTRAN, IVIRTUL)
      
!---- Reorder Eigenvector
      ALLOCATE(WEX(IVIRTUL), AUX1(1,IVIRTUL))
      DO I = 1, IVIRTUL
          WEX(I) = HARMTRAN(I,I)
      ENDDO
      CALL RQSORT (IVIRTUL,WEX,AUX1(1,:))
      
      ALLOCATE(AUX3(IVIRTUL, MVIRT), AUX4(MVIRT, IVIRTUL))
      AUX3 = PROJECT
      AUX4 = PROJTRAN
      
      DO I = 1, IVIRTUL
          PROJECT (I,:) = AUX3(AUX1(1,I),:)
          PROJTRAN(:,I) = AUX4(:,AUX1(1,I))          
      ENDDO
      DEALLOCATE(WEX, AUX1, AUX3, AUX4)
!----      
      CALL ZGEMM('N', 'N', IVIRTUL,   MVIRT, MVIRT, CONE, PROJECT, IVIRTUL,    HARM, MVIRT, CZERO,    COPY1, IVIRTUL)
      CALL ZGEMM('N', 'N', IVIRTUL,   MVIRT, MVIRT, CONE, PROJECT, IVIRTUL,   VPERT, MVIRT, CZERO,    COPY2, IVIRTUL) 
      
      CALL ZGEMM('N', 'N', IVIRTUL, IVIRTUL, MVIRT, CONE,  COPY1, IVIRTUL, PROJTRAN, MVIRT, CZERO, HARMTRAN, IVIRTUL)
      CALL ZGEMM('N', 'N', IVIRTUL, IVIRTUL, MVIRT, CONE,  COPY2, IVIRTUL, PROJTRAN, MVIRT, CZERO, VPERTRAN, IVIRTUL)    
      
      HAMATRE = HARMTRAN + VPERTRAN
      
      DEALLOCATE( HARM , VPERT ) 
      DEALLOCATE( COPY1, COPY2, PROJECT, PROJTRAN)

      IF (LOUT .EQ. 2) THEN
        WRITE   (NOUT, "(/'The Harmonic Part was Reduced to the Following Form : '/)")
        DO J = 1, IVIRTUL
          WRITE (NOUT,"(200F15.7)") (HARMTRAN(I,J), I=1,IVIRTUL)!(REAL(HARMTRAN(I,J)), I=1,IVIRTUL)
        ENDDO
        WRITE   (NOUT, "(/'The Perturbated Part was Reduced to the Following Form : '/)")
        DO J = 1, IVIRTUL
          WRITE (NOUT,"(200F15.7)") (VPERTRAN(I,J), I=1,IVIRTUL)!(REAL(VPERTRAN(I,J)), I=1,IVIRTUL)
        ENDDO
      ENDIF

!---  The Calculating Accuracy should be Improved ! 

!==================================================================================================      
      WRITE(NOUT, 1800)
      WRITE( *  ,    *)
1800  FORMAT(/64('=')/)
      
      WRITE(NOUT, 1810) IVIRTUL
      WRITE(*,    1810) IVIRTUL
1810  FORMAT('Projection of Isotropic Hamiltonian to Sayvetz Subspace has been Performed', / &
     &       'The Number of Virtual States after Transformation = ', I4                      )

      WRITE   (NOUT, "('The Isomorphic Hamilton Matrix was Reduced to the Following Form : '/)")
      DO J = 1, IVIRTUL
        WRITE (NOUT,"(200F15.7)") (REAL(HAMATRE(I,J)), I=1,IVIRTUL)
      ENDDO

      ALLOCATE(EIGVAL(IVIRTUL))           !WORK2(LWORK),

      LWORK  = IVIRTUL*(IVIRTUL+2)
      LRWORK = 2*IVIRTUL**2+5*IVIRTUL+1
      LIWORK = 5*IVIRTUL + 3
      ALLOCATE (WORK(LWORK), RWORK(LRWORK), IWORK(LIWORK)     )
      CALL ZHEEVD('V', 'U', IVIRTUL, HAMATRE, IVIRTUL, EIGVAL, WORK, LWORK, RWORK, LRWORK, IWORK, LIWORK, INFO)
      
      DEALLOCATE(WORK, RWORK, IWORK)

      WRITE(NOUT,"(/ 'Eigenvalues with L = ', I2 ,' have been Calculated : '/)") NSAYVETZ                    ! Sayvetz Condition
      WRITE(NOUT,"(200F15.7)") (EIGVAL(I), I = 1, IVIRTUL)  

      WRITE(NOUT,"(/ 'Eigenvectors with L = ', I2 ,' have been Calculated : '/)") NSAYVETZ                    ! Sayvetz Condition
      DO I = 1, IVIRTUL
          WRITE(NOUT,"(200F15.7)") (REAL(HAMATRE(I, J)), J = 1, IVIRTUL)      
      ENDDO
      
!-----------------------------------------------------------------------------------------------
!     Create/read specifications of vibrational states of interest in v/l representation:
!-----------------------------------------------------------------------------------------------
         
      FILE_POLY2 = TRIM (PATH_BIN) // 'POLYAD2.BIN'   
      FILE_STATE = TRIM (PATH_BIN) // 'STATES .TXT'
      OPEN (UNIT = NSCR, FILE = FILE_POLY2, DISPOSE = 'DELETE',         &
     &      ACTION = 'READWRITE', FORM = 'UNFORMATTED')   
      OPEN (UNIT = NAUX, FILE = FILE_STATE,                             &!DISPOSE = 'DELETE',         &
     &      ACTION = 'READWRITE', FORM = 'FORMATTED')   
      
      IF (NQEX .NE. 0) THEN
          
          ALLOCATE(WEX(NQEX))
          J = 1
          WEX(1)  = W(1)
          WAVENUM = W(1)
          DO I = 1, NQ
              IF ( WAVENUM .NE. W(I)) THEN
                  WAVENUM = W(I)
                  J = J + 1
                  WEX(J) = WAVENUM
              ENDIF
          ENDDO
!---          
          NOPTST = 0
          NSTATE = 0
          MSIZE = 0  !  Maximum size of polyad blocks
             
          DO 1950 MODE = 0, 1
              K = 0
              DO 1960 NPOLY = 1, MAXPOL
!----         Create states for given NPOLY and save to file NSCR
                  IOUT = 0  !  = MODE             
                  CALL PSTATE (NQEX,WEX,KEXPOLY,NPOLY,MAXQUA,MODE,          &  !ISYMSP,
     &                NSIZE,JEXQUANT,MAXTOT,LIMIT,IOUT)
                  IF (NSIZE .EQ. 0) CYCLE             
                  IF (MODE .EQ. 0) THEN
                      MSIZE = MAX (MSIZE, NSIZE)
                      NSTATE = NSTATE + NSIZE
                      CYCLE
                  ENDIF
                   
                  DO 1970 I = 1, NSIZE
                    K = K + 1
                    DO Q = 1, NQEX
                        IEXQUANT(Q,K) = JEXQUANT(Q,I)
                    END DO
                    CALL ENERHAOS (NQEX,WEX,IEXQUANT(1,K),EZERO,EHARM)
                    WRITE (NAUX,1930) K,NPOLY,I,EHARM-EZERO,(IEXQUANT(Q,K),Q=1,NQEX)
1970              CONTINUE
1960          CONTINUE
          
          
              IF (MODE .EQ. 0) THEN
                ALLOCATE (IEXQUANT(NQEX,0:NSTATE),JEXQUANT(NQEX,0:MSIZE))
                JEXQUANT = 0
                IEXQUANT = 0  !  Ground state is initialized here
                K = 0
                I = 1
                NPOLY = 0
                CALL ENERHAOS (NQEX,WEX,IEXQUANT(1,K),EZERO,EHARM)
                WRITE (NAUX,1930) K,NPOLY,I, EHARM-EZERO,(IEXQUANT(Q,K),Q=1,NQEX)
              ENDIF
                IF (MODE .EQ. 1) DEALLOCATE (JEXQUANT)
1950      CONTINUE
          CLOSE (NSCR)
          WRITE(NAUX, 1980)
          
1930      FORMAT (1XI4,1XI4,1XI4,2X,F12.4,3X,20I3)    
1980      FORMAT ('!',80('-'))      
          REWIND(NAUX)
!-----    
          I = 0
          DO WHILE (.NOT. EOF(NAUX))  
              READ (NAUX,1990) KEYWOR, LINE
                IF (KEYWOR(1:1) .EQ. '!') CYCLE
                I = I + 1
          ENDDO
1990      FORMAT(A1,A64)      
          
          NEXSTATE = I 
          REWIND(NAUX)
          
          ALLOCATE(JQUANT(NQEX,NEXSTATE ))
          I = 0  
          DO WHILE (.NOT. EOF(NAUX))  
              READ (NAUX,1990) KEYWOR, LINE
                IF (KEYWOR(1:1) .EQ. '!') CYCLE
                I = I + 1
                READ(UNIT = LINE, FMT="(32X,20I3)") (JQUANT(J, I), J = 1, NQEX)
          ENDDO
          
          NTSTATE = 0
          DO I = 1, NEXSTATE
             K = 1
             DO J = 1, NQIN
                  K = (JQUANT(NQEX - J + 1 , I) + 1 ) * K
             ENDDO
             NTSTATE = NTSTATE + K
          ENDDO
                
          ALLOCATE (JEXQUANT(NQ, NTSTATE))      
          ALLOCATE (VMAXS(NQIN))
          JEXQUANT = 0
          ICOUNT  = 0
          
          DO I = 1, NEXSTATE
              DO K = 1, NQIN
                  VMAXS(K) = JQUANT(NQEX - NQIN + K , I) + 1
              ENDDO
              
              NTOT = 1
              DO K = 1, NQIN
                  NTOT = NTOT * VMAXS(K)
              ENDDO
              
              ALLOCATE( AUX1(NTOT, NQIN), AUX2(NTOT, NQIN))
              
          
              CALL MLOOPASRCSV(NQIN, VMAXS, NTOT, AUX1)
              
              DO NJ = 1, NTOT
                  DO NI = 1, NQIN
                      AUX2(NJ, NI) = -(VMAXS(NI) - 1) + 2 * ( AUX1(NJ, NI) - 1)
                  ENDDO
              ENDDO
              
              DO M = 1, NTOT 
                  DO N = 1, NQEX
                      JEXQUANT(N       , ICOUNT + M) = JQUANT(N, I)
                  ENDDO              
                  DO N = 1, NQIN
                      JEXQUANT(NQEX + N, ICOUNT + M) = AUX2(M, N)
                  ENDDO
              ENDDO
              DEALLOCATE(AUX1, AUX2)
              ICOUNT = ICOUNT + NTOT
          ENDDO
          
          DEALLOCATE(JQUANT, VMAXS)
          
          IF (ICOUNT .EQ. NTSTATE) THEN
              WRITE(*,"(A)") "Basis function in |v,l> representation has been generated correctly"
              DO I = 1, NTSTATE
                  WRITE(NAUX, "(I2,30X,20I3)" ) I, (JEXQUANT(J, I), J = 1, NQ)
              ENDDO
          ELSE
              WRITE(*,"(A)") "Basis function in |v,l> representation has not been generated"
              PAUSE
              STOP
          ENDIF
!-----          
          NVIRT = 0
          DO I = 1, NTSTATE
              NSUM = 0 
              DO J = 1, NQIN
                  NSUM = NSUM +JEXQUANT(NQEX+J, I) 
              ENDDO
              IF (ABS(NSUM) .EQ. NSAYVETZ ) NVIRT = NVIRT + 1                     ! Sayvetz Condition should be introduced
          ENDDO
          
          ALLOCATE(IVIRTU(NQ, NVIRT))
          
          NVIRT = 0
          DO I = 1, NTSTATE
              NSUM = 0 
              DO J = 1, NQIN
                  NSUM = NSUM +JEXQUANT(NQEX+J, I) 
              ENDDO
              IF (ABS(NSUM) .EQ. NSAYVETZ ) THEN                                    ! Sayvetz Condition should be introduced
                  NVIRT = NVIRT + 1
                  DO K = 1, NQ
                      IVIRTU(K, NVIRT) = JEXQUANT(K, I)
                  ENDDO
              ENDIF
          ENDDO
          
          DEALLOCATE (JEXQUANT)
          CLOSE(NAUX)
          
          NSTATE = 0
          IF (IVIRTUL .EQ. NVIRT) THEN
              WRITE(*   , 2000) NVIRT
              WRITE(NOUT, 2000) NVIRT
              WRITE(NOUT, 2010)
              DO I = 1, NVIRT
                  CALL ENERHAOS (NQEX,WEX,IVIRTU(1:5,I),EZERO,EHARM)
                  IF (EHARM - EZERO .LT. EREFMAX) NSTATE = NSTATE + 1
                  WRITE (UNIT = QUANID, FMT = "('(',24(I2:','))") (IVIRTU(J, I), J = 1, NQ)
                  QUANID = TRIM(QUANID) // ')'
                  WRITE(NOUT, "(I4,2X,F12.4,5X, A)" ) I, EHARM-EZERO, QUANID 
              ENDDO
          ELSE
              GOTO 960
          ENDIF
          
          
          WRITE(NOUT, 2020)
2000      FORMAT(/64('-')// 'Basis function in |V, L> representation under Sayvetz condition '/ &
                & 'has been generated correctly'                                              / &
                & 'The Number of Virtual states is', I4)
2010      FORMAT(/64('-')/                                                                      &
     &       '  ##     Harm.Ener.    Vibrational Quantum Numbers |V, L>'/64('-')                   )      
2020      FORMAT(64('-')/)
     
      ELSE IF (NQEX .EQ. 0) THEN
          NOPTST = 0
          NSTATE = 0
          MSIZE = 0  !  Maximum size of polyad blocks   
          DO MODE = 0, 1
              K = 0
              DO NPOLY = 1, MAXPOL
!----         Create states for given NPOLY and save to file NSCR
                  IOUT = 0  !  = MODE             
                  CALL PSTATE (NQ,W,KPOLY,NPOLY,MAXQUA,MODE,          &  !ISYMSP,
     &                NSIZE,JEXQUANT,MAXTOT,LIMIT,IOUT)
                  IF (NSIZE .EQ. 0) CYCLE             
                  IF (MODE .EQ. 0) THEN
                      MSIZE = MAX (MSIZE, NSIZE)
                      NSTATE = NSTATE + NSIZE
                      CYCLE
                  ENDIF
                   
                  DO I = 1, NSIZE
                    K = K + 1
                    DO Q = 1, NQ
                        IEXQUANT(Q,K) = JEXQUANT(Q,I)
                    END DO
                    CALL ENERHAOS (NQ,W,IEXQUANT(1,K),EZERO,EHARM)
                    WRITE (NAUX,1930) K,NPOLY,I,EHARM-EZERO,(IEXQUANT(Q,K),Q=1,NQ)
                  ENDDO
              ENDDO
          
              IF (MODE .EQ. 0) THEN
                ALLOCATE (IEXQUANT(NQ,0:NSTATE),JEXQUANT(NQ,0:MSIZE))
                JEXQUANT = 0
                IEXQUANT = 0  !  Ground state is initialized here
                K = 0
                I = 1
                NPOLY = 0
                CALL ENERHAOS (NQ,W,IEXQUANT(1,K),EZERO,EHARM)
                WRITE (NAUX,1930) K,NPOLY,I, EHARM-EZERO,(IEXQUANT(Q,K),Q=1,NQ)
              ENDIF
                IF (MODE .EQ. 1) DEALLOCATE (JEXQUANT)
          ENDDO
          WRITE(NAUX, 1980)
          REWIND(NAUX)
          CLOSE (NSCR)
          
          I = 0
          DO WHILE (.NOT. EOF(NAUX))  
              READ (NAUX,1990) KEYWOR, LINE
                IF (KEYWOR(1:1) .EQ. '!') CYCLE
                I = I + 1
          ENDDO
          NEXSTATE = I 
          
          REWIND(NAUX)
          ALLOCATE(JQUANT(NQ,NEXSTATE ))
          I = 0  
          DO WHILE (.NOT. EOF(NAUX))  
              READ (NAUX,1990) KEYWOR, LINE
                IF (KEYWOR(1:1) .EQ. '!') CYCLE
                I = I + 1
                IF (I .GT. NEXSTATE) EXIT
                READ(UNIT = LINE, FMT="(32X,20I3)") (JQUANT(J, I), J = 1, NQ)
          ENDDO
!---          
          NVIRT = 0
          DO I = 1, NEXSTATE
              NSUM = 0 
              DO J = 1, NQ
                  NSUM = NSUM + ((-1)**J)* JQUANT(J, I) 
              ENDDO
             ! IF (NSUM .EQ. NSAYVETZ ) 
              NVIRT = NVIRT + 1                     ! Sayvetz Condition should be introduced
          ENDDO

          ALLOCATE(IVIRTU(NQ, NVIRT))
          NVIRT = 0
          DO I = 1, NEXSTATE
              NSUM = 0 
              DO J = 1, NQIN
                  NSUM = NSUM +(-1)**(J)*JQUANT(J, I) 
              ENDDO
              !IF (NSUM .EQ. NSAYVETZ ) THEN                                    ! Sayvetz Condition should be introduced
                  NVIRT = NVIRT + 1
                  DO K = 1, NQ
                      IVIRTU(K, NVIRT) = JQUANT(K, I)
                  ENDDO
              !ENDIF
          ENDDO

          DEALLOCATE(JQUANT)
          CLOSE(NAUX)
          NSTATE = 0
          IF (IVIRTUL .EQ. NVIRT) THEN
              WRITE(*   , 2000) NVIRT
              WRITE(NOUT, 2000) NVIRT
              WRITE(NOUT, 2010)
              DO I = 1, NVIRT
                  CALL ENERHAOS (NQ,W,IVIRTU(1:5,I),EZERO,EHARM)
                  IF (EHARM - EZERO .LT. EREFMAX) NSTATE = NSTATE + 1
                  WRITE (UNIT = QUANID, FMT = "('(',24(I2:','))") (IVIRTU(J, I), J = 1, NQ)
                  QUANID = TRIM(QUANID) // ')'
                  WRITE(NOUT, "(I4,2X,F12.4,5X, A)" ) I, EHARM-EZERO, QUANID 
              ENDDO
          ELSE
              GOTO 960
          ENDIF          
          WRITE(NOUT, 2020)
      ENDIF
      
!-----------------------------------------------------------------------------------------------
!     RSPT: Loop over vibrational states
!-----------------------------------------------------------------------------------------------      
      ALLOCATE(IQUANT(NQ, NSTATE), ENERCI(NSTATE), ENERPT(NSTATE), ENERHARM(NSTATE),IASSIG(IVIRTUL))
      IQUANT = 0
      DO I = 1, NSTATE
          IF (NQEX .NE. 0) THEN
              IQUANT(:, I) = IVIRTU(:, I)
              CALL ENERHARMDEG (NQEX, NQIN, WEX,IQUANT(1:NQEX,I) ,EZERO, EHARM)         
              ENERHARM( I) = EHARM
          ELSE IF (NQEX .EQ. 0) THEN
              IQUANT(:, I) = IVIRTU(:, I)
              CALL ENERHAOS (NQ,W,IQUANT(1:NQ,I) ,EZERO, EHARM)         
              ENERHARM( I) = EHARM
          ENDIF
      ENDDO

      ENERCI = 0.0D0
      IASSIG = 0
     
      DO 900 LSTATE = 1, NSTATE
         
         WRITE (NOUT,9900)
         CALL WATCH (ISPLIT);  CALL LWATCH (LSPLIT0)
         
         !ISP = ISYMSTAT (NQ,IQUANT(1,LSTATE),ISYMSP)
         WRITE (UNIT = QUANID, FMT = "('(',24(I2:','))") (IQUANT(Q,LSTATE),Q=1,NQ)
         QUANID = TRIM(QUANID) // ')'
         WRITE (*,3100) TRIM(MOLECULE)
         WRITE (*,3110) LSTATE,NSTATE,TRIM(QUANID)

3100     FORMAT (/'Molecule: ',A20)                                                !  3100
3110     FORMAT (/'Vibrational State',I4,' (',I4,')  ',A,           &     
     &   /' -- H(0)/VCI/RSPT(n) Energies:')
!-----------------------------------------------------------------------------------------------
!     Choose current vibrational state, indexation from 0 (zero state).
!
!     Prepare trial set of expansion coefficients for single-mode
!     wave function in harmonic oscillator basis set
!-----------------------------------------------------------------------------------------------
        WRITE (NOUT,3120) LSTATE
        DO N = 1, (NQ - 1) / 10 + 1
           L1 = (N - 1) * 10 + 1
           L2 = (N - 1) * 10 + MIN0 (NQ - (N - 1) * 10, 10)
           WRITE (NOUT,3130) L1,L2,(IQUANT(Q,LSTATE),Q=L1,L2)
        END DO                                                      
3120    FORMAT (/15('='),' Vibrational State (',I4,')',                     &
       &   ' -- Evaluate RSPT Energy ',15('=')//'Quantum numbers:')
3130    FORMAT ('v(',I3,' -',I3') =',20I3 : ('  ',20I3))        
         
!-----------------------------------------------------------------------------------------------
!     Select correct eigen-vector/-value for the VCI energy
!
!     ^                    ***
!     |                  ** | **
!   1 |                 *   |   *
!     |                *    |    *
!     |               *     |     *
!     |              *      |      *
!     |             *       |       *
!     |           **        |        **
!     |         **          |          **
!     |       **            |            **
! 0.01|*******              |              *******
!     |______|______________|______________|_______>
!           -Range          v_obs         +Range
!
!     Weight (W) = exp [-GauCon x Range^2]
!     WeiMin (1%) = 0.01 = exp [-GauCon x Range^2]
!     GauCon = - ln(0.01) / Range^2 = 4.60517 / 3600 = -0.001279214
!
!     At Range =  0 cm^-1, Weight = 1.00 (100%)
!     At Range = 60 cm^-1, Weight = 0.01 (  1%)
!
!     Parameters:
!     (1) Range (cutoff) = 60 cm^-1
!     (2) Weight(cutoff) = 1%
!-----------------------------------------------------------------------------------------------
        FRAME = 1000.0D0  !  in Wavenumbers
        WGTMIN = 0.01D0   !  Percent / 100
        GAUCON = - LOG (WGTMIN) / FRAME ** 2  
        
        WRITE (NOUT,3200) GAUCON,FRAME,WGTMIN*100.0D0

        IF (LSTATE .EQ. 1) THEN
           COMPMX = 0.0D0  !  Maximum component
           LST = 0
           DO 530 J = 1, IVIRTUL
              COMP = REAL(ABS (HAMATRE(LSTATE,J)), KIND=16)                                     !  ABS
              IF (COMP .GT. COMPMX) THEN                                                        ! ENERHARM
                 COMPMX = COMP
                 ENERCI(LSTATE) = REAL(EIGVAL(  J), KIND=16)
                 LST = J
              ENDIF
  530      CONTINUE
           IASSIG(LST) = 1
        ELSE
           COMPMX = 0.0D0
           LST = 0
           DO 540 J = 1, IVIRTUL
              IF (IASSIG(J) .EQ. 1) CYCLE
              DIFF = EIGVAL(J) - ENERHARM(LSTATE)
              DELTA = DIFF                                                        
              WEIGHT = EXP (-GAUCON * DELTA ** 2)                                 !  EXP
              WEIGHT = MAX (WEIGHT, WGTMIN)                                       !  MAX
              COMP   = WEIGHT * REAL(ABS (HAMATRE(LSTATE,J)),KIND=16)           !  ABS
              IF (COMP .GT. COMPMX) THEN                              
                 COMPMX = COMP
                 ENERCI(LSTATE) = REAL(EIGVAL(J), KIND=16)
                 LST = J
              ENDIF
  540      CONTINUE
           IASSIG(LST) = 1
        ENDIF
        
        DIFF = ENERCI(LSTATE) - ENERHARM(LSTATE)
        WRITE (NOUT,3210) IVIRTUL,ENERHARM(LSTATE),DIFF, ENERCI(LSTATE)

        CALL LWATCHTX (LSPLIT1,TIMTXT)
        WRITE (NOUT,3220) TIMTXT
        
 3200   FORMAT (/'Assignment of matrix eigenvalues is performed using ',      &     !  3200
       &     'the Gaussian weighting:'/                                       &
       &     'Weight = max(exp(-Gauss*|E(CI)-E(PT)|^2), Wmin), ',             &
       &     'Gauss = -ln(Wmin)/Range^2,'/                                    &
       &     'Parameter Gauss =',F12.8,', Parameter Range = ',F6.1,           &
       &     ', and Wmin =',F6.2,' %')
 3210   FORMAT (/'Vibrational Configuration Interaction Matrix ',             &
       &     'have been produced:'//                                          &
       &     'The number of basis functions (Nvirt.) =',I8//                  &
       &     'H(0) Energy =',F24.16/'Difference  =',F24.16/                   &
       &     'VCI  Energy =',F24.16)
3220    FORMAT (/'VCI state calculation was accomplished in ',A16)         
       
!-----
        CALL LWATCH (LSPLIT2)                                                     !  LWATCH ()
        WRITE (NOUT,3230) MAX_PT
3230    FORMAT (/70('-')//'Rayleigh-Schrodinger Perturbation Theory, ',           &     !  3300
     &   'Maximum order =',I4)
!-----------------------------------------------------------------------------------------------
!      Verify Degeneracy
!-----------------------------------------------------------------------------------------------       
        NDEG = 0 
        DO I = 1, IVIRTUL
            EZERO = REAL(HARMTRAN(I,I), KIND = 16)
            IF (ABS(ENERHARM(LSTATE)- EZERO) .LT. 1.0E-6 ) NDEG = NDEG + 1                  ! The First One is always the state itself
        ENDDO                                                                               ! 1.0E-3 Should be improved
        
        IF (NDEG > 1) THEN
            ALLOCATE(JQUANT(NQ, NDEG), IDEG(NDEG))
            NDEG = 0
            DO I = 1, IVIRTUL
                EZERO = REAL(HARMTRAN(I,I), KIND = 16)
                IF (ABS(ENERHARM(LSTATE)- EZERO ) .LT. 1.0E-6 ) THEN                        ! 1.0E-3 Should be improved
                    NDEG = NDEG + 1
                    JQUANT(:,NDEG) = IVIRTU(:,I)                                            ! Record Degenerate States
                    IDEG(NDEG)     = I                                                      ! Record The indecies of Degenerate States
                ENDIF
            ENDDO
            WRITE(*   , 3300) NDEG
            WRITE(NOUT, 3300) NDEG
            DO I = 1, NDEG
                IF (IDEG(I) .EQ. LSTATE) CYCLE
                WRITE(NOUT, 3310) NQ, (JQUANT(Q,I),Q=1,NQ) !TRIM(QUANID)
            ENDDO
            DEALLOCATE(JQUANT)
        ELSE 
            WRITE(*   , 3320)
            WRITE(NOUT, 3320)
        ENDIF
3300    FORMAT (/'The reference state is ', I4,' - fold degenereate ' &
       &       /'Zero order state conicides with ')
3310    FORMAT ('v(  1 -',I3,') =  ', 20I3)
3320    FORMAT (/'The reference state is nondenegerate ')    
!-----------------------------------------------------------------------------------------------         
!     Nondegenerate  RSPT Loop 
!-----------------------------------------------------------------------------------------------                 
        IF (NDEG .EQ. 1) THEN
        !--------------------------------------------------------------------!
        !     Evaluate zero-order RSPT(0) energy.                            !
        !                                                                    !
        !                  (0)        (0)                                    !
        !     Initialize: C    = 1,  C    = 0.                               !
        !                  ll        k/=l                                    !
        !--------------------------------------------------------------------!
            NRSPT = 0   !  RSPT order: ZCOEF(NTZERO), LADZER(NTZERO)

            ALLOCATE (EPTORD(0:MAX_PT), STAT = IERR)
            LOCERR = 16;  IF (IERR .NE. 0) GO TO 990
            ALLOCATE (PSIFUN(0:MAX_PT,1:IVIRTUL), STAT = IERR)
            LOCERR = 17;  IF (IERR .NE. 0) GO TO 990      
            
            EPTORD = 0.0D0
            EPTORD(NRSPT) = REAL(HARMTRAN(LSTATE,LSTATE), KIND = 16)
            PSIFUN = 0.0D0            
            PSIFUN(NRSPT,LSTATE) = (1.0D0, 0.0D0)        !  C(0)kl = Delta(k,l)
       !---------------------------------------------------------------------!
       !     First order of the perturbation theory: a special case          !
       !                                                                     !
       !      (1)      M   N   (0)            M   N   (0)                    !
       !     E    = < PRO SUM D    X   | V | PRO SUM D    X   >              !
       !      l       m=1 j=1  lmj  jm       m=1 j=1  lmj  jm                !
       !                                                                     !
       !---------------------------------------------------------------------!
            NRSPT = 1   !  RSPT order: VCOEF(NTPERT), LADPER(NTPERT)
            
            EPTORD(1) = REAL(VPERTRAN(LSTATE, LSTATE), KIND = 16)               ! Diagonal Term of Vpert is always Real
            ENERPT(LSTATE) = EPTORD(0) + EPTORD(1)
            
            WRITE (*,   3550) NRSPT,EPTORD(NRSPT)
            WRITE (NOUT,3560) EPTORD(0),EPTORD(1)
3550        FORMAT ('Energy Correction in ',I2,'-order = ',2F20.12,' vs. VCI')
3560        FORMAT (/'Zero-Order (Harmonic approx.)  = ',F24.16/                &
     &               'Energy correction (1st order)  = ',F24.16)
       !---------------------------------------------------------------------!
       !     Integrate the term V(s) over all modes Q(m) and find C(1)_lk,ll !
       !                                                                     !
       !                M       (0)        M       (0)                       !
       !             < PRO psi(m)   | V | PRO psi(m)   >                     !
       !      (1)      m=1       k        m=1       l       (1)              !
       !     C     = -----------------------------------;  C    = 0.         !
       !      k/=l                (0)    (0)                ll               !
       !                       ( E    - E   )                                !
       !                          l      k                                   !
       !                                                                     !
       !     NOTE: k = virtual, l = level.    Eq. (2.29) Shavitt, Bartlett   !
       !---------------------------------------------------------------------!
            IF (LOUT .GT. 1) WRITE (NOUT,3600)
            
            PSIFUN(NRSPT,LSTATE) = (0.0D0,0.0D0)                                            
            
            K = 0
            DO 650 KSTATE = 1, IVIRTUL                                                  
               IF (KSTATE .EQ. LSTATE) CYCLE                                          
               ELEMAT = VPERTRAN(KSTATE, LSTATE)
               DENOM = EPTORD(0) - REAL(HARMTRAN(KSTATE,KSTATE), KIND = 16)
               TERM = ELEMAT / CMPLX(DENOM,0.0D0, KIND = 8)                                         ! Chang type of variable
               PSIFUN(NRSPT,KSTATE) =  TERM  !PSIFUN(NRSPT,KSTATE) +
            
               IF (LOUT .GT. 1 .AND. KSTATE .LE. LIMIT .AND. ABS (ELEMAT) .GT. TOLCUT) THEN          !  ABS
                  K = K + 1
                  WRITE (NOUT,3610) K,KSTATE,REAL(ELEMAT), IMAG(ELEMAT) ,DENOM
                  WRITE (NOUT,3620) KSTATE,(IVIRTU(Q,KSTATE),Q=1,NQ)
               ENDIF
650         CONTINUE

3600        FORMAT (/'Evaluate the first-order correction to ',                                  &     !  3600
     &         'the wave function:')
3610        FORMAT (I4,'. < State | V | Virt.(',I6,') > =', F16.8, '+',F16.8, 'I',               &
     &         ',  Denom = ',F16.8)
3620        FORMAT (20X,'Virt.(',I6,'):',10I4/(34X,10I4))
     
            IF (LOUT .GT. 1) THEN
               WRITE (NOUT,3630)
               ANORM = 0.0D0
               SCPRO = 0.0D0
               K = 0
               DO 660 KSTATE = 1, IVIRTUL
                  ANORM = ANORM + REAL(ABS(PSIFUN(NRSPT,KSTATE)),KIND=16) ** 2
                  SCPRO = SCPRO + PSIFUN(NRSPT,KSTATE) * PSIFUN(0,KSTATE)
                  IF (LOUT .GT. 1 .AND. KSTATE .LE. LIMIT .AND.                 &
     &               ABS (PSIFUN(NRSPT,KSTATE)) .GT. 0.0D0)   THEN                    !  ABS
                        WRITE (NOUT,3640) KSTATE, REAL(PSIFUN(NRSPT,KSTATE)), IMAG(PSIFUN(NRSPT,KSTATE))
                        WRITE (NOUT,3650) KSTATE,(IVIRTU(Q,KSTATE),Q=1,NQ)
                  ENDIF
  660          CONTINUE
               ANORM = SQRT (ANORM)                                                   !  SQRT
               WRITE (NOUT,3660) ANORM,SCPRO
            ENDIF
        
 3630       FORMAT (/'Virtual states coefficients, C(  1)k:')                                       
 3640       FORMAT (I4,'. C(1)k = < Psi(0)k | Psi(1) >  = ',F24.16,'+',F24.16,'I')
 3650       FORMAT (6X,'Virt. k(',I6,'):',10I4/(34X,10I4))
 3660       FORMAT (/'Wave function vector norm      = ',F24.16/                &
     &               'Scalar Product <Psi(0)|Psi(1)> = ',F24.16/)            
       !---------------------------------------------------------------------!
       !     Second (and subsequent) order(s) of the perturbation theory:    !
       !                                                                     !
       !      (n)   vir      (0)         (0)   (n-1)                         !
       !     E    = SUM < Psi   | V | Psi   > C                              !
       !      l      k       l           k     kl                            !
       !---------------------------------------------------------------------!
            WRITE (*,4000) MAX_PT                                                       !,TOLOUT
4000        FORMAT (/'Rayleigh-Schrodinger PT corrections, ',                  &     
     &         'max order =',I3,', Tol. = 1.0E-6            ',/                 &
     &         'Indicated errors are differencies against the VCI method.'/)
            
            CONVER = .FALSE.
            DO 800 NRSPT = 2, MAX_PT
            
            ICENT0 = 0
            CALL WATCH  (ISPLIT)
            WRITE (*,4010) NRSPT
            CALL COUNTER (0,0,1)

            SUMVIR = 0.0D0
            DO 720 KSTATE = 1, IVIRTUL                                                  
               IF (KSTATE .EQ. LSTATE) CYCLE                                          
            
               ICENT1 = 100 * KSTATE / IVIRTUL
               IF (ICENT1 .NE. ICENT0) THEN
                  CALL COUNTER (1,ICENT1,1)
                  ICENT0 = ICENT1
               ENDIF

               ELEMAT = VPERTRAN(LSTATE, KSTATE)
               IF (ELEMAT .EQ. 0.0D0) CYCLE
            
               TERM = ELEMAT * PSIFUN(NRSPT-1,KSTATE)
               SUMVIR = SUMVIR + TERM                                            ! Chang type of variable
  720       CONTINUE
            
            CALL COUNTER (1,-1,1)
            CALL WATCH2 (ISPLIT)
            
!----          Calculation of the energy correction E(n) is accomplished:
            EPTORD(NRSPT) = REAL(SUMVIR, KIND = 16)                              ! Chang type of variable
!----          Correct the energy level at the Rayleigh Shrodinger PT n-th order:
            ENERPT(LSTATE) = ENERPT(LSTATE) + EPTORD(NRSPT)
            
            DIFF = ENERPT(LSTATE) - ENERCI(LSTATE)
            EOUT = EPTORD(NRSPT)
            
            IF (ABS (EOUT) .LT. 1.0E+4) THEN                                
               WRITE (NOUT,4030) NRSPT,EOUT,DIFF
               WRITE (*   ,4050) NRSPT,EOUT,DIFF
            ELSE
               WRITE (NOUT,4040) NRSPT,EOUT
               WRITE (*   ,4060) NRSPT,EOUT
            ENDIF
            IF (NRSPT .EQ. 2)  WRITE (NOUT,4070) ENERPT(LSTATE),DIFF
            

4010        FORMAT ('Evaluate RSPT Energy,  E(',I3,')  ...'\)
4030        FORMAT ('Energy correction, order (',I3,') = ',F24.16,              &
            &         '  (',F24.16,')')
4040        FORMAT ('Energy correction, order (',I3,') = ',ES24.12)
4050        FORMAT ('Energy Correction, ',I3,'-order = ',F16.10,                &
            &         ', Error =',F16.10)
4060        FORMAT ('Energy Correction, ',I3,'-order = ',ES16.9)
4070        FORMAT ('Total Energy, After 2nd order  = ',F24.16,                 &
            &         '  (',F24.16,') Error vs. VCI')
       !----------------------------------------------------------------------!
       !    Integrate the term V(s) over all modes Q(k) and find C(n)kl       !
       !                                                                      !
       !     (n)     / vir      (0)         (0)   (n-1)   n-1  (i)  (n-i) \   !
       !    C      = | SUM < Psi   | V | Psi   > C      - SUM E    C      | * !
       !     k/=l    \  m       k           m     ml      i=1  l    kl    /   !
       !                                                                      !
       !                (0)    (0)  -1    (n)                                 !
       !           * ( E    - E    )  .  C    = 0.                            !
       !                l      k          ll                                  !
       !----------------------------------------------------------------------!
            PSIFUN(NRSPT,LSTATE) = 0.0D0                                       

            ICENT0 = 0
            CALL WATCH (ISPLIT)
            WRITE (*,4100)
            CALL COUNTER (0,0,1)                                                      
            
            DO 770 KSTATE = 1, IVIRTUL
               IF (KSTATE .EQ. LSTATE) CYCLE
            
               ICENT1 = 100 * KSTATE / IVIRTUL
               IF (MOD (ICENT1, 10) .EQ. 0) THEN
                  CALL COUNTER (1,ICENT1,1)                                           
                  ICENT0 = ICENT1
               ENDIF
            
!----             Calculate the sum over virtual states <k|V|m>C^(n-1)_m
               SUMVIR = 0.0D0
               DO 750 MSTATE = 1, IVIRTUL                                            
                  IF (MSTATE .EQ. LSTATE) CYCLE                                    
!----                SUM[m=1..vir] < Psi^0(k) | V | Psi^0(m) >
                  ELEMAT = VPERTRAN(KSTATE, MSTATE)
                  IF (ELEMAT .EQ. 0.0D0) CYCLE
                  TERM = ELEMAT * PSIFUN(NRSPT-1,MSTATE)
                  SUMVIR = SUMVIR + TERM
750            CONTINUE
!----             Calculate the second sum over N_order = 1, Max_order - 1
               SUMORD = 0.0D0
               DO 760 IORD = 1, NRSPT - 1
                  TERM = CMPLX(EPTORD(IORD), KIND = 8) * PSIFUN(NRSPT-IORD,KSTATE)  ! Chang type of variable
                  SUMORD = SUMORD + TERM
760            CONTINUE
            
               DENOM = EPTORD(0) - REAL(HARMTRAN(KSTATE,KSTATE), KIND = 16)
            
               TERM = (SUMVIR - SUMORD) / CMPLX(DENOM, KIND=8)                      ! Chang type of variable
               PSIFUN(NRSPT,KSTATE) = TERM
770         CONTINUE
            
            CALL COUNTER (1,-1,1)
            CALL WATCH2 (ISPLIT)
            
            IF (LOUT .GT. 1) THEN
               WRITE (NOUT,4110) NRSPT
               ANORM = 0.0D0
               K = 0
               DO 790 KSTATE = 1, IVIRTUL
                  ANORM = ANORM + REAL(ABS(PSIFUN(NRSPT,KSTATE)),KIND = 16) ** 2
                  IF (KSTATE .LE. LIMIT .AND.                                   &
     &               ABS (PSIFUN(NRSPT,KSTATE)) .GT. TOLCUT) THEN                  !  ABS
                     K = K + 1
                     WRITE (NOUT,4120) K,REAL(PSIFUN(NRSPT,KSTATE)),IMAG(PSIFUN(NRSPT,KSTATE))
                     WRITE (NOUT,4130) KSTATE,(IVIRTU(Q,KSTATE),Q=1,NQ)
                  ENDIF
790            CONTINUE

               ANORM = SQRT (ANORM)                                                  
               WRITE (NOUT,4140) ANORM
            ENDIF            

4100        FORMAT ('Wave function expansion coefficients -- '\)                      
4110        FORMAT (/'Virtual states coefficients, C(',I3,')k:--')
4120        FORMAT (I4,'.  C(n)k = < Psi(0)k | Psi(n) >  = ',F24.16,'+', F24.16,'I')
4130        FORMAT (6X,'Virt. k(',I6,'):',10I4/(34X,10I4))
4140        FORMAT (/'Norm of the coefficients vector =',F20.12/)
       !----------------------------------------------------------------------!
       !     Print calculated wave function expansion coefficients.           !
       !     Loop over higher orders of perturbation theory                   !
       !----------------------------------------------------------------------!
!----          Check for convergence, reserve option
            IF (ABS (EPTORD(NRSPT)) .LT. REAL (TOLOUT, KIND = 8)) THEN                !  ABS
               CONVER = .TRUE.
            ENDIF
            
800         CONTINUE  !  RSPT Orders, NRSPT = 2 .. MAX_PT
       !=======================================================================
       !     Exit from Perturbation Theory Orders
       !=======================================================================
            EOUT = ENERPT(LSTATE)
            DIFF = ENERPT(LSTATE) - ENERCI(LSTATE)
            IF (ABS (EOUT) .LT. 1.0E+6) THEN
               WRITE (NOUT,4210) ENERPT(LSTATE),DIFF
            ELSE
               WRITE (NOUT,4220) ENERPT(LSTATE),DIFF
            ENDIF
        
4210        FORMAT ('Total Energy, After Summation  = ',F24.16,                 &     !  4200
     &         '  (',F24.16,') Error vs. VCI')
4220        FORMAT ('Total Energy, After Summation  = ',ES24.16,                &
     &   '  (',ES24.16,') Error vs. VCI')
!-----
            CALL LWATCHTX (LSPLIT2,TIMTXT)
            WRITE (NOUT,5400) TIMTXT      
5400        FORMAT (/'RSPT(n) state calculation was accomplished in ',A16)            !  5400
            
            DEALLOCATE(PSIFUN, EPTORD)
        ENDIF
!-----------------------------------------------------------------------------------------------                 
!     Degenerate  RSPT Loop        
!-----------------------------------------------------------------------------------------------    
!     STEP 1 :
!        
!        Solve Secular Equation to Obtain the FirstOrder Correction 
!        of Energy and Wavefunction in Degenerate Case;
!        Record the First Order Energy and Zero - Order Wavefunction. 
!        
!        NDEG      is the total number of Degenerate States  
!        IDEG(I)   corresponds the index of the degenerate state in the total space
!        INDEX     is the index of Reference State in Degenerate Subspace
!        LSTATE    is the index of Reference State in the total space    
!        JQUANT(I) records the quantum number of each degenerate state
!-----------------------------------------------------------------------------------------------        
        
        IF (NDEG > 1) THEN    
            DO I = 1, NDEG
                IF (LSTATE .EQ. IDEG(I)) EXIT
            ENDDO
            INDEX = I                                                            ! Record Index of State among Degenerate Subspace
            IF (INDEX .EQ. 1 )WRITE(NOUT," ('The reference state is the', I3, '-st state among the degenerate pair ' )" ) INDEX
            IF (INDEX .EQ. 2 )WRITE(NOUT," ('The reference state is the', I3, '-nd state among the degenerate pair ' )" ) INDEX
            IF (INDEX .GT. 2 )WRITE(NOUT," ('The reference state is the', I3, '-th state among the degenerate pair ' )" ) INDEX
            
            ALLOCATE(VPERTDEG(NDEG,NDEG))                                        ! Copy Matrix Element of Perturbation in Degenerate Subspace   
            DO I = 1, NDEG
                DO J = 1, NDEG
                    VPERTDEG(I,J) = VPERTRAN(IDEG(I), IDEG(J))                   ! Vpertran is double complex, but it's every element is real(double)
                ENDDO
            ENDDO
            
            WRITE(NOUT," ('Pertubation Matrix has following form in Degenerate Subspace :'/)" )
            DO I = 1, NDEG
                WRITE(UNIT = LINEMAT, FMT = "('|',20(1XF10.5,' +', F10.5,'I '):)") (REAL(VPERTDEG(I, J)),IMAG(VPERTDEG(I, J)), J = 1 ,NDEG)  
                LINEMAT = TRIM(LINEMAT) // '|'
                WRITE(NOUT, '(A)' ) LINEMAT 
            ENDDO 
            !----- Diagolize SubMatrix                                           ! Solve the Secular equation to obtain first order correction
!            LWORK = MAX(1, 3*NDEG - 1)
!            ALLOCATE(RWORK(LWORK), ENERDEG(NDEG) )
!            CALL DSYEV( "V", "U", NDEG, VPERTDEG, NDEG, ENERDEG, RWORK, LWORK, INFO)
!            DEALLOCATE(RWORK)
            LWORK  = NDEG*(NDEG+2)
            LRWORK = 2*NDEG**2+5*NDEG+1
            LIWORK = 5*NDEG + 3
            ALLOCATE (WORK(LWORK), RWORK(LRWORK), IWORK(LIWORK), ENERDEG(NDEG))
            CALL ZHEEVD('V', 'U', NDEG, VPERTDEG, NDEG, ENERDEG, WORK, LWORK, RWORK, LRWORK, IWORK, LIWORK, INFO)
            DEALLOCATE(WORK, RWORK, IWORK)
           
            WRITE(NOUT," ('Eigensystem of Submatrix was Calculated :'/)" )
            DO I = 1, NDEG
                WRITE(NOUT,"('Index : ',I4, '  Eigenvalue   : ', F10.5)" )  I ,ENERDEG(I)
                WRITE(NOUT,"('Corresponding Eigenvectors : ' , 20(F10.5))" ) (VPERTDEG(J, I), J = 1, NDEG)
            ENDDO
        !---------------------------------------------------------------------!
        !                  (0)                 (EIG.VEC.)                     !
        !     Initialize: C                 = C          ,  I = 1, NDEG       !
        !                  IDEG(I), LSTATE     I, INDEX                       !
        !                                                                     !
        !                                                                     !
        !                  (0)                                                !
        !                 C          = 0.                                     !
        !                  K/=LSTATE                                          !
        !---------------------------------------------------------------------!
        !                                    (0)       NDEG  (EIG.VEC.)       !
        !    Wavefunction has the form:   Psi       =  SUM  C          | I  > !
        !                                    LSTATE    I=1   I, INDEX         !
        !                                                                     !  
        !                                             VIRTUL    (0,L)         !
        !                                           =  SUM     C       | K  > !
        !                                              K=1      K             !
        !---------------------------------------------------------------------!
            NRSPT = 0   !  RSPT order: ZCOEF(NTZERO), LADZER(NTZERO)

            ALLOCATE (EPTORD(0:MAX_PT), STAT = IERR)
            ALLOCATE (PSIFUN(0:MAX_PT,1:IVIRTUL), STAT = IERR)
            
            EPTORD = 0.0D0
            EPTORD(NRSPT) = REAL(HARMTRAN(LSTATE,LSTATE),KIND=16)
        
            PSIFUN = (0.0D0,0.0D0)          
            !PSIFUN(NRSPT,LSTATE) = 1.0D0                                          !  C(0)kl = Delta(k,l)
            DO I = 1, NDEG 
                PSIFUN(NRSPT,IDEG(I)) = VPERTDEG(I,INDEX)                          ! Chang type of variable
            ENDDO
            WRITE(NOUT,"(/'Eigenvector will be used for the following calculation.')" )
       !----------------------------------------------------------------------!
       !                                                                      !
       !     First order of the perturbation theory in degenerate case is     !
       !     one of the Eigenvalues of Perturbation Submatrix                 !
       !                                                                      !
       !     NOTE:                                                            !      
       !     The Perturbation Submatrix is diagonal in the new Wavefunction   !  
       !     Psi(0), which corresponds analogous in nondegenerate case        !     
       !----------------------------------------------------------------------!
            NRSPT = 1   !  RSPT order: VCOEF(NTPERT), LADPER(NTPERT)
            
            EPTORD(1) = REAL(ENERDEG(INDEX), KIND = 16)
            ENERPT(LSTATE) = EPTORD(0) + EPTORD(1)
            
            WRITE (*,   3550) NRSPT,EPTORD(NRSPT)
            WRITE (NOUT,3560) EPTORD(0),EPTORD(1)
6550        FORMAT ('Energy Correction in ',I2,'-order = ',2F20.12,' vs. VCI')
6560        FORMAT (/'Zero-Order (Harmonic approx.)  = ',F24.16/                &
     &               'Energy correction (1st order)  = ',F24.16)
       !----------------------------------------------------------------------!
       !     Integrate the term V(s) over all modes Q(m) and find C(1)_lk,ll  !
       !                                                                      !
       !                   (0)         (0)           (0)        VIRTUL (0,L)  !
       !             <  Psi   | V | Psi   >    <  Psi   | V |   SUM   C    |m>!
       !      (1)           k           l             k         m=1     m     !
       !     C     = ----------------------- = -------------------------------!
       !      k/=IDEG       (0)    (0)                (0)    (0)              !
       !                 ( E    - E   )            ( E    - E   )             !
       !                    l      k                  l      k                !
       !                                                                      !
       !      (1)                                                             !
       !     C       = 0.                                                     !
       !      k=IDEG                                                          !
       !                                                                      !
       !     NOTE: k = virtual, l = LSTATE.                                   !
       !     If k is one of the Degenerate State, i.e. k = IDEG(I) I=1, NDEG  !
       !     then C(1) = 0                                                    !
       !----------------------------------------------------------------------!
            IF (LOUT .GT. 1) WRITE (NOUT,6600)
            
            PSIFUN(NRSPT,:) = (0.0D0,0.0D0)                                         
            
            K = 0
            LOOP1: DO  KSTATE = 1, IVIRTUL                                                  
                
                LOOP2: DO I = 1, NDEG
                    IF (KSTATE .EQ. IDEG(I)) CYCLE LOOP1  
                ENDDO LOOP2
               
                SUMVIR = (0.0D0,0.0D0)
                DO MSTATE = 1, IVIRTUL
                    ELEMAT = VPERTRAN(KSTATE, MSTATE)
                    IF (ELEMAT .EQ. (0.0D0,0.0D0)) CYCLE
                    SUMVIR = SUMVIR + ELEMAT * PSIFUN(0,MSTATE)
                ENDDO                
                
                DENOM = EPTORD(0) - REAL(HARMTRAN(KSTATE,KSTATE), KIND = 16)
                TERM = SUMVIR / CMPLX(DENOM,0.0D0, KIND=8)                             ! Chang type of variable
                PSIFUN(NRSPT,KSTATE) =  TERM
        
                IF (LOUT .GT. 1 .AND. KSTATE .LE. LIMIT .AND. &
                    &           ABS (PSIFUN(NRSPT,KSTATE)) .GT. TOLCUT) THEN    
                  K = K + 1
                  WRITE (NOUT,6610) K,KSTATE,REAL(SUMVIR), IMAG(SUMVIR)
                  WRITE (NOUT,6620) KSTATE,(IVIRTU(Q,KSTATE),Q=1,NQ)
                ENDIF
           ENDDO LOOP1

6600        FORMAT (/'Evaluate the first-order correction to ',                 &     !  3600
     &         'the wave function:')
6610        FORMAT (I4,'. < State | V | Virt.(',I6,') > =',F16.8, '+',F16.8, 'I')
6620        FORMAT (20X,'Virt.(',I6,'):',10I4/(34X,10I4))
     
            IF (LOUT .GT. 1) THEN
               WRITE (NOUT,6630)
               ANORM = 0.0D0
               SCPRO = 0.0D0
               K = 0
               DO KSTATE = 1, IVIRTUL
                  ANORM = ANORM + REAL(ABS(PSIFUN(NRSPT,KSTATE)),KIND=16) ** 2        ! Chang type of variable
                  SCPRO = SCPRO + PSIFUN(NRSPT,KSTATE) * PSIFUN(0,KSTATE)
                  IF (LOUT .GT. 1 .AND. KSTATE .LE. LIMIT .AND.                 &
     &               ABS (PSIFUN(NRSPT,KSTATE)) .GT. 0.0D0)   THEN                    !  ABS
                        WRITE (NOUT,6640) KSTATE,REAL(PSIFUN(NRSPT,KSTATE)),IMAG(PSIFUN(NRSPT,KSTATE))
                        WRITE (NOUT,6650) KSTATE,(IVIRTU(Q,KSTATE),Q=1,NQ)
                  ENDIF
               ENDDO
               ANORM = SQRT (ANORM)                                                   !  SQRT
               WRITE (NOUT,6660) ANORM,SCPRO
            ENDIF
        
6630        FORMAT (/'Virtual states coefficients, C(  1)k:')                                       
6640        FORMAT (I4,'. C(1)k = < Psi(0)k | Psi(1) >  = ',F24.16,'+',F24.16,'I')
6650        FORMAT (6X,'Virt. k(',I6,'):',10I4/(34X,10I4))
6660        FORMAT (/'Wave function vector norm      = ',F24.16/                &
     &               'Scalar Product <Psi(0)|Psi(1)> = ',F24.16/)            
       !---------------------------------------------------------------------!
       !     Higher order(s) contribution of the perturbation theory:        !
       !                                                                     !
       !      (n)         (0)     vir  (n-1)    (0)                          !
       !     E    =  < Psi   | V  SUM C     | Psi   >                        !
       !      l           l        k   kl        k                           !
       !                                                                     !        
       !            VIRTUL  (0,L) vir   (n-1)                                !
       !          =  SUM   C      SUM  C      < M | V | K >                  !
       !             M=1    M      k    kl                                   !
       !---------------------------------------------------------------------!
            WRITE (*,7000) MAX_PT                                                       !,TOLOUT
7000        FORMAT (/'Rayleigh-Schrodinger PT corrections, ',                  &     
     &         'max order =',I3,', Tol. = 1.0E-6            ',/                 &
     &         'Indicated errors are differencies against the VCI method.'/)
            
            CONVER = .FALSE.
            DO NRSPT = 2, MAX_PT
            
            ICENT0 = 0
            CALL WATCH  (ISPLIT)
            WRITE (*,6010) NRSPT
            CALL COUNTER (0,0,1)

            SUMVIR = (0.0D0,0.0D0)
            DO  KSTATE = 1, IVIRTUL                                                  
                ICENT1 = 100 * KSTATE / IVIRTUL
                IF (ICENT1 .NE. ICENT0) THEN
                    CALL COUNTER (1,ICENT1,1)
                    ICENT0 = ICENT1    
                ENDIF
                DO MSTATE = 1, IVIRTUL
                    ELEMAT = VPERTRAN(MSTATE, KSTATE)
                    IF (ELEMAT .EQ. (0.0D0,0.0D0)) CYCLE
                    TERM = ELEMAT * CONJG(PSIFUN(0,MSTATE))* PSIFUN(NRSPT-1,KSTATE)
                    SUMVIR = SUMVIR + TERM
                ENDDO
            ENDDO
            
            CALL COUNTER (1,-1,1)
            CALL WATCH2 (ISPLIT)
            
!----          Calculation of the energy correction E(n) is accomplished:
            EPTORD(NRSPT) = REAL(SUMVIR, KIND =16)                                  ! Chang type of variable
!----          Correct the energy level at the Rayleigh Shrodinger PT n-th order:
            ENERPT(LSTATE) = ENERPT(LSTATE) + EPTORD(NRSPT)
            
            DIFF = ENERPT(LSTATE) - ENERCI(LSTATE)
            EOUT = EPTORD(NRSPT)
            
            IF (ABS (EOUT) .LT. 1.0E+4) THEN                                
               WRITE (NOUT,6030) NRSPT,EOUT,DIFF
               WRITE (*   ,6050) NRSPT,EOUT,DIFF
            ELSE
               WRITE (NOUT,6040) NRSPT,EOUT
               WRITE (*   ,6060) NRSPT,EOUT
            ENDIF
            IF (NRSPT .EQ. 2)  WRITE (NOUT,6070) ENERPT(LSTATE),DIFF
            

6010        FORMAT ('Evaluate RSPT Energy,  E(',I3,')  ...'\)
6030        FORMAT ('Energy correction, order (',I3,') = ',F24.16,              &
            &         '  (',F24.16,')')
6040        FORMAT ('Energy correction, order (',I3,') = ',ES24.12)
6050        FORMAT ('Energy Correction, ',I3,'-order = ',F16.10,                &
            &         ', Error =',F16.10)
6060        FORMAT ('Energy Correction, ',I3,'-order = ',ES16.9)
6070        FORMAT ('Total Energy, After 2nd order  = ',F24.16,                 &
            &         '  (',F24.16,') Error vs. VCI')            
       !----------------------------------------------------------------------!
       !    Integrate the term V(s) over all modes Q(k) and find C(n)kl       !
       !                                                                      !
       !     (n)     / vir      (0)         (0)   (n-1)   n-1  (i)  (n-i) \   !
       !    C      = | SUM < Psi   | V | Psi   > C      - SUM E    C      | * !
       !     k/=IDEG \  m       k           m     ml      i=1  l    kl    /   !
       !                                                                      !
       !                (0)    (0)  -1                                        !
       !           * ( E    - E    )                                          !
       !                l      k                                              !
       !      (n)                                                             !
       !     C       = 0.                                                     !
       !      k=IDEG                                                         !
       !                                                                      !
       !     NOTE: k = virtual, l = LSTATE.                                   !
       !     If k is one of the Degenerate State, i.e. k = IDEG(I) I=1, NDEG  !
       !     then C(1) = 0                                                    !
       !----------------------------------------------------------------------!            
            PSIFUN(NRSPT,:) = (0.0D0,0.0D0)                                      

            ICENT0 = 0
            CALL WATCH (ISPLIT)
            WRITE (*,6100)
            CALL COUNTER (0,0,1)                                                      
            
            LOOP3: DO KSTATE = 1, IVIRTUL
                LOOP4: DO I = 1, NDEG
                    IF (KSTATE .EQ. IDEG(I)) CYCLE LOOP3  
                ENDDO LOOP4
            
                ICENT1 = 100 * KSTATE / IVIRTUL
                IF (MOD (ICENT1, 10) .EQ. 0) THEN
                    CALL COUNTER (1,ICENT1,1)                                           
                    ICENT0 = ICENT1
                ENDIF
!----             Calculate the sum over virtual states <k|V|m>C^(n-1)_m
               SUMVIR = 0.0D0
               LOOP5: DO MSTATE = 1, IVIRTUL      
!                  DO I = 1, NDEG 
!                      IF (MSTATE .EQ. LSTATE) CYCLE  LOOP5                                   
!                  ENDDO    
!----                SUM[m=1..vir] < Psi^0(k) | V | Psi^0(m) >
                  ELEMAT = VPERTRAN(KSTATE ,MSTATE)
                  IF (ELEMAT .EQ. (0.0D0,0.0D0)) CYCLE
                  TERM = ELEMAT * PSIFUN(NRSPT-1,MSTATE)
                  SUMVIR = SUMVIR + TERM
               ENDDO LOOP5
!----             Calculate the second sum over N_order = 1, Max_order - 1
               SUMORD = (0.0D0,0.0D0)
               DO IORD = 1, NRSPT - 1
                  TERM = CMPLX(EPTORD(IORD),0.0D0,KIND=8) * PSIFUN(NRSPT-IORD,KSTATE)     ! Chang type of variable
                  SUMORD = SUMORD + TERM
               ENDDO
            
               DENOM = EPTORD(0) - REAL(HARMTRAN(KSTATE,KSTATE), KIND = 16)
               TERM = (SUMVIR - SUMORD) / CMPLX(DENOM,0.0D0, KIND=8)
               PSIFUN(NRSPT,KSTATE) = TERM
            ENDDO LOOP3
            
            CALL COUNTER (1,-1,1)
            CALL WATCH2 (ISPLIT)
            
            IF (LOUT .GT. 1) THEN
               WRITE (NOUT,6110) NRSPT
               ANORM = 0.0D0
               K = 0
               DO KSTATE = 1, IVIRTUL
                  ANORM = ANORM + REAL(ABS(PSIFUN(NRSPT,KSTATE)),KIND=16) ** 2
                  IF (KSTATE .LE. LIMIT .AND.                                   &
     &               ABS (PSIFUN(NRSPT,KSTATE)) .GT. TOLCUT) THEN                  !  ABS
                     K = K + 1
                     WRITE (NOUT,6120) K,REAL(PSIFUN(NRSPT,KSTATE)),IMAG(PSIFUN(NRSPT,KSTATE))
                     WRITE (NOUT,6130) KSTATE,(IVIRTU(Q,KSTATE),Q=1,NQ)
                  ENDIF
               ENDDO
               ANORM = SQRT (ANORM)                                                  
               WRITE (NOUT,6140) ANORM
            ENDIF            

6100        FORMAT ('Wave function expansion coefficients -- '\)                      
6110        FORMAT (/'Virtual states coefficients, C(',I3,')k:--')
6120        FORMAT (I4,'.  C(n)k = < Psi(0)k | Psi(n) >  = ',F24.16,'+',F24.16,'I')
6130        FORMAT (6X,'Virt. k(',I6,'):',10I4/(34X,10I4))
6140        FORMAT (/'Norm of the coefficients vector =',F20.12/)
       !----------------------------------------------------------------------!
       !     Print calculated wave function expansion coefficients.           !
       !     Loop over higher orders of perturbation theory                   !
       !----------------------------------------------------------------------!
!----          Check for convergence, reserve option
            IF (ABS (EPTORD(NRSPT)) .LT. REAL (TOLOUT, KIND = 8)) THEN                !  ABS
               CONVER = .TRUE.
            ENDIF
            
            ENDDO  !  RSPT Orders, NRSPT = 2 .. MAX_PT
       !=======================================================================
       !     Exit from Perturbation Theory Orders
       !=======================================================================
            EOUT = ENERPT(LSTATE)
            DIFF = ENERPT(LSTATE) - ENERCI(LSTATE)
            IF (ABS (EOUT) .LT. 1.0E+6) THEN
               WRITE (NOUT,6210) ENERPT(LSTATE),DIFF
            ELSE
               WRITE (NOUT,6220) ENERPT(LSTATE),DIFF
            ENDIF
        
6210        FORMAT ('Total Energy, After Summation  = ',F24.16,                 &     !  4200
     &         '  (',F24.16,') Error vs. VCI')
6220        FORMAT ('Total Energy, After Summation  = ',ES24.16,                &
     &   '  (',ES24.16,') Error vs. VCI')
!-----
            CALL LWATCHTX (LSPLIT2,TIMTXT)
            WRITE (NOUT,6400) TIMTXT      
6400        FORMAT (/'RSPT(n) state calculation was accomplished in ',A16)            !  5400

!-----------------------------------------------------------------------------------------------            
            DEALLOCATE(IDEG,VPERTDEG, ENERDEG)
            DEALLOCATE(PSIFUN, EPTORD)
        ENDIF
        
900   ENDDO
         
      
      

     
9900  FORMAT (/'')  !  <line feed>     
!-----------------------------------------------------------------------------------------------
      WRITE (*,"(/80('-'))")
      PAUSE 'RSPT_L: Normal Termination. Press ENTER for exit: '
      STOP

910   WRITE (*,"('RSPT configuration file not found -- '/A)") FILE_CFG
      STOP
920   WRITE (*,"('Molecular data file not found:'/A)") FILE_MOL
      PAUSE
      STOP
930   WRITE (*,"('Error opening output file:'/A)") FILE_LST
      PAUSE
      STOP
940   WRITE (*,"('Error opening RSPT.param file:'/A)") FILE_INP
      PAUSE
      STOP
950   WRITE (*, "('The number of degree is not coincide with the Number of Ex/Internal Quantum Number : ',I4,'+',I4,' \= ', I4)") NQEX, NQIN, NQ
      PAUSE
      STOP
960   WRITE (*, "(' Basis Sets in different representation can not be matched')")
      PAUSE
      STOP
      
      
990   CONTINUE
      WRITE (*,"('Memory Allocation Error, Codes =',2I6)") LOCERR,IERR
      PAUSE
      STOP

      CLOSE(NINP)
      CLOSE(NOUT)
      CLOSE(NAUX)
      CLOSE(NSCR)
    END PROGRAM RSPT_L
