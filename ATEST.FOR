      PROGRAM TEST
C     sample program for EZUP tests
C     WILL NOT REALLY RUN!  WON'T EVEN COMPILE !!!
C
      character*(*) avalue,a2
      character bvalue(*),ank1(7)
      CHARACTER*8 FAFORM(3)
      CHARACTER*42 OBSURN(10)
      CHARACTER A,KHAR*4, FNAME*12, CARD*127, LABEL(400,5,2),SPC
      LOGICAL OPND,EXST
      DIMENSION QQ(32),AA(32)
      REAL x,y,z
      REAL*4 x1,Y1
      REAL*8 X2,Y2
      COMPLEX zratio,znum,zden
      double precision ABCD, ABDE
      double precision complex zcomp1, zcomp2
      complex*16 zcompd, zvalue
      REAL*4 abcde, ewgdas
      INTEGER a1,a3,a6,a7,a8,a9,a11,a12
      integer k1mk23, k1mk34, k1mk56
c
      common abcde, ewgdas, a1, a3, a6,
     #a7, a8,    a9,
     #a11,a12
c
      common /a1/ k1mk23, k1mk34,k1mk56
      common /trash/ ix, iy, iz
c
c
      DATA IOOUT/6/ IDV /3/ IEND/72/ IBGN/7/ X0/1.E8/ SPC/' '/
      DATA FAFORM(1)/'AVERAGE ' /FAFORM(2)/'THIN    '/FAFORM(3)/'THICK  
     $ '/ MAXNAM/10/
      DATA OBSURN( 1)/'DRY                                       '/OBSUR
     $N(2)/'MOIST                                     '/OBSURN(3)/'WET  
     $                                     '/OBSURN(4)/'FROZEN          
     $                          '/OBSURN(5)/'ICE                        
     $               '/OBSURN(6)/'SNOW (LESS THAN HALF OF SURFACE COVERE
     $D)  '/OBSURN(7)/'SNOW (BETWEEN HALF AND FULL SURFACE COVER)'/OBSUR
     $N(8)/'SNOW (GROUND COMPLETELY COVERED)          '/OBSURN(9)/'LOOSE
     $ DRY/SAND/DUST/SNOW (50-98 % COVER)  '/OBSURN(10)/'LOOSE DRY/SAND/
     $DUST/SNOW (COMPLETE-COVER) '/
C
      do 11111 k=1,12
      DO J=1,31,2
      QQ(J)=j*2.34
      END DO
11111 WRITE(*,*)'LOOK AT WHAT HAPPENS TO THE NEXT LINE!'
c
   5  if(x-x0)7,11,12
      if(xidabc.eq.4) REWIND 22
      if(x.eq.5) call bogus(gripe)
   7  GOTO(13,131,131,131,131,131,131,1310,1310,1310,1310,1310,1310,131
     &0),IDV
      FNAME='TRASH.TXT'
c     the next two lines begin with TABS
	IF(CARD(IPOS:IPOS).EQ.'(') GOTO 21
	READ(8,'(A)')
10    READ(8,'(A)',END=1310) CARD
  11  WRITE(IOOUT,'(A)') ' UNEXPECTED EOF ON INPUT UNIT'
      STOP
  12  WRITE(IOOUT,'(A)') ' A NON EOSAEL FORMAT INPUT CARD ENCOUNTERED'
      WRITE(IOOUT,'(A)') CARD
      STOP
C
C    NOTE ADDITION OF SUBROUTINE UCA
C
   13 CALL UCA(ALPHA1)
C
      DO 20 KK=1,MAXNAM
C     CHECK FOR CARD TYPES
      IF (ALPHA1.NE.RNAMES(KK)) GO TO 21
      INOPT=KK
   20 CONTINUE
21    write(*,*)'HELP!'
      write(ioout,2342) a,b,c
2342  format(1x,5e15.3)
  107 READ(1,2,ERR=30,END=131) A
 1310 CONTINUE
      KHAR='YES '
      IF(KHAR.EQ.'YES ') GO TO 279
      INQUIRE(FILE=FNAME,OPENED=OPND,EXIST=EXST,ERR=131)
      OPEN(UNIT=8,FILE=FNAME,ERR=9994,STATUS = 'UNKNOWN')
      WRITE(*,*)' THIS  IS A FORTRAN 77 FORMAT THAT WILL CONTINUE ON NEX
     &T DATA CARD IMAGE.
     $ THIS CARD MAY NOT BE A LEGAL FORTRAN CARD.
     * ANOTHER CC.'
    2 FORMAT(1X,'A')
  279 CONTINUE
   30 DO 35 I=1,7
         X=4
      DO 100 J=1,INOPT
c     next line begins with two TABS
		DO 100 K=1,5
      IF(X.EQ.3) THEN
      Y=5
      elseif(x.eq.27.0) then
           c=6.8**Y
	END IF
      DO 99 M=1,4
      IF(LABEL(J,K,M).EQ.A) GOTO 100
      IF(LABEL(J,K,M).EQ.7) GOTO 110
   99 continue
      LABEL(J,K,I)=SPC
  100 continue
C
  110 CONTINUE
      X=8+C
 35   CONTINUE
  131 CONTINUE
      IF(I.EQ.1) GOTO 50
      GOTO 60
   50        CONTINUE
   60 CONTINUE
 9994 CALL TRASH(TIME,RETIME,DUMMY)
        CLOSE(8,STATUS='DELETE')
C
      IF(QQ(ILAY).LT.0.0.OR.QQ(ILAY).GT.10000)CALL ERRMSG(19,*440,NRET2)
      CALL INDATF(ISEA,LAMDA,IPRINT,N,ISTORE,BR0,*1100,NRET1)
          CALL SPF(ABETA(IT),Q(L),PHASE,MSPF(L),G1(L),G2(L),
     1    CC(L),*800,NRET2)
          IF(ISEA.EQ.1.AND.JRT.EQ.1)CALL SPF(SABETA,Q(L),SPHASE,MSPF(L),
     1    G1(L),G2(L),CC(L),*800,NRET2)
       CALL SKYRAD(N,ALB,ALBPR,ISEA,JUD,JTOT,JRT,*800,NRET2) 
      IF(AA(ILAY).LT.0.0.OR.AA(ILAY).GT.1)CALL ERRMSG(20,*440,NRET2)
      IF(IEND.LE.0.OR.IBGN.LE.0)CALL ERRMSG(63,*1610,NRET3) 
 1100 continue
  800 continue
  440 continue
 1610 continue
      IF(EXST) WRITE(*,*)'EXISTS'
      IF(OPND) WRITE(*,*)'OPENED'
      WRITE(IOOUT,*)(FAFORM(L),L=1,3)
      WRITE(IOOUT,*)(OBSURN(4))
      CALL EXIT
      end
C
      real FUNCTION XRANG(X,XMIN,XMAX)
      real*4 x,xmin,xmax
      XRANG=1.0
      IF(X-XMAX)100,100,300
  100 IF(X-XMIN)300,200,200
  200 XRANG=0.0
  300 RETURN
      END 
C ********************************************************************
      subroutine punish(a,b,c)
      real a
      real b,c
      b=sqrt(a)
    1 c=a+b
      do 4 j=1,7
    4 c=c+j
      return
      end
c
      subroutine dummy
      integer k1mk23, k1mk34, k1mk56
c
c the following type declarations for ix, iy and iz will illustrate the
c error made by ezup when it attempts to assign data types for the same
c three variables which were imiplicit integers in common/trash/ in the
c first program unit
      real*4 ix,iy,iz
c
      common /a1/ k1mk23, k1mk34,k1mk56
      i=4
      return
      end
C
      BLOCK DATA
C
      include 'ezup.inc'
C
      DATA KEYWDS/'CHAR','COMM','COMP','DIME','DOUB','EQUI','INTE',
     1            'REAL','DATA','IMPL','EXTE','FUNC','END ','ENDF',
     2            'LOGI','BLOC','BACK','PARA','EXTE','INTR','REWI',
     3            'WRIT','FORM','SUBR','CALL','PROG','STOP','SAVE',
     4            'OPEN','RETU','INQU','ELSE','GO T','GOTO','CONT',
     5            'ENDI','INCL','ENTR','ENDD','    '/
C
      END
C ********************************************************************
      real*4 FUNCTION XRANG1(X,XMIN,XMAX)
      real*4 x,xmin,xmax
      XRANG=1.0
      IF(X-XMAX)100,100,300
  100 IF(X-XMIN)300,200,200
  200 XRANG=0.0
  300 RETURN
      END 
C ********************************************************************
      real*8 FUNCTION XRANG2(X,XMIN,XMAX)
      real*4 x,xmin,xmax
      XRANG=1.0
      IF(X-XMAX)100,100,300
  100 IF(X-XMIN)300,200,200
  200 XRANG=0.0
  300 RETURN
      END 
C ********************************************************************
      complex FUNCTION XRANG3(X,XMIN,XMAX)
      real*4 x,xmin,xmax
      XRANG=1.0
      IF(X-XMAX)100,100,300
  100 IF(X-XMIN)300,200,200
  200 XRANG=0.0
  300 RETURN
      END 
C ********************************************************************
      complex*8 FUNCTION XRANG4(X,XMIN,XMAX)
      real*4 x,xmin,xmax
      XRANG=1.0
      IF(X-XMAX)100,100,300
  100 IF(X-XMIN)300,200,200
  200 XRANG=0.0
  300 RETURN
      END 
C ********************************************************************
      complex*16 FUNCTION XRANG5(X,XMIN,XMAX)
      real*4 x,xmin,xmax
      XRANG=1.0
      IF(X-XMAX)100,100,300
  100 IF(X-XMIN)300,200,200
  200 XRANG=0.0
  300 RETURN
      END 
C ********************************************************************
      double complex FUNCTION XRANG6(X,XMIN,XMAX)
      real*4 x,xmin,xmax
      XRANG=1.0
      IF(X-XMAX)100,100,300
  100 IF(X-XMIN)300,200,200
  200 XRANG=0.0
  300 RETURN
      END 
C ********************************************************************
      double precision FUNCTION XRANG7(X,XMIN,XMAX)
      real*4 x,xmin,xmax
      XRANG=1.0
      IF(X-XMAX)100,100,300
  100 IF(X-XMIN)300,200,200
  200 XRANG=0.0
  300 RETURN
      END 
C ********************************************************************
      FUNCTION XRANG7(X,XMIN,XMAX)
      real*4 x,xmin,xmax
      XRANG=1.0
      IF(X-XMAX)100,100,300
  100 IF(X-XMIN)300,200,200
  200 XRANG=0.0
  300 RETURN
      END 
C ********************************************************************
      FUNCTION IRANG0(I,IMIN,IMAX)
      INTEGER I,IMIN,IMAX
      IRANG=1.0
      IF(I-IMAX)100,100,300
  100 IF(X-XMIN)300,200,200
  200 IRANG=0.0
  300 RETURN
      END 
