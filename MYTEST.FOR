      Program CNVRT2
C     convert digitizer data to absolute data
c  with: interpolation at 5 deg increments in angle
c
c     assumes data collected using pgm DIGI01.FOR
c
      character infile*64,outfil*64,messag*78,basnam*64,tmpnam*64
      DIMENSION XRAW(100),YRAW(100),XBAR(100),YBAR(100)
      dimension xabs(100),yabs(100),elx(4),xout(181),yout(181)
      luin=1
      luout=2
      messag='Enter the name of the input data file (from DIGI01):'
      call gtnf88(luin,0,messag,infile,ierr)
      if (ierr.ne.0) stop
      messag='Enter a name for the output file (converted data):'
      call gtof88(luout,0,messag,outfil,ierr)
      if (ierr.lt.0) stop
      if (ierr.eq.1) luout=6
      call nparse(outfil,basnam,last)
c     xmin and ymin are absolute values of coords at origin of measurements
c     xmax is abs value of x variable at rh end of axis
c     ymax is abs value of y variable at top of y axis
c     xlen and ylen are physical lengths of the axes in same units
c     xorg and yorg are the digitized integer coords for origin of graph
c     a and y will the floated integer digitized coords of a point
c     xbar and ybar are the converted values
c
      read(luin,*)xmin,xmax
      read(luin,*)ymin,ymax
      read(luin,*)XPOBAR,YPOBAR
      READ(LUIN,*)XPE,YPE
      READ(LUIN,*)XPT,YPT
C     CALCULATE TILT OF GRAPH XBAR AXIS WRT XPRIME(DIGITIZER X-AXIS)
      THETA=ATAN( (YPE-YPOBAR)/(XPE-XPOBAR))
C     AND THE SINE AD COSINE FOR COORD TRANSFORMATIONS
      CT=COS(THETA)
      ST=SIN(THETA)
C     SOME CONVERSIONS FROM DIGITIZER TO GRAPH PHYSICAL AXES (XBAR,YBAR)
      XBARE= CT*(XPE-xpobar) + ST*(YPE-ypobar)
      YBARE=-ST*(XPE-xpobar) + CT*(YPE-ypobar)
      XBART= CT*(XPT-xpobar) + ST*(YPT-ypobar)
      YBART=-ST*(XPT-xpobar) + CT*(YPT-ypobar)
      XBARO=0
      YBARO=0
      A=(YBART-YBARO)/ALOG10(YMAX/YMIN)
      B=YBARO-A*ALOG10(YMIN)
      C=(XMAX-XMIN)/(XBARE-XBARO)
      D=XMIN-C*XBARO
c
c     create file for the auxillary working data
c
      tmpnam=basnam
      tmpnam(last+1:last+4)= '.aux'
      open(unit=3,file=tmpnam,status='unknown')
      write(3,*)'xmin,xmax:     ',xmin,xmax
      write(3,*)'ymin,ymax:     ',ymin,ymax
      write(3,*)'xpobar,ypobar: ',xpobar,ypobar
      write(3,*)'xpe,ype:       ',xpe,ype
      write(3,*)'xpt,ypt:       ',xpt,ypt
      write(3,*)'theta:         ',theta
      write(3,*)'xbare,ybare:   ',xbare,ybare
      write(3,*)'xbart,ybart:   ',xbart,ybart
      write(3,*)'a,b:           ',a,b
      write(3,*)'c,d:           ',c,d
      close(3)
c
C ???? WHERE ARE XLEN AND YLEN ????
C
C     READ RAW DIGITIZER COORDS AND CONVERT TO GRAPH XBAR,YBAR COORDS
C
      N=0
  100 CONTINUE
      READ(LUIN,*,ERR=9000)xdum,ydum
c     CHECK FOR TERMINATING NEGATIVE INPUT DATA VALUES
      IF(Xdum.LT.0.0.AND.Ydum.LT.0.0) GOTO 110
      N=N+1
      xraw(n)=xdum
      yraw(n)=ydum
      XDUM=XRAW(N)-XPOBAR
      YDUM=YRAW(N)-YPOBAR
      XBAR(N)= CT*XDUM+ST*YDUM
      YBAR(N)=-ST*XDUM+CT*YDUM
      GOTO 100
C
  110 NPTS=N
C
C     CONVERT FROM GRAPH COORDS TO ABSOLUTE VALUES
c
      DO 200 N=1,NPTS
      Xabs(n)=C*XBAR(N)+D
      Yabs(n)=10**( (YBAR(N)-B)/A)
  200 WRITE(LUOUT,101) Xabs(n),Yabs(n)
      close(luout)
c     now want to interpolate over to a uniform angle-scale-> xout()
c     First set up the desired xvalues
c
      nang=37
      delx=180.0/(nang-1)
      do 205 n=1,nang
  205 xout(n)=(n-1)*delx
c
c     create file for the interpolated data
c
      tmpnam=basnam
      tmpnam(last+1:last+4)= '.fit'
      open(unit=9,file=tmpnam,status='unknown')
c
      nq=npts
c
c     cycle through the XABS() array and find smallest value of adjusted
c     x <= each desired output value of x. Then apply fitting routine.
c
      do 400 jout=1,nang
      xwant=XOUT(JOUT)
c ABORT interpolation for points XWANT > largest actual data point!
      IF(XWANT.GT.XABS(NPTS)) GOTO 405
      DO 310 I=1,NQ
         IF (xwant.LE.xabs(I)) GO TO 320
  310 CONTINUE
      GO TO 340
  320 IF (I.EQ.1) GO TO 330
      IF (I.EQ.2) GO TO 330
      IF (I.EQ.NQ) GO TO 340
      IMN=I-1
      IMX=I+1
      GO TO 350
C 
C     SPECIAL CASE: USE FIRST three VALUES OF xabs(I)
C 
  330 IMN=1
      IMX=3
      GO TO 350
C 
C     SPECIAL CASE: USE LAST three VALUES OF xabs(I)
C 
  340 IMN=NQ-2
      IMX=NQ
C 
C     INITIALIZE L-SUB-X VALUES
C 
  350 DO 360 I=1,3
  360    ELX(I)=1.0D+00
C 
C     CALCULATE THE INTERPOLATION FACTORS L-SUB-X
C 
      DO 370 I=IMN,IMX
         IP=I+1-IMN
         DO 370 J=IMN,IMX
C 
C     OMIT SITUATION I = J IN DOING INTERPOLATION CALCS
C 
         IF (I.NE.J) ELX(IP)=ELX(IP)*(xwant-xabs(J))/(xabs(I)-xabs(J))
  370 CONTINUE
c      NV=NOUT
c      DO 380 J=1,NV
         yout(Jout)=0.0D+00
         DO 380 I=IMN,IMX
         IP=I+1-IMN
  380    yout(Jout)=yout(Jout)+ELX(IP)*yabs(I)
  390  continue
  400 write(9,101) xout(jout),yout(jout)
  405 CONTINUE
      CLOSE(9)
c
c
      GO TO 9200
  101 format(1x,1P,e10.3,5x,e10.3)
 9000 write(*,*)'Failed to detect end of valid data'
 9200 close(luin)
      close(luout)
      stop
      end
c
      subroutine nparse(inname,basnam,last)
      character inname*64,basnam*24,achr
      basnam=' '
c I want to separate off everything to left of dec. pt. as basnam
      i=0
    5 i=i+1
      achr=inname(i:i)
      if (achr.eq.'.') goto 20
      goto 5
   20 last=i-1
      do 30 i=1,last
   30 basnam(i:i)=inname(i:i)
      write (*,*)'Base name: ', basnam
      return
      end
