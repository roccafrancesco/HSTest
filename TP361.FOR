      SUBROUTINE TP361(MODE)
      IMPLICIT DOUBLEPRECISION (A-H,O-Z)  
      INTEGER    MODE,NMAX,MMAX,N,NILI,NINL,NELI,NENL,NEX   
      DOUBLEPRECISION X,G,GF,GG,FX,XL,XU,FEX,XEX 
      LOGICAL    INDEX1,INDEX2,LXL,LXU,LEX    
      PARAMETER (NMAX=101, MMAX=50)
      COMMON     /L1/  N,NILI,NINL,NELI,NENL 
     /           /L2/  X(NMAX)                              
     /           /L3/  G(MMAX)           
     /           /L4/  GF(NMAX)          
     /           /L5/  GG(MMAX,NMAX)        
     /           /L6/  FX
     /           /L9/  INDEX1(MMAX)     
     /           /L10/ INDEX2(MMAX)
     /           /L11/ LXL(NMAX)       
     /           /L12/ LXU(NMAX)    
     /           /L13/ XL(NMAX)     
     /           /L14/ XU(NMAX)    
     /           /L20/ LEX,NEX,FEX,XEX(NMAX) 
      INTEGER I,J
      DOUBLEPRECISION A(5),B(5),C(5),D(5),H(3),HH(3,5)    
      DATA (A(I),I=1,5)/-0.8720288849D+7,0.1505125253D+6,
     F                  -0.1566950325D+3,0.4764703222D+6,
     F                  0.7294828271D+6/
     F     (B(I),I=1,5)/-0.145421402D+6,0.29311506D+4,-0.40427932D+2,
     F                 0.5106192D+4,0.1571136D+5/
     F     (C(I),I=1,5)/-0.1550111084D+6,0.436053352D+4,0.129492344D+2
     F                  ,0.10236884D+5,0.13176786D+5/
     F     (D(I),I=1,5)/-0.3266695104D+6,0.739068412D+4,
     F                  -0.278986976D+2,0.16643076D+5,0.30988146D+5/
      GOTO (1,2,3,4,5),MODE
    1 N=5
      NILI=0
      NINL=6
      NELI=0
      NENL=0
      X(1)=0.252D+1
      X(2)=0.2D+1
      X(3)=0.375D+2
      X(4)=0.925D+1
      X(5)=0.68D+1
      LXL(5)=.FALSE.
      LXU(1)=.FALSE.
      DO 6 I=1,4
      LXL(I)=.TRUE.
    6 LXU(I+1)=.TRUE.
      XL(1)=0.0D+0
      XL(2)=0.12D+1
      XL(3)=0.2D+2
      XL(4)=0.9D+1
      XU(2)=0.24D+1
      XU(3)=0.6D+2
      XU(4)=0.93D+1
      XU(5)=0.7D+1
      LEX=.FALSE.
      NEX=1
      FEX=-0.77641212D+6
      XEX(1)=0.68128605D+0
      XEX(2)=0.24D+1
      XEX(3)=0.2D+2
      XEX(4)=0.93D+1
      XEX(5)=0.7D+1
      RETURN
    2 FX=A(1)
      DO 7 I=2,5
    7 FX=FX+A(I)*X(I)
      FX=X(1)*FX-0.24345D+5
      FX=-FX
      RETURN
    3 GF(1)=A(1)
      DO 8 I=2,5
      GF(1)=GF(1)+A(I)*X(I)
    8 GF(I)=A(I)*X(1)
      DO 20 I=1,5
   20 GF(I)=-GF(I)
      RETURN
    4 H(1)=B(1)
      H(2)=C(1)
      H(3)=D(1)
      DO 9 I=2,5
      H(1)=H(1)+B(I)*X(I)
      H(2)=H(2)+C(I)*X(I)
    9 H(3)=H(3)+D(I)*X(I)
      DO 10 I=1,3
   10 H(I)=X(1)*H(I)
      DO 11 I=1,3
   11 IF (INDEX1(I)) G(I)=H(I)
      IF (INDEX1(4)) G(4)=0.294D+5-H(1)
      IF (INDEX1(5)) G(5)=0.294D+5-H(2)
      IF (INDEX1(6)) G(6)=0.2772D+6-H(3)
      RETURN
    5 HH(1,1)=B(1)
      HH(2,1)=C(1)
      HH(3,1)=D(1)
      DO 12 I=2,5
      HH(1,1)=HH(1,1)+B(I)*X(I)
      HH(2,1)=HH(2,1)+C(I)*X(I)
      HH(3,1)=HH(3,1)+D(I)*X(I)
      HH(1,I)=B(I)*X(1)
      HH(2,I)=C(I)*X(1)
   12 HH(3,I)=D(I)*X(1)
      DO 13 J=1,3
      IF (.NOT.INDEX2(J)) GOTO 13
      DO 130 I=1,5
  130 GG(J,I)=HH(J,I)
   13 CONTINUE
      DO 14 J=4,6
      IF (.NOT.INDEX2(J)) GOTO 14
      DO 140 I=1,5
  140 GG(J,I)=-HH(J-3,I)
   14 CONTINUE
      RETURN
      END
