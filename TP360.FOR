      SUBROUTINE TP360(MODE)
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
      DOUBLEPRECISION  C(10),H,HH(5)
      DATA (C(I),I=1,10)/-0.8720288849D+7,0.1505125233D+6,
     F     -0.1566950325D+3,0.4764703222D+6,0.7294828271D+6,
     F     -0.3266695104D+6,0.739068412D+4,-0.278986976D+2,
     F     0.16643076D+5,0.30988146D+5/
      INTEGER I
      GOTO (1,2,3,4,5),MODE
    1 N=5  
      NILI=0
      NINL=2
      NELI=0
      NENL=0
      X(1)=0.252D+1
      X(2)=0.2D+1
      X(3)=0.375D+2
      X(4)=0.925D+1
      X(5)=0.68D+1
      LXL(1)=.TRUE.
      LXU(1)=.FALSE.
      DO 6 I=2,5
      LXL(I)=.TRUE.
    6 LXU(I)=.TRUE.
      XL(1)=0.0D+0
      XL(2)=0.12D+1
      XL(3)=0.2D+2
      XL(4)=0.9D+1
      XL(5)=0.65D+1
      XU(2)=0.24D+1
      XU(3)=0.6D+2
      XU(4)=0.93D+1
      XU(5)=0.7D+1
      LEX=.FALSE.
      NEX=1
      FEX=-0.52803351D+7
c      FEX=-0.52803351D0
      XEX(1)=0.4537431D+1
      XEX(2)=0.24D+1
      XEX(3)=0.6D+2
      XEX(4)=0.93D+1
      XEX(5)=0.7D+1
      RETURN
    2 FX=(-C(1)-C(2)*X(2)-C(3)*X(3)-C(4)*X(4)-C(5)*X(5))*X(1)+
     F   0.24345D+5       
      RETURN
    3 GF(1)=-C(1)-C(2)*X(2)-C(3)*X(3)-C(4)*X(4)-C(5)*X(5)
      DO 7 I=2,5
      GF(I)=-C(I)*X(1)
    7 CONTINUE
      RETURN
    4 H=(C(6)+C(7)*X(2)+C(8)*X(3)+C(9)*X(4)+C(10)*X(5))*X(1)
      IF (INDEX1(1)) G(1)=H
      IF (INDEX1(2)) G(2)=0.2772D+6-H
      RETURN
    5 HH(1)=C(6)+C(7)*X(2)+C(8)*X(3)+C(9)*X(4)+C(10)*X(5)
      DO 8 I=2,5     
    8 HH(I)=C(I+5)*X(1)  
      IF (.NOT.INDEX2(1)) GOTO 11
      DO 9 I=1,5
    9 GG(1,I)=HH(I)
   11 IF (.NOT.INDEX2(2)) GOTO 12
      DO 10 I=1,5
   10 GG(2,I)=-HH(I)
   12 RETURN
      END
