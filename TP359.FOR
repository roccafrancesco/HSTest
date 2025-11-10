      SUBROUTINE TP359(MODE)
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
      DOUBLEPRECISION A(5),B(5),C(5),D(5),H(6)            
      DATA (A(I),I=1,5)/-0.8720288849D+7,0.1505125253D+6,
     /                  -0.1566950325D+3,0.4764703222D+6,
     /                  0.7294828271D+6/
     /     (B(I),I=1,5)/-0.145421402D+6,0.29311506D+4,-0.40427932D+2,
     /                 0.5106192D+4,0.1571136D+5/
     /     (C(I),I=1,5)/-0.1550111084D+6,0.436053352D+4,0.129492344D+2
     /                  ,0.10236884D+5,0.13176786D+5/
     /     (D(I),I=1,5)/-0.3266695104D+6,0.739068412D+4,
     /                  -0.278986976D+2,0.16643076D+5,0.30988146D+5/
      INTEGER I,J
      GOTO (1,2,3,4,5),MODE
    1 N=5
      NILI=14
      NINL=0
      NELI=0
      NENL=0
      X(1)=0.252D+1
      X(2)=0.504D+1
      X(3)=0.945D+2
      X(4)=0.2331D+2
      X(5)=0.17136D+2
      DO 10 I=1,5
   10 GF(I)=-A(I)
      DO 6 J=1,8
      DO 6 I=1,5
    6 GG(J,I)=0.0D+0
      DO 7 I=1,4
      GG(2*I-1,I+1)=-0.1D+1
    7 GG(2*I,I+1)=0.1D+1
      GG(1,1)=0.24D+1
      GG(2,1)=-0.12D+1
      GG(3,1)=0.6D+2
      GG(4,1)=-0.2D+2
      GG(5,1)=0.93D+1
      GG(6,1)=-0.9D+1
      GG(7,1)=0.7D+1
      GG(8,1)=-0.65D+1
      DO 8 I=1,5
      GG(9,I)=B(I)
      GG(10,I)=C(I)
      GG(11,I)=D(I)
      GG(12,I)=-B(I)
      GG(13,I)=-C(I)
      GG(14,I)=-D(I)
      LXU(I)=.FALSE.
    8 LXL(I)=.FALSE.
      LEX=.FALSE.
      NEX=1
      FEX=-0.52804168D+7
      XEX(1)=0.45374D+1
      XEX(2)=0.10889D+2
      XEX(3)=0.27224D+3
      XEX(4)=0.42198D+2
      XEX(5)=0.31762D+2
      RETURN
    2 FX=-0.24345D+5
      DO 9 I=1,5
    9 FX=FX+A(I)*X(I)
      FX=-FX
    3 RETURN
    4 IF (INDEX1(1)) G(1)=0.24D+1*X(1)-X(2)
      IF (INDEX1(2)) G(2)=-0.12D+1*X(1)+X(2)
      IF (INDEX1(3)) G(3)=0.6D+2*X(1)-X(3)
      IF (INDEX1(4)) G(4)=-0.2D+2*X(1)+X(3)
      IF (INDEX1(5)) G(5)=0.93D+1*X(1)-X(4)
      IF (INDEX1(6)) G(6)=-0.9D+1*X(1)+X(4)
      IF (INDEX1(7)) G(7)=0.7D+1*X(1)-X(5)
      IF (INDEX1(8)) G(8)=-0.65D+1*X(1)+X(5)
      DO 11 I=1,3
   11 H(I)=0.0D+0 
      H(4)=0.294D+6 
      H(5)=0.294D+6
      H(6)=0.2772D+6
      DO 12 I=1,5
      H(1)=H(1)+B(I)*X(I)
      H(2)=H(2)+C(I)*X(I)
      H(3)=H(3)+D(I)*X(I)
      H(4)=H(4)-B(I)*X(I)
      H(5)=H(5)-C(I)*X(I)
   12 H(6)=H(6)-D(I)*X(I)
      DO 13 I=9,14
   13 IF (INDEX1(I)) G(I)=H(I-8)
    5 RETURN
      END
