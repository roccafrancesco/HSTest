      SUBROUTINE TP362(MODE)
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
      DOUBLEPRECISION TP362A
      INTEGER I
      GOTO (1,2,3,4,5),MODE
    1 N=5
      NILI=4
      NINL=0
      NELI=0
      NENL=0
      X(1)=15.0D0
      X(2)=9.05D0
      X(3)=6.14D0
      X(4)=4.55D0
      X(5)=3.61D0
      DO 14 I=1,5
      XL(I)=3.0D0
      XU(I)=20.0D0
      LXL(I)=.TRUE.
   14 LXU(I)=.TRUE.
      XL(1)=15.D+0
      XL(5)=2.0D0
      LEX=.FALSE.
      NEX=3
      FEX=0.418D-01
      XEX(1)=0.19960505D+02 
      XEX(2)=0.19960505D+02  
      XEX(3)=0.19960505D+02
      XEX(4)=0.19960505D+02
      XEX(5)=0.19960505D+02
C      FEX=0.26229998D0
c      XEX(6)=0.15050962D+2
c      XEX(7)=0.88751199D+1
c      XEX(8)=0.59088230D+1
c      XEX(9)=0.48604810D+1
c      XEX(10)=0.43992690D+1
C      FEX=0.92400000D-01
c      XEX(11)=0.18414858D+02
c      XEX(12)=0.15909146D+02 
c      XEX(13)=0.15909146D+02
c      XEX(14)=0.15909146D+02
c      XEX(15)=0.86690897D+01
      RETURN
    2 FX=TP362A(X)
    3 RETURN
    4 DO 41 I=1,4
   41 IF (INDEX1(I)) G(I)=X(I)-X(I+1)
    5 RETURN 
      END
