      SUBROUTINE TP365(MODE)
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
      DOUBLEPRECISION P,Q
      INTEGER I
      GOTO (1,2,3,4,5), MODE
    1 N=7
      NILI=0
      NINL=5 
      NELI=0
      NENL=0
      X(1)=3.D+0
      X(2)=0.D+0
      X(3)=2.D+0
      X(4)=-1.5D+0
      X(5)=1.5D+0
      X(6)=5.D+0
      X(7)=0.D+0
      DO 11 I=1,3
      LXL(I*2)=.FALSE.
   11 LXL(I*2-1)=.TRUE.
      LXL(7)=.TRUE.
      DO 12 I=1,7
   12 LXU(I)=.FALSE.
      XL(1)=0.D+0
      XL(3)=0.D+0
      XL(5)=1.D+0
      XL(7)=1.D+0
      LEX=.FALSE.
      NEX=1
      FEX=0.23313708D+2
      XEX(1)=0.48284266D+1
      XEX(2)=0.47529555D-5
      XEX(3)=0.48284276D+1
      XEX(4)=0.10000024D+1
      XEX(5)=0.24142144D+1
      XEX(6)=0.24142151D+1
      XEX(7)=0.10000000D+1
      RETURN
    2 FX=X(1)*X(3)
    3 RETURN
    4 P=DSQRT(X(2)**2+X(3)**2)
      Q=DSQRT(X(3)**2+(X(2)-X(1))**2)
      IF (INDEX1(1)) G(1)=(X(4)-X(6))**2+(X(5)-X(7))**2-4.D+0
      IF (INDEX1(2)) G(2)=(X(3)*X(4)-X(2)*X(5))/P-1.D+0
      IF (INDEX1(3)) G(3)=(X(3)*X(6)-X(2)*X(7))/P-1.D+0
      IF (INDEX1(4)) G(4)=(X(1)*X(3)+(X(2)-X(1))*X(5)-X(3)*X(4))
     F    /Q-1.D+0
      IF (INDEX1(5)) G(5)=(X(1)*X(3)+(X(2)-X(1))*X(7)-X(3)*X(6))
     F    /Q-1.D+0
    5 RETURN 
      END
