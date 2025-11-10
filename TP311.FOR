      SUBROUTINE TP311(MODE)
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
      INTEGER I
      GOTO (1,2,3,4,4),MODE
    1 N=2
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 5 I=1,2
      X(I)=.1D+1
      LXL(I)=.FALSE.
    5 LXU(I)=.FALSE.
      LEX=.TRUE.
      NEX=2
      XEX(1)=.3D+1
      XEX(2)=.2D+1
      XEX(3)=3.58443D+0
      XEX(4)=-1.84813D+0
      FEX=.0D+0
    4 RETURN
    2 FX=(X(1)**2+X(2)-.11D+2)**2+(X(1)+X(2)**2-.7D+1)**2
      RETURN
    3 GF(1)=.4D+1*X(1)*(X(1)**2+X(2)-.11D+2)
     F      +.2D+1*(X(1)+X(2)**2-.7D+1)
      GF(2)=.2D+1*(X(1)**2+X(2)-.11D+2)
     F      +.4D+1*X(2)*(X(1)+X(2)**2-.7D+1)
      RETURN
      END    
