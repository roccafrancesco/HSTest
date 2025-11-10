      SUBROUTINE TP313(MODE)
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
      DOUBLEPRECISION XH
      INTEGER I
      GOTO (1,2,3,4,4),MODE
    1 N=2
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      X(1)=.0D+0
      X(2)=-.1D+1
      DO 5 I=1,2
      LXL(I)=.FALSE.
    5 LXU(I)=.FALSE.
      LEX=.FALSE.
      NEX=1
      FEX=.199786D+0
      XEX(1)=.3D+1
      XEX(2)=.2850214D+1
      RETURN
    2 FX=.1D-3*(X(1)-.3D+1)**2-(X(2)-X(1))+DEXP(.2D+2*(X(2)-X(1)))
      RETURN
    3 XH=.2D+2*DEXP(.2D+2*(X(2)-X(1)))
      GF(1)=.1D+1+.2D-3*(X(1)-.3D+1)-XH
      GF(2)=XH-.1D+1
    4 RETURN
      END  
