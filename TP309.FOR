      SUBROUTINE TP309(MODE)
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
      X(I)=.0D+0
      LXL(I)=.FALSE.
    5 LXU(I)=.FALSE.
      LEX=.FALSE.
      NEX=1
      XEX(1)=.34826826D+1
      XEX(2)=.39D+1
      FEX=-.39871708D+1
    4 RETURN
    2 FX=.141D+1*X(1)**4-.1276D+2*X(1)**3+.3991D+2*X(1)**2
     F   -.5193D+2*X(1)+.2437D+2+(X(2)-.39D+1)**2
      RETURN
    3 GF(1)=.564D+1*X(1)**3-.3828D+2*X(1)**2+.7982D+2*X(1)-.5193D+2
      GF(2)=.2D+1*X(2)-.78D+1
      RETURN
      END
