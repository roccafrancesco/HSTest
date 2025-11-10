      SUBROUTINE TP337(MODE)
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
      GOTO (1,2,3,4,5),MODE
 1    N=3
      NILI=0
      NINL=1
      NELI=0
      NENL=0
      DO 6 I=1,3
 6    X(I)=.1D+1
      LXU(1)=.FALSE.
      LXL(1)=.FALSE.
      LXU(2)=.FALSE.
      LXL(2)=.TRUE.
      LXU(3)=.TRUE.
      LXL(3)=.FALSE.
      XL(2)=.1D+1
      XU(3)=.1D+1
      GG(1,3)=.0D+0
      LEX=.FALSE.
      NEX=1
      XEX(1)=.57735194D+00
      XEX(2)=.17320458D+01
      XEX(3) =-.20256839D-05
      FEX=.6D+1
      RETURN 
 2    FX=.9D+1*X(1)**2+X(2)**2+.9D+1*X(3)**2
      RETURN
 3    GF(1)=.18D+2*X(1)
      GF(2)=.2D+1*X(2)
      GF(3)=.18D+2*X(3)
      RETURN
 4    IF (INDEX1(1)) G(1)=X(1)*X(2)-.1D+1
      RETURN
 5    IF (.NOT.INDEX2(1)) GOTO7
      GG(1,1)=X(2)
      GG(1,2)=X(1)
 7    RETURN
      END
