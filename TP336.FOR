      SUBROUTINE TP336 (MODE)
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
      NINL=0
      NELI=1
      NENL=1
      DO 6 I=1,3
      X(I)=.0D+0
      LXU(I)=.FALSE.
 6    LXL(I)=.FALSE.
      GG(1,1)=.5D+1
      GG(1,2)=.5D+1
      GG(1,3)=-.3D+1
      LEX=.FALSE.
      NEX=1
      XEX(1)=.53459441D+00
      XEX(2)=.53397092D+00
      XEX(3) =-.21905778D+00
      FEX=-.33789573D+00
      RETURN
 2    FX=.7D+1*X(1)-.6D+1*X(2)+.4D+1*X(3)
      RETURN
 3    GF(1)=.7D+1
      GF(2)=-.6D+1
      GF(3)=.4D+1
      RETURN
 4    IF (INDEX1(1)) G(1)=.5D+1*X(1)+.5D+1*X(2)-.3D+1*X(3)-.6D+1
      IF (INDEX1(2)) G(2)=X(1)**2+.2D+1*X(2)**2+.3D+1*X(3)**2-.1D+1
      RETURN
 5    IF (.NOT.INDEX2(2)) GOTO 8
      GG(2,1)=.2D+1*X(1)
      GG(2,2)=.4D+1*X(2)
      GG(2,3)=.6D+1*X(3)
 8    RETURN
      END
