      SUBROUTINE TP315(MODE)
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
      GOTO(1,2,3,4,5),MODE
    1 N=2
      NILI=1
      NINL=2
      NELI=0
      NENL=0
      X(1)=-.1D+0
      X(2)=-.9D+0
      LXU(1)=.FALSE.
      LXU(2)=.FALSE.
      LXL(1)=.FALSE.
      LXL(2)=.FALSE.
      GG(1,1)=.1D+1
      GG(1,2)=-.2D+1
      GF(1)=.0D+0
      GF(2)=-.1D+1
      LEX=.TRUE.
      NEX=1
      FEX=-.8D+0
      XEX(1)=.6D+0
      XEX(2)=.8D+0
      RETURN
    2 FX=-X(2)
    3 RETURN
    4 IF (INDEX1(1)) G(1)=.1D+1-.2D+1*X(2)+X(1)
      IF (INDEX1(2)) G(2)=X(1)**2+X(2)**2
      IF (INDEX1(3)) G(3)=.1D+1-X(1)**2-X(2)**2
      RETURN
    5 IF (.NOT. INDEX2(2)) GOTO 6
      GG(2,1)=.2D+1*X(1)
      GG(2,2)=.2D+1*X(2)
    6 IF (.NOT. INDEX2(3)) GOTO 7
      GG(3,1)=-.2D+1*X(1)
      GG(3,2)=-.2D+1*X(2)
    7 RETURN
      END
