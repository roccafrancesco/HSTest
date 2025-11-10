      SUBROUTINE TP341(MODE)
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
      GOTO(1,2,3,4,5),MODE
 1    N=3
      NILI=0
      NINL=1
      NELI=0
      NENL=0
      DO 6  I=1,3
      X(I)=0.1D+1
      LXL(I)=.TRUE.
      LXU(I)=.FALSE.
 6    XL(I)=0.0D+0
      LEX=.FALSE.
      NEX=1
      XEX(1)=.40000000D+01
      XEX(2)=.28284271D+01
      XEX(3)=.20000000D+01
      FEX=-.22627417D+02
      RETURN
 2    FX=-X(1)*X(2)*X(3)
      RETURN
 3    GF(1)=-X(2)*X(3)
      GF(2)=-X(1)*X(3)
      GF(3)=-X(1)*X(2)
      RETURN
 4    IF (INDEX1(1))  G(1)=-.1D+1*X(1)**2-0.2D+1*X(2)**2
     F                      -0.4D+1*X(3)**2+0.48D+2
      RETURN
 5    IF (.NOT.INDEX2(1))  GOTO 7
      GG(1,1)=-0.2D+1*X(1)
      GG(1,2)=-0.4D+1*X(2)
      GG(1,3)=-0.8D+1*X(3)
 7    RETURN
      END     
