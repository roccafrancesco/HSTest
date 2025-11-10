      SUBROUTINE TP303(MODE)
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
      DOUBLEPRECISION POM
      N=20
      GOTO 10
      ENTRY TP304(MODE)
      N=50
      GOTO 10
      ENTRY TP305(MODE)
      N=100
   10 GOTO(1,2,3,4,4),MODE
    1 NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,N
      X(I)=.1D+0
      XEX(I)=.0D+0
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.TRUE.
      NEX=1
      FEX=.0D+0
      RETURN
    2 POM=.0D+0
      DO 7 I=1,N
    7 POM=POM+.5D+0*DBLE(I)*X(I)
      FX=POM**2+POM**4
      DO 8 I=1,N
    8 FX=FX+X(I)**2
      RETURN
    3 DO 9 I=1,N
    9 GF(I)=.2D+1*X(I)+POM*DBLE(I)+.2D+1*DBLE(I)*POM**3
    4 RETURN
      END
