      SUBROUTINE TP346(MODE)
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
      NINL=2
      NELI=0
      NENL=0
      DO 6 I=1,3
      LXU(I)=.TRUE.
      LXL(I)=.TRUE.
 6    XL(I)=.0D+0
      X(1)=.223D+2
      X(2)=.5D+0
      X(3)=.125D+3
      XU(1)=.36D+2
      XU(2)=.5D+1
      XU(3)=.125D+3
      GG(1,3)=.0D+0
      GG(2,2)=.0D+0
      LEX=.FALSE.
      NEX=1
      XEX(1)=.16508383D+02
      XEX(2)=.24768216D+01
      XEX(3)=.12399452D+03
      FEX=-.56847825D+01
      RETURN
 2    FX=-.201D-1*X(1)**4*X(2)*X(3)**2*.1D-6
      RETURN
 3    GF(1)=-.804D-1*X(1)**3*X(2)*X(3)**2*.1D-6
      GF(2)=-.201D-1*X(1)**4*X(3)**2*.1D-6
      GF(3)=-.402D-1*X(1)**4*X(2)*X(3)*.1D-6
      RETURN
 4    IF (INDEX1(1)) G(1)=.675D+3-X(1)**2*X(2)
      IF (INDEX1(2)) G(2)=.419D+0-X(1)**2*X(3)**2*.1D-6
      RETURN
 5    IF (.NOT.INDEX2(1)) GOTO7
      GG(1,1)=-.2D+1*X(1)*X(2)
      GG(1,2)=-X(1)**2
 7    IF (.NOT.INDEX2(2)) GOTO8
      GG(2,1)=-.2D-6*X(1)*X(3)**2
      GG(2,3)=-.2D-6*X(1)**2*X(3)
 8    RETURN
      END
