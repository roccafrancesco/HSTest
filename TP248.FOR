      SUBROUTINE TP248(MODE)
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
    1 N=3
      NILI=1
      NINL=0
      NELI=0
      NENL=1
      X(1)=-0.1D+0
      X(2)=-1.D+0
      X(3)=0.1D+0
      DO 6 I=1,3
      LXL(I)=.FALSE.
    6 LXU(I)=.FALSE.
      GG(1,1)=1.D+0
      GG(1,2)=-2.D+0
      GG(1,3)=0.D+0
      LEX=.TRUE.
      NEX=1
      FEX=-0.8D+0
      XEX(1)=0.6D+0
      XEX(2)=0.8D+0
      XEX(3)=0.D+0
      GF(1)=0.D+0
      GF(2)=-1.D+0
      GF(3)=0.D+0
      RETURN
    2 FX=-X(2)
    3 RETURN
    4 IF (INDEX1(1)) G(1)=1.D+0-2.D+0*X(2)+X(1)
      IF (INDEX1(2)) G(2)=X(1)**2+X(2)**2+X(3)**2-1.D+0
      RETURN
    5 IF (.NOT.INDEX2(2)) GOTO 8
      DO 9 I=1,3
    9 GG(2,I)=2.D+0*X(I)
    8 RETURN
      END
