      SUBROUTINE TP226(MODE)
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
    1 N=2
      NILI=0
      NINL=2
      NELI=0
      NENL=0
      X(1)=.8D+0
      X(2)=.05D+0
      DO 6 I=1,2
      LXU(I)=.FALSE.
      LXL(I)=.TRUE.
      XL(I)=0.D+0
    6 XEX(I)=1.D+0/DSQRT(2.D+0)
      NEX=1
      LEX=.TRUE.
      FEX=-.5D+0  
      RETURN
    2 FX=-X(1)*X(2)
      RETURN
    3 GF(1)=-X(2)
      GF(2)=-X(1)
      RETURN
    4 IF (INDEX1(1)) G(1)=X(1)**2+X(2)**2
      IF (INDEX1(2)) G(2)=1.D+0-X(1)**2-X(2)**2
      RETURN
    5 IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,1)=2.D+0*X(1)
      GG(1,2)=2.D+0*X(2)
    7 IF (.NOT.INDEX2(2)) GOTO 8
      GG(2,1)=-2.D+0*X(1)
      GG(2,2)=-2.D+0*X(2)
    8 RETURN
      END
