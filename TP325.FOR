      SUBROUTINE TP325(MODE)
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
    1 N=2
      NILI=1
      NINL=1
      NELI=0
      NENL=1
      X(1)=-.3D+1
      X(2)=.0D+0
      DO 6 I=1,2
      GG(1,I)=-.1D+1
      LXL(I)=.FALSE.
    6 LXU(I)=.FALSE.
      GG(2,1)=-.1D+1
      GF(2)=.1D+1
      LEX=.FALSE.
      NEX=1
      FEX=0.37913414D+1 
      XEX(1)=-0.23722813D+1
      XEX(2)=-0.18363772D+1
      RETURN
    2 FX=X(1)**2+X(2)
      RETURN
    3 GF(1)=0.2D+1*X(1)
      RETURN
    4 IF (INDEX1(1)) G(1)=-(X(1)+X(2))+0.1D+1
      IF (INDEX1(2)) G(2)=-(X(1)+X(2)**2)+0.1D+1
      IF (INDEX1(3)) G(3)=X(1)**2+X(2)**2-0.9D+1
      RETURN
    5 IF (INDEX2(2)) GG(2,2)=-.2D+1*X(2)
      IF (.NOT.INDEX2(3)) RETURN
      GG(3,1)=.2D+1*X(1)
      GG(3,2)=.2D+1*X(2)
      RETURN
      END
