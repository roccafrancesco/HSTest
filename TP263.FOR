      SUBROUTINE TP263(MODE) 
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
 1    N=4 
      NILI=0 
      NINL=2 
      NELI=0 
      NENL=2 
      DO 6 I=1,4 
      X(I)=0.1D+2 
      LXL(I)=.FALSE.
 6    LXU(I)=.FALSE.
      GG(1,2)=0.1D+1 
      GG(1,3)=0.D+0 
      GG(2,2)=-0.1D+1 
      GG(2,3)=0.D+0 
      GG(3,2)=0.1D+1 
      GG(4,2)=-0.1D+1 
      GG(4,3)=0.D+0 
      GF(1)=-0.1D+1 
      DO 7 I=1,3  
      GG(I,4)=0.D+0 
 7    GF(I+1)=0.D+0 
      LEX=.TRUE. 
      NEX=1 
      FEX=-0.1D+1 
      DO 8 I=1,2 
      XEX(I)=0.1D+1 
 8    XEX(I+2)=0.D+0 
      RETURN 
 2    FX=-X(1) 
 3    RETURN 
 4    IF (INDEX1(1)) G(1)=X(2)-X(1)**3
      IF (INDEX1(2)) G(2)=X(1)**2-X(2) 
      IF (INDEX1(3)) G(3)=X(2)-X(1)**3-X(3)**2
      IF (INDEX1(4)) G(4)=X(1)**2-X(2)-X(4)**2
      RETURN 
 5    IF (INDEX2(1)) GG(1,1)=-0.3D+1*X(1)**2
      IF (INDEX2(2)) GG(2,1)=0.2D+1*X(1) 
      IF (.NOT.INDEX2(3)) GOTO 9 
      GG(3,1)=-0.3D+1*X(1)**2
      GG(3,3)=-0.2D+1*X(3) 
 9    IF (.NOT.INDEX2(4)) GOTO 10 
      GG(4,1)=0.2D+1*X(1) 
      GG(4,4)=-0.2D+1*X(4) 
10    RETURN 
      END 
