      SUBROUTINE TP262(MODE) 
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
      INTEGER I,J
      DOUBLEPRECISION HGG(4,4) 
      DATA HGG/-1.D+0,-0.2D+0,-2.D+0,1.D+0,-1.D+0,-0.5D+0,-1.D+0,1.D+0,
     F     	2*-1.D+0,-0.5D+0,1.D+0,-1.D+0,-2.D+0,-0.2D+0,-2.D+0/
      GOTO (1,2,3,4,3),MODE 
 1    N=4 
      NILI=3 
      NINL=0 
      NELI=1 
      NENL=0 
      DO 6 I=1,4 
      X(I)=0.1D+1 
      LXL(I)=.TRUE.
      LXU(I)=.FALSE.
      XL(I) =0.D+0 
      DO 6 J=1,4
      GG(I,J)=HGG(I,J)
 6    CONTINUE
      DO 7 I=1,3,2
      GF(I)=-0.5D+0 
 7    GF(I+1)=-0.1D+1 
      LEX=.TRUE.
      NEX=1 
      FEX=-0.1D+2 
      XEX(1)=0.D+0 
      XEX(2)=0.26D+2/0.3D+1 
      XEX(3)=0.D+0 
      XEX(4)=0.4D+1/0.3D+1 
      RETURN 
 2    FX=-0.5D+0*X(1)-X(2)-0.5D+0*X(3)-X(4) 
 3    RETURN 
 4    IF (INDEX1(1)) G(1)=0.1D+2-X(1)-X(2)-X(3)-X(4) 
      IF (INDEX1(2)) G(2)=0.1D+2-0.2D+0*X(1)-0.5D+0*X(2)
     1                     -X(3)-0.2D+1*X(4) 
      IF (INDEX1(3)) G(3)=0.1D+2-0.2D+1*X(1)-X(2) 
     1                     -0.5D+0*X(3)-0.2D+0*X(4) 
      IF (INDEX1(4)) G(4)=X(1)+X(2)+X(3)-0.2D+1*X(4)-0.6D+1
      RETURN 
      END 
