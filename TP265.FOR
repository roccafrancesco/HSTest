      SUBROUTINE TP265(MODE) 
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
      DOUBLEPRECISION HGG(2,4)
      DATA HGG /1.D+0,0.D+0,1.D+0,0.D+0,0.D+0,1.D+0,0.D+0,1.D+0/
      GOTO (1,2,3,4,5),MODE 
 1    N=4 
      NILI=0 
      NINL=0 
      NELI=2 
      NENL=0 
      DO 6 I=1,4 
      X(I)=0.D+0
      LXL(I)=.TRUE.
      XL(I)=0.D+0 
      LXU(I)=.FALSE.
      XU(I)=1.D+0
      DO 6 J=1,2
      GG(J,I)=HGG(J,I) 
 6    CONTINUE   
      LEX=.TRUE.
      NEX=2
      FEX=0.97474658D+0
      DO 7 I=1,3,2
      XEX(I)=1.D+0
 7    XEX(I+1)=0.D+0
      DO 8 I=5,8
 8    XEX(I)=XEX(9-I)
      RETURN 
 2    FX=2.D+0 
      DO 9 I=1,2 
 9    FX=FX-DEXP(-0.1D+2*X(I)*DEXP(-X(I+2))) 
      RETURN 
 3    DO 10 I=1,2 
      GF(I)=0.1D+2*DEXP(-0.1D+2*X(I)*DEXP(-X(I+2))-X(I+2)) 
10    GF(I+2)=-X(I)*GF(I) 
      RETURN 
 4    IF (INDEX1(1)) G(1)=X(1)+X(2)-0.1D+1 
      IF (INDEX1(2)) G(2)=X(3)+X(4)-0.1D+1 
 5    RETURN 
      END 
