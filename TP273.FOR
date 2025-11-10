      SUBROUTINE TP273(MODE) 
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
      DOUBLEPRECISION HX,TP273A
      INTEGER I
      GOTO (1,2,3,4,4)MODE 
 1    N=6 
      NILI=0 
      NINL=0 
      NELI=0 
      NENL=0 
      DO 6 I=1,6 
      X(I)=0.D+0 
      XEX(I)=0.1D+1 
      LXL(I)=.FALSE. 
6     LXU(I)=.FALSE. 
      LEX=.TRUE. 
      NEX=1 
      FEX=0.D+0 
      RETURN 
 2    HX=TP273A(X) 
      FX=0.1D+2*HX*(0.1D+1+HX) 
      RETURN 
 3    HX=TP273A(X) 
      DO 7 I=1,6 
 7    GF(I)=0.2D+2*(0.16D+2-DBLE(I))*(X(I)-0.1D+1) 
     1       *(0.1D+1+0.2D+1*HX) 
 4    RETURN 
      END       
