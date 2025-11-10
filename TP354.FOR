      SUBROUTINE TP354(MODE)
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
    1 N=4  
      NILI=1
      NINL=0
      NELI=0
      NENL=0
      X(1)=0.3D+1
      X(2)=-0.1D+1
      X(3)=0.0D+0
      X(4)=0.1D+1
      DO 6 I=1,4
      GG(1,I)=0.1D+1
      LXU(I)=.TRUE.
      LXL(I)=.FALSE.
    6 XU(I)=0.2D+2
      LEX=.FALSE.
      NEX=1
      FEX=0.11378385D+0
      XEX(1)=0.50336521D+0
      XEX(2)=-0.45569070D-1
      XEX(3)=0.23580504D+0
      XEX(4)=0.30639882D+0
      RETURN
    2 FX=(X(1)+0.1D+2*X(2))**2+0.5D+1*(X(3)-X(4))**2+(X(2)-0.2D+1
     F   *X(3))**4+0.1D+2*(X(1)-X(4))**4
      RETURN
    3 GF(1)=0.2D+1*X(1)+0.2D+2*X(2)+0.4D+2*(X(1)-X(4))**3
      GF(2)=0.2D+2*X(1)+0.2D+3*X(2)+0.4D+1*(X(2)-0.2D+1*X(3))**3
      GF(3)=0.1D+2*X(3)-0.1D+2*X(4)-0.8D+1*(X(2)-0.2D+1*X(3))**3
      GF(4)=-0.1D+2*X(3)+0.1D+2*X(4)-0.4D+2*(X(1)-X(4))**3
      RETURN
    4 IF (INDEX1(1)) G(1)=X(1)+X(2)+X(3)+X(4)-0.1D+1
    5 RETURN
      END
