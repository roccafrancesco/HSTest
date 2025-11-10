      SUBROUTINE TP261(MODE) 
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
     /           /L15/ F(200)    
     /           /L16/ DF(200,200)    
     /           /L20/ LEX,NEX,FEX,XEX(NMAX) 
      DOUBLEPRECISION  F,DF,A,B,C
      INTEGER I,J
      GOTO (1,2,2,4,4),MODE 
 1    N=4 
      NILI=0 
      NINL=0 
      NELI=0 
      NENL=0 
      DO 6 I=1,4 
      XL(I)=0.0
      LXL(I)=.TRUE.
      XU(I)=1.0D+1
      LXU(I)=.TRUE.
      X(I)=0.D+0 
      DO 6 J=1,5 
 6    DF(J,I)=0.D+0 
      DF(5,4)=0.1D+1 
      LEX=.TRUE.
      NEX=1 
      FEX=0.D+0 
      XEX(1)=0.D+0 
      DO 7 I=2,4 
 7    XEX(I)=0.1D+1 
      RETURN 
 2    CONTINUE
      F(1)=(DEXP(X(1))-X(2))**2 
      F(2)=0.1D+2*(X(2)-X(3))**3 
      F(3)=DTAN(X(3)-X(4))**2
      F(4)=X(1)**4
      F(5)=X(4)-0.1D+1
      IF (MODE.EQ.3) GOTO 3
      FX=0.D+0 
      DO 8 I=1,5 
 8    FX=FX+F(I)**2 
      RETURN 
 3    A=DEXP(X(1))-X(2) 
      B=DTAN(X(3)-X(4)) 
      C=B/(DCOS(X(3)-X(4)))**2
      DF(1,1)=0.2D+1*DEXP(X(1))*A 
      DF(1,2)=-0.2D+1*A 
      DF(2,2)=0.3D+2*(X(2)-X(3))**2
      DF(2,3)=-DF(2,2) 
      DF(3,3)=0.2D+1*C 
      DF(3,4)=-DF(3,3) 
      DF(4,1)=0.4D+1*X(1)**3
      GF(1)=0.4D+1*DEXP(X(1))*A**3+0.8D+1*X(1)**7 
      GF(2)=-0.4D+1*A**3+0.6D+3*(X(2)-X(3))**5
      GF(3)=0.4D+1*B*B*C-0.6D+3*(X(2)-X(3))**5 
      GF(4)=-0.4D+1*B*B*C+0.2D+1*(X(4)-0.1D+1) 
 4    RETURN 
      END 
