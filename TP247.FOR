      SUBROUTINE TP247(MODE)
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
      DOUBLEPRECISION THETA,DTHETA(3),XPI
      INTEGER I
      GOTO (1,2,3,4,4),MODE
    1 N=3
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      NEX=1
      DO 6 I=1,2
      LXL(I)=.FALSE.
    6 LXU(I)=.FALSE.
      LXL(3)=.TRUE.
      LXU(3)=.TRUE.
      LXL(1)=.TRUE.
      XL(1)=0.1D+0
      XL(3)=-2.5D+0
      XU(3)=7.5D+0
      LEX=.TRUE.
      X(1)=0.1
      X(2)=0.D+0
      X(3)=0.D+0
      FEX=0.D+0
      XEX(1)=1.D+0
      XEX(2)=0.D+0
      XEX(3)=0.D+0
      RETURN
    2 XPI=DASIN(1.D+0)*2.D+0
      THETA=1.D+0/(2.D+0*XPI)*DATAN(X(2)/X(1))
      IF (X(1).LT.0.D+0) THETA=THETA+0.5D+0
      FX=1.D+2*((X(3)-1.D+1*THETA)**2+
     /   (DSQRT(X(1)**2+X(2)**2)-1.D+0)**2)+X(3)**2
      RETURN
    3 XPI=DASIN(1.D+0)*2.D+0
      THETA=1.D+0/(2.D+0*XPI)*DATAN(X(2)/X(1))
      DTHETA(1)=-X(2)/((1.D+0+(X(2)/X(1))**2)*X(1)**2)
      DTHETA(2)=1.D+0/((1.D+0+(X(2)/X(1))**2)*X(1))
      DTHETA(3)=0.D+0
      IF (X(1).LT.0.D+0) THETA=THETA+0.5D+0
      GF(1)=1.D+2*(2.D+1*(X(3)-1.D+1*THETA)*DTHETA(1)+
     /      2.D+0*(DSQRT(X(1)**2+X(2)**2)-1.D+0)/(DSQRT(X(1)**2
     /      +X(2)**2))*X(1))
      GF(2)=1.D+2*(2.D+1*(X(3)-1.D+1*THETA)*DTHETA(2)+
     /      2.D+0*(DSQRT(X(1)**2+X(2)**2)-1.D+0)/(DSQRT(X(1)**2
     /      +X(2)**2))*X(2))
      GF(3)=1.D+2*(2.D+0*(X(3)-1.D+1*THETA))+2.D+0*X(3)
    4 RETURN
      END
