      SUBROUTINE TP259(MODE)
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
      GOTO (1,2,3,4,4),MODE
    1 N=4
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,4
      X(I)=0.D+0
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LXU(4)=.TRUE.
      XU(4)=1.D+0
      LEX=.FALSE.
      NEX=2
      FEX=-0.85446210D+01
      XEX(1)=0.14358451D+01
      XEX(2)=0.20631635D+01
      XEX(3)=0.69002268D-01 
      XEX(4)=-0.99963939D-01
c      DO 7 I=1,4
c    7 XEX(N+I)=1.D+0
      RETURN
    2 FX=1.D+2*(X(2)-X(1)**2)**2+(1.D+0-X(1))**2 +
     1   9.D+1*(X(4)-X(3)**2)**2+(1.D+0-X(3))**3 +
     2   10.1D+0*(X(2)-1.D+0)**2+(X(4)-1.D+0)**2 +
     3   19.8D+0*(X(2)-1.D+0)*(X(4)-1.D+0)  
      RETURN
    3 GF(1)=4.D+2*(X(1)**3-X(1)*X(2))+2.D+0*X(1)-2.D+0
      GF(2)=-2.D+2*X(1)**2+220.2D+0*X(2)+19.8D+0*X(4)-4.D+1
      GF(3)=36.D+1*(X(3)**3-X(3)*X(4))-3.D+0*(1.D+0-X(3))**2
      GF(4)=-18.D+1*X(3)**2+182.D+0*X(4)+19.8D+0*X(2)-21.8D+0
    4 RETURN
      END
