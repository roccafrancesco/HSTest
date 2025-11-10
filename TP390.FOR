      SUBROUTINE TP390(MODE)
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
      DOUBLEPRECISION PSI(11),ZI1,ZI2,ZI3
      GOTO (1,2,3,4,3),MODE
    1 N=19
      NILI=1
      NINL=0
      NELI=0
      NENL=11
      X(1)=2.D-2
      X(2)=4.D+0
      X(3)=100.D+0
      X(4)=100.D+0
      X(5)=15.D+0
      X(6)=15.D+0
      X(7)=100.D+0
      X(8)=1000.D+0
      X(9)=1000.D+0
      X(10)=1000.D+0
      X(11)=9000.D+0
      X(12)=0.001D+0
      X(13)=0.001D+0
      X(14)=1.D+0
      X(15)=0.001D+0
      X(16)=0.001D+0
      X(17)=.1D+0
      X(18)=8000.D+0
      X(19)=0.001D+0
      DO 6 I=1,19
      LXU(I)=.TRUE.
      LXL(I)=.TRUE.
      XU(I)=1.D+5
    6 XL(I)=0.00001D+0
      DO 7 I=1,2
      XU(I)=5.D+1
    7 XU(I+15)=5.D+1
      DO 9 I=3,6
    9 XU(I)=1.D+2
      DO 10 I=12,15
   10 XU(I)=1.D+0
      LEX=.FALSE.
      NEX=1
      FEX=0.244724654D+2
      XEX(1)=0.004473667D+0
      XEX(2)=0.3441565D+1
      XEX(3)=0.9934824D+2
      XEX(4)=0.89130035D+2
      XEX(5)=0.15279316D+2
      XEX(6)=0.15279316D+2
      XEX(7)=0.94726127D+2
      XEX(8)=0.12304197D+5
      XEX(9)=0.12313263D+5
      XEX(10)=0.12313263D+5
      XEX(11)=0.95905631D+5
      XEX(12)=0.00001D+0
      XEX(13)=0.00001D+0
      XEX(14)=0.9999890D+0
      XEX(15)=0.00001D+0
      XEX(16)=0.00001D+0
      XEX(17)=0.1622235D+0
      XEX(18)=0.83051515D+4
      XEX(19)=0.0014797356D+0
      RETURN
    2 ZI1=25.D+0*(2268.D+0*X(16)*X(1))**0.827D0
      ZI2=1.75D+05*X(17)+3.65D+04*X(17)**.182D0
      ZI3=12.6D+0*X(18)+5.35D+0*10.D+0**3.378D0/X(18)**.126D0
      FX=1.4D+0*(ZI1+ZI2+ZI3+1.095D+04+1.15D+03*(X(1)*(X(13)-X(14))
     1 +X(2)*(1.D+0+X(12))-3.D+0*(1.D+0-X(19))))
    3 RETURN
    4 IF (INDEX1(1)) G(1)=1.D+0-X(13)-X(14)
      CALL TP390A(X,PSI)
      DO 8 I=2,12
      IF (.NOT.INDEX1(I)) GOTO 8
      G(I)=PSI(I-1)
    8 CONTINUE
      RETURN
      END
