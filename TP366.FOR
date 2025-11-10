      SUBROUTINE TP366(MODE)
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
      DOUBLEPRECISION C(38)
      DATA C/.59553571D-3,.88392857D+0,-.11756250D+0,1.1088D+0,
     F .1303533D+0,-.0066033D+0,.66173269D-3,.17239878D-1,
     F -.56595559D-2,-.19120592D-1,.5685075D+2,1.08702D+0,
     F .32175D+0,-.03762D+0,.006198D+0,.24623121D+4,-.25125634D+2,
     F .16118996D+3,5.D+3,-.48951D+6,.44333333D+2,.33D+0,.022556D+0,
     F -.007595D+0,.00061D+0,-.5D-3,.819672D+0,.819672D+0,.245D+5,
     F -.25D+3,.10204082D-1,.12244898D-4,.625D-4,.625D-4,-.7625D-4,
     F 1.22D+0,1.D+0,-1.D+0/
      GOTO(1,2,3,4,5),MODE
    1 N=7
      NILI=0
      NINL=14
      NELI=0
      NENL=0
      X(1)=1745.D+0
      X(2)=110.D+0
      X(3)=3048.D+0
      X(4)=89.D+0
      X(5)=92.8D+0
      X(6)=8.D+0
      X(7)=145.D+0
      DO 11 I=1,7
      LXL(I)=.TRUE.
   11 LXU(I)=.TRUE.
      XL(1)=1.D+0
      XL(2)=1.D+0
      XL(3)=1.D+0
      XL(4)=85.D+0
      XL(5)=90.D+0
      XL(6)=3.D+0
      XL(7)=145.D+0
      XU(1)=2.D+3
      XU(2)=1.2D+2
      XU(3)=5.D+3
      XU(4)=93.D+0
      XU(5)=95.D+0
      XU(6)=12.D+0
      XU(7)=162.D+0
      LEX=.FALSE.
      NEX=1
      FEX=0.70430560D+03 
      XEX(1)=0.90540351D+3
      XEX(2)=0.36394998D+2
      XEX(3)=0.23814783D+4
      XEX(4)=0.88987691D+2
      XEX(5)=0.95D+2
      XEX(6)=0.12D+2
      XEX(7)=0.15353535D+3
      RETURN
    2 FX=(1.715D+0*X(1)+.035D+0*X(1)*X(6)+4.0565D+0*X(3)
     1 +10.D+0*X(2)+3000.D+0-.063D+0*X(3)*X(5))
    3 RETURN
    4 IF (INDEX1(1)) G(1)=1.D+0-C(1)*X(6)**2-C(2)*X(3)/X(1)-C(3)*X(6)
      IF (INDEX1(2)) G(2)=1.D+0-C(4)*X(1)/X(3)-C(5)*X(1)/X(3)*X(6)
     F -C(6)*X(1)/X(3)*X(6)**2
      IF (INDEX1(3)) G(3)=1.D+0-C(7)*X(6)**2-C(8)*X(5)-C(9)*X(4)
     F -C(10)*X(6)
      IF (INDEX1(4)) G(4)=1.D+0-C(11)/X(5)-C(12)/X(5)*X(6)-C(13)
     F *X(4)/X(5)-C(14)/X(5)*X(6)**2
      IF (INDEX1(5)) G(5)=1.D+0-C(15)*X(7)-C(16)*X(2)/X(3)/X(4)
     F -C(17)*X(2)/X(3)
      IF (INDEX1(6)) G(6)=1.D+0-C(18)/X(7)-C(19)*X(2)/X(3)/X(7)
     F -C(20)*X(2)/X(3)/X(4)/X(7)
      IF (INDEX1(7)) G(7)=1.D+0-C(21)/X(5)-C(22)*X(7)/X(5)
      IF (INDEX1(8)) G(8)=1.D+0-C(23)*X(5)-C(24)*X(7)
      IF (INDEX1(9)) G(9)=1.D+0-C(25)*X(3)-C(26)*X(1)
      IF (INDEX1(10)) G(10)=1.D+0-C(27)*X(1)/X(3)-C(28)/X(3)
      IF (INDEX1(11)) G(11)=1.D+0-C(29)*X(2)/X(3)/X(4)-C(30)*X(2)/X(3)
      IF (INDEX1(12)) G(12)=1.D+0-C(31)*X(4)-C(32)/X(2)*X(3)*X(4)
      IF (INDEX1(13)) G(13)=1.D+0-C(33)*X(1)*X(6)-C(34)*X(1)-C(35)*X(3)
      IF (INDEX1(14)) G(14)=1.D+0-C(36)/X(1)*X(3)-C(37)/X(1)-C(38)*X(6)
    5 RETURN 
      END
