      SUBROUTINE TP393(MODE)
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
      DOUBLEPRECISION PHI,C,E
      GOTO (1,2,3,4,5),MODE
    1 N=48
      NILI=0
      NINL=1
      NELI=2
      NENL=0
      DO 6 I=1,24
    6 X(I)=1.D+0
      DO 7 I=25,30
    7 X(I)=1.3D+0
      DO 8 I=31,48
    8 X(I)=1.D+0
      DO 9 I=1,48
      LXL(I)=.TRUE.
    9 XL(I)=.002D+0
      DO 10 I=1,24
      LXU(I)=.TRUE.
   10 XU(I)=2.D+0
      DO 11 I=25,48
   11 LXU(I)=.FALSE.
      LEX=.FALSE.
      NEX=1
      FEX=0.86337998D+0
      XEX(1)=2.D+0
      XEX(2)=.002D+0
      XEX(3)=2.D+0
      XEX(4)=.0339797D+0
      XEX(5)=.01657455D+0
      XEX(6)=2.D+0
      XEX(7)=1.8945347D+0
      XEX(8)=.002D+0
      XEX(9)=2.D+0
      XEX(10)=.03424074D+0
      XEX(11)=.016670308D+0
      XEX(12)=2.D+0
      XEX(13)=2.D+0
      XEX(14)=.002D+0
      XEX(15)=2.D+0
      XEX(16)=.002D+0
      XEX(17)=.002D+0
      XEX(18)=1.988000D+0
      XEX(19)=2.D+0
      XEX(20)=.002D+0
      XEX(21)=2.D+0
      XEX(22)=.002D+0
      XEX(23)=.002D+0
      XEX(24)=2.D+0
      XEX(25)=1.0159886D+0
      XEX(26)=.002D+0
      XEX(27)=1.003163D+0
      XEX(28)=.002D+0
      XEX(29)=.002D+0
      XEX(30)=.999691944D+0
      XEX(31)=1.11272844D+0
      XEX(32)=.002D+0
      XEX(33)=1.1024463D+0
      XEX(34)=.002D+0
      XEX(35)=.002D+0
      XEX(36)=1.1030764D+0
      XEX(37)=.92326572D+0
      XEX(38)=.9343325D+0
      XEX(39)=.92947437D+0
      XEX(40)=.91383802D+0
      XEX(41)=.90517162D+0
      XEX(42)=.89452569D+0
      XEX(43)=1.174573D+0
      XEX(44)=.002D+0
      XEX(45)=1.12080408D+0
      XEX(46)=.002D+0
      XEX(47)=.002D+0
      XEX(48)=1.1163321536D+0
      RETURN
    2 E=0.D+0
      DO 100 I=1,12
      C=1.D+0-X(I)
  100 E=E+10.D+0*C*C
      DO 120 I=25,36
      C=X(I)-1.D+0
  120 E=E+1000.D+0*(.1D+0+2.D+0*C*(C+DSQRT(.1D+0+C*C)))/4.D+0
      DO 140 I=37,42
      C=X(I)-1.D+0
  140 E=E+2000.D+0*(.1D+0+2.D+0*C*(C+DSQRT(.1D+0+C*C)))/4.D+0
      DO 160 I=43,48
  160 E=E+100.D+0*X(I)
      FX=E/1000.D+0
    3 RETURN
    4 IF (.NOT.INDEX1(1)) GOTO 12
      CALL TP393B(X,PHI)
      G(1)=PHI
   12 IF (.NOT.INDEX1(2)) GOTO 14
      G(2)=12.D+0
      DO 13 I=1,12
   13 G(2)=G(2)-X(I)
   14 IF (.NOT.INDEX1(3)) GOTO 5
      G(3)=12.D+0
      DO 15 I=1,12
   15 G(3)=G(3)-X(I+12)
    5 RETURN
      END
