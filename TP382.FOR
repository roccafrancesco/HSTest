      SUBROUTINE TP382(MODE)
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
      DOUBLEPRECISION R(13),S(13),U(13),V(13),Z1(13),Z2(13),Z3(13),HELP
      DATA R/0.8D+0,1.1D+0,0.85D+0,3.45D+0,2.D+0,2.1D+0,3.D+0,0.8D+0,
     1 0.45D+0,0.72D+0,1.8D+0,3.D+0,0.6D+0/
      DATA S/11.6D+0,13.7D+0,9.5D+0,48.5D+0,31.9D+0,51.1D+0,65.5D+0,
     1 0.D+0,0.D+0,0.D+0,21.8D+0,46.9D+0,0.D+0/
      DATA Z1/0.4844D+0,0.3003D+0,0.1444D+0,0.0588D+0,4.9863D+0,
     1 0.0653D+0,21.0222D+0,0.D+0,0.D+0,0.D+0,0.2970D+0,9.2933D+0,
     2 0.D+0/
      DATA U/0.05D+0,0.07D+0,0.D+0,0.33D+0,0.D+0,1.27D+0,1.27D+0,
     1 23.35D+0,35.84D+0,0.81D+0,1.79D+0,7.34D+0,0.D+0/
      DATA Z2/0.0001D+0,0.D+0,0.D+0,0.D+0,0.D+0,0.0040D+0,0.1404D+0,
     1 1.3631D+0,0.5138D+0,0.0289D+0,0.0097D+0,0.3893D+0,0.D+0/
      DATA V/0.35D+0,0.37D+0,0.1D+0,0.62D+0,0.D+0,1.03D+0,1.69D+0,
     1 18.21D+0,0.01D+0,0.08D+0,0.31D+0,1.59D+0,22.45D+0/
      DATA Z3/0.001D+0,0.0009D+0,0.0001D+0,0.0005D+0,0.D+0,0.0021D+0,
     1 0.0825D+0,0.2073D+0,0.D+0,0.0004D+0,0.0005D+0,0.0107D+0,
     2 1.0206D+0/
      GOTO(1,2,3,4,5),MODE    
    1 N=13
      NILI=0
      NINL=3
      NELI=1
      NENL=0
      DO 6 I=1,13
      X(I)=0.1D+0
      LXU(I)=.FALSE.
      LXL(I)=.TRUE.
      XL(I)=0.0D+0
    6 GG(4,I)=1.0D+0
      LEX=.FALSE.
      NEX=1
      FEX=1.03831D+0
      XEX(1)=0.769578D+0
      XEX(2)=0.D+0
      XEX(3)=0.D+0
      XEX(4)=0.D+0
      XEX(5)=0.D+0
      XEX(6)=0.18880588D+0
      XEX(7)=0.D+0
      XEX(8)=0.D+0
      XEX(9)=0.20625416D+0
      XEX(10)=0.D+0
      XEX(11)=0.D+0
      XEX(12)=0.D+0
      XEX(13)=0.020990697D+0
      DO 8 I=1,13
    8 GF(I)=R(I)
      RETURN
    2 FX=0.D+0
      DO 7 I=1,13
    7 FX=FX+R(I)*X(I)
    3 RETURN
    4 IF(.NOT.INDEX1(1)) GOTO 9
      G(1)=0.D+0
      DO 10 I=1,13
   10 G(1)=G(1)+Z1(I)*X(I)**2
      G(1)=-DSQRT(G(1))*0.1645D+1-0.18D+2
      DO 11 I=1,13
   11 G(1)=G(1)+S(I)*X(I)
    9 IF(.NOT.INDEX1(2)) GOTO 12
      G(2)=0.D+0               	
      DO 13 I=1,13
   13 G(2)=G(2)+Z2(I)*X(I)**2
      G(2)=-DSQRT(G(2))*0.1645D+1-0.1D+1
      DO 14 I=1,13
   14 G(2)=G(2)+U(I)*X(I)
   12 IF(.NOT.INDEX1(3)) GOTO 15
      G(3)=0.0D+0
      DO 16 I=1,13
   16 G(3)=G(3)+Z3(I)*X(I)**2
      G(3)=-DSQRT(G(3))*1.645D+0-0.9D+0
      DO 17 I=1,13
   17 G(3)=G(3)+V(I)*X(I)
   15 IF(.NOT.INDEX1(4)) GOTO 18
      G(4)=-1.D+0
      DO 19 I=1,13
   19 G(4)=G(4)+X(I)
   18 RETURN
    5 IF(.NOT.INDEX2(1)) GOTO 27
      HELP=0.D+0	
      DO 21 I=1,13
   21 HELP=HELP+Z1(I)*X(I)**2
      HELP=-1.645D+0/2.D+0/DSQRT(HELP)
      DO 22 I=1,13
   22 GG(1,I)=S(I)+HELP*2.D+0*Z1(I)*X(I)
   27 IF(.NOT.INDEX2(2)) GOTO 28
      HELP=0.D+0
      DO 23 I=1,13
   23 HELP=HELP+Z2(I)*X(I)**2
      HELP=-1.645D+0/2.D+0/DSQRT(HELP)
      DO 24 I=1,13
   24 GG(2,I)=U(I)+HELP*2.D+0*Z2(I)*X(I)
   28 IF(.NOT.INDEX2(3)) GOTO 20
      HELP=0.D+0
      DO 25 I=1,13
   25 HELP=HELP+Z3(I)*X(I)**2
      HELP=-1.645D+0/2.D+0/DSQRT(HELP)
      DO 26 I=1,13
   26 GG(3,I)=V(I)+HELP*2.D+0*Z3(I)*X(I)
   20 RETURN
      END   
