      SUBROUTINE TP327(MODE)
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
      DOUBLEPRECISION Y(44),Z(44),F,DF
      INTEGER I
      DATA Y/2*.49D+0,.48D+0,.47D+0,.48D+0,.47D+0,2*.46D+0,.45D+0,
     F       .43D+0,.45D+0,2*.43D+0,.44D+0,2*.43D+0,.46D+0,.45D+0,
     F       2*.42D+0,.43D+0,2*.41D+0,.4D+0,.42D+0,2*.4D+0,.41D+0,
     F       .4D+0,2*.41D+0,3*.4D+0,.38D+0,.41D+0,2*.4D+0,.41D+0,
     F       .38D+0,2*.4D+0,2*.39D+0/
      DATA Z/2*.8D+1,4*.1D+2,4*.12D+2,3*.14D+2,3*.16D+2,2*.18D+2,
     F       3*.2D+2,3*.22D+2,3*.24D+2,3*.26D+2,2*.28D+2,3*.3D+2,
     F       2*.32D+2,.34D+2,2*.36D+2,2*.38D+2,.4D+2,.42D+2/
      GOTO(1,2,2,4,5),MODE
    1 N=2
      NILI=0
      NINL=1
      NELI=0
      NENL=0
      X(1)=.42D+0
      X(2)=.5D+1
      DO 12 I=1,2
      LXU(I)=.FALSE.
      LXL(I)=.TRUE.
   12 XL(I)=.4D+0
      LEX=.FALSE.
      NEX=2
      FEX=0.28459670D-01
      XEX(1)=0.41995259D+0
      XEX(2)=0.12848442D+01
C      FEX=.30646306D-1
c      XEX(3)=.42190424D+0
c      XEX(4)=.50000526D+1
      RETURN
    2 DO 6 I=1,44
    6 F(I)=Y(I)-X(1)-(.49D+0-X(1))*DEXP(-X(2)*(Z(I)-.8D+1))
      IF (MODE.EQ.3) GOTO 3
      FX=.0D+0
      DO 7 I=1,44
    7 FX=FX+F(I)**2
      RETURN
    3 GF(1)=.0D+0
      GF(2)=.0D+0
      DO 8 I=1,44
      DF(I,1)=-.1D+1+DEXP(-X(2)*(Z(I)-.8D+1))
      DF(I,2)=(.49D+0-X(1))*DEXP(-X(2)*(Z(I)-.8D+1))*(Z(I)-.8D+1)
      GF(1)=GF(1)+DF(I,1)*F(I)*.2D+1
    8 GF(2)=GF(2)+DF(I,2)*F(I)*.2D+1
      RETURN
    4 IF (INDEX1(1)) G(1)=-.09D+0-X(1)*X(2)+.49D+0*X(2)
      RETURN
    5 IF (.NOT.INDEX2(1)) RETURN
      GG(1,1)=-X(2)
      GG(1,2)=.49D+0-X(1)
      RETURN
      END
