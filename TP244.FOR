      SUBROUTINE TP244(MODE)
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
      DOUBLEPRECISION YI,ZI,F,DF
      INTEGER I
      GOTO (1,2,2,4,4),MODE
    1 N=3
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      NEX=1
      X(1)=1.D+0
      X(2)=2.D+0
      X(3)=1.D+0
      DO 6 I=1,3
      XL(I)=0.0
      XU(I)=1.0D+10
      LXL(I)=.TRUE.
    6 LXU(I)=.TRUE.
      LEX=.TRUE.
      FEX=0.D+0
      XEX(1)=1.D+0
      XEX(2)=1.D+1
      XEX(3)=5.D+0
      RETURN
    2 FX=0.D+0
      DO 7 I=1,10
      ZI=0.1D+0*DBLE(I)
      YI=DEXP(-ZI)-5.D+0*DEXP(-1.D+1*ZI)
    7 F(I)=DEXP(-X(1)*ZI)-X(3)*DEXP(-X(2)*ZI)-YI
      IF (MODE.EQ.3) GOTO 3
      DO 10 I=1,8
   10 FX=FX+F(I)**2
      RETURN
    3 DO 8 I=1,3
    8 GF(I)=0.D+0
      DO 9 I=1,10
      ZI=0.1D+0*DBLE(I)
      YI=DEXP(-ZI)-5.D+0*DEXP(-1.D+1*ZI)
      DF(I,1)=-ZI*DEXP(-X(1)*ZI)
      DF(I,2)=ZI*X(3)*DEXP(-X(2)*ZI)
      DF(I,3)=-DEXP(-X(2)*ZI)
      GF(1)=GF(1)+F(I)*DF(I,1)*2.D+0
      GF(2)=GF(2)+F(I)*DF(I,2)*2.D+0
    9 GF(3)=GF(3)+F(I)*DF(I,3)*2.D+0
    4 RETURN
      END
