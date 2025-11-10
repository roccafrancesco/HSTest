      SUBROUTINE TP242(MODE)
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
      DOUBLEPRECISION TI,F,DF
      INTEGER I
      GOTO (1,2,2,4,4),MODE
    1 N=3
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      X(1)=2.5D+0
      X(2)=1.D+1
      X(3)=X(2)
      DO 6 I=1,3
      LXL(I)=.TRUE.
      LXU(I)=.TRUE.
      XL(I)=0.D+0
    6 XU(I)=1.D+1
      LEX=.TRUE.
      NEX=1
      FEX=0.D+0
      XEX(1)=1.D+0
      XEX(2)=1.D+1
      XEX(3)=1.D+0
      RETURN
    2 FX=0.D+0
      DO 7 I=1,10
      TI=0.1D+0*DBLE(I)
    7 F(I)=DEXP(-X(1)*TI)-DEXP(-X(2)*TI)-X(3)*
     F      (DEXP(-TI)-DEXP(-1.D+1*TI))
      IF (MODE.EQ.3) GOTO 3
      DO 8 I=1,10
    8 FX=FX+F(I)**2
      RETURN
    3 GF(1)=0.D+0
      GF(2)=0.D+0
      GF(3)=0.D+0
      DO 9 I=1,10
      TI=0.1D+0*DBLE(I)
      F(I)=DEXP(-X(1)*TI)-DEXP(-X(2)*TI)-X(3)*
     F     (DEXP(-TI)-DEXP(-1.D+1*TI))
      DF(I,1)=-TI*DEXP(-X(1)*TI)
      DF(I,2)=TI*DEXP(-X(2)*TI)
      DF(I,3)=DEXP(-1.D+1*TI)-DEXP(-TI)
      GF(1)=GF(1)+F(I)*DF(I,1)*2.D+0
      GF(2)=GF(2)+F(I)*DF(I,2)*2.D+0
    9 GF(3)=GF(3)+F(I)*DF(I,3)*2.D+0
    4 RETURN
      END
