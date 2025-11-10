      SUBROUTINE TP245(MODE)
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
      DOUBLEPRECISION F,DF,DI
      INTEGER I
      GOTO (1,2,2,4,4),MODE
    1 N=3
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      NEX=1
      X(1)=0.D+0
      X(2)=1.D+1
      X(3)=2.D+1
      DO 6 I=1,3
      XL(I)=0.0
      LXL(I)=.TRUE.
      XU(I)=20.0
    6 LXU(I)=.TRUE.
      XU(1)=12.0D0
      XU(2)=12.0D0
      LEX=.TRUE.
      FEX=0.D+0
      XEX(1)=1.D+0
      XEX(2)=1.D+1
      XEX(3)=1.D+0
      RETURN
    2 FX=0.D+0
      DO 7 I=1,10
      DI=DBLE(I)
    7 F(I)=DEXP(-DI*X(1)/1.D+1)-DEXP(-DI*X(2)/1.D+1)-
     /     X(3)*(DEXP(-DI/1.D+1)-DEXP(-DI))
      IF (MODE.EQ.3) GOTO 3
      DO 10 I=1,10
   10 FX=FX+F(I)**2
      RETURN
    3 DO 8 I=1,3
    8 GF(I)=0.D+0
      DO 9 I=1,10
      DI=DBLE(I)
      DF(I,1)=-DI/1.D+1*DEXP(-DI*X(1)/1.D+1)
      DF(I,2)=DI/1.D+1*DEXP(-DI*X(2)/1.D+1)
      DF(I,3)=DEXP(-DI)-DEXP(-DI/1.D+1)
      GF(1)=GF(1)+F(I)*DF(I,1)*2.D+0
      GF(2)=GF(2)+F(I)*DF(I,2)*2.D+0
    9 GF(3)=GF(3)+F(I)*DF(I,3)*2.D+0
    4 RETURN
      END
