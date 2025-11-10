      SUBROUTINE TP333(MODE)
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
      DOUBLEPRECISION F,DF,A(8),Y(8)
      DATA A/.4D+1,.575D+1,.75D+1,.24D+2,.32D+2,.48D+2,.72D+2,.96D+2/
      DATA Y/.721D+2,.656D+2,.559D+2,.171D+2,.98D+1,.45D+1,.13D+1,.6D+0/
      INTEGER I,LSUM
      GOTO(1,2,2,4,4),MODE
    1 N=3
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      X(1)=.3D+2
      X(2)=.04D+0
      X(3)=.3D+1
      DO 6 I=1,3
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LXL(2)=.TRUE.
      XL(2)=0.0
      LXU(2)=.TRUE.
      XU(2)=0.07D0
      LEX=.FALSE.
      NEX=1
      FEX=0.0432
      XEX(1)=.89902D+2
      XEX(2)=.06699D+0
      XEX(3)=.47809D+0
      LSUM=8
      RETURN
    2 DO 7 I=1,8
    7 F(I)=(Y(I)-X(1)*DEXP(-X(2)*A(I))-X(3))/Y(I)
      IF (MODE.EQ.3) GOTO 3
      FX=.0D+0
      DO 8 I=1,8
    8 FX=FX+F(I)**2
      RETURN
    3 GF(1)=.0D+0
      GF(2)=.0D+0
      GF(3)=.0D+0
      DO 9 I=1,8
      DF(I,1)=(-DEXP(-X(2)*A(I)))/Y(I)
      DF(I,2)=(X(1)*A(I)*DEXP(-X(2)*A(I)))/Y(I)
      DF(I,3)=-1.D+0/Y(I)
      GF(1)=GF(1)+DF(I,1)*F(I)*.2D+1
      GF(2)=GF(2)+DF(I,2)*F(I)*.2D+1
    9 GF(3)=GF(3)+DF(I,3)*F(I)*.2D+1
    4 RETURN
      END
