      SUBROUTINE TP288(MODE)
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
      DOUBLEPRECISION F,DF
      INTEGER I,J
      GOTO(1,3,3,4,4),MODE
    1 N=20
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,5
      X(I)=.3D+1
      X(I+5)=-.1D+1
      X(I+10)=.0D+0
    6 X(I+15)=.1D+1
      DO 7 I=1,20
      XEX(I)=.0D+0
      LXL(I)=.FALSE.
    7 LXU(I)=.FALSE.
      FEX=.0D+0
      LEX=.TRUE.
      NEX=1
      RETURN
    2 FX=.0D+0
      DO 9 I=1,20
    9 FX=FX+F(I)**2
      RETURN
    3 DO 8 I=1,5
      F(I)=X(I)+.1D+2*X(I+5)
      F(I+5)=DSQRT(.5D+1)*(X(I+10)-X(I+15))
      F(I+10)=(X(I+5)-.2D+1*X(I+10))**2
    8 F(I+15)=DSQRT(.1D+2)*(X(I)-X(I+15))**2
      IF (MODE.EQ.2) GOTO 2
      DO 11 I=1,5
      DO 11 J=1,20
      DF(I,J)=.0D+0
      IF (J.EQ.I) DF(I,J)=.1D+1
      IF (J.EQ.(I+5)) DF(I,J)=.1D+2
      DF(I+5,J)=.0D+0
      IF (J.EQ.(I+10)) DF(I+5,J)=DSQRT(.5D+1)
      IF (J.EQ.(I+15)) DF(I+5,J)=-DSQRT(.5D+1)
      DF(I+10,J)=.0D+0
      IF (J.EQ.(I+5)) DF(I+10,J)=.2D+1*(X(I+5)-.2D+1*X(I+10))
      IF (J.EQ.(I+10)) DF(I+10,J)=-.4D+1*(X(I+5)-.2D+1*X(I+10))
      DF(I+15,J)=.0D+0
      IF (J.EQ.I) DF(I+15,J)=DSQRT(.1D+2)*.2D+1*(X(I)-X(I+15))
   11 IF (J.EQ.(I+15)) DF(I+15,J)=-DSQRT(.1D+2)*.2D+1*(X(I)-X(I+15))
      DO 10 J=1,20
      GF(J)=.0D+0
      DO 10 I=1,20
   10 GF(J)=GF(J)+.2D+1*F(I)*DF(I,J)
    4 RETURN
      END
