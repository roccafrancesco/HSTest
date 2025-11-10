      SUBROUTINE TP272(MODE)
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
      DOUBLEPRECISION H,F,DF
      INTEGER I,J
      GOTO (1,2,2,4,4),MODE
    1 N=6
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,6
      X(I)=1.D+0
      XL(I)=0.0
      LXL(I)=.TRUE.
    6 LXU(I)=.FALSE.
      X(2)=2.D+0
      LEX=.TRUE.
      NEX=1
      FEX=0.D+0
      XEX(1)=1.D+0
      XEX(2)=10.D+0
      XEX(3)=4.D+0
      XEX(4)=1.D+0
      XEX(5)=5.D+0
      XEX(6)=3.D+0
      RETURN
    2 DO 20 I=1,13
      H=.1D+0*DBLE(I)
   20 F(I)=X(4)*DEXP(-X(1)*H)-X(5)*DEXP(-X(2)*H)+X(6)*DEXP(-X(3)*H)
     F     -DEXP(-H)+5.D+0*DEXP(-1.D+1*H)-3.D+0*DEXP(-4.D+0*H)
      IF (MODE.EQ.3) GOTO 3
      FX=0.D+0
      DO 7 I=1,13
    7 FX=FX+F(I)**2
      RETURN
    3 DO 8 J=1,13
      H=.1D+0*DBLE(J)
      DF(J,4)=DEXP(-X(1)*H)
      DF(J,5)=-DEXP(-X(2)*H)
      DF(J,6)=DEXP(-X(3)*H)
      DO 8 I=1,3
    8 DF(J,I)=-H*X(I+3)*DF(J,I+3)
      DO 13 I=1,6
      GF(I)=0.D+0
      DO 13 J=1,13
   13 GF(I)=GF(I)+2.D+0*DF(J,I)*F(J)
    4 RETURN
      END
