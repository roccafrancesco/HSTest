      SUBROUTINE TP267(MODE)
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
      INTEGER I,J
      DOUBLEPRECISION H,F,DF
      GOTO (1,2,2,4,4),MODE
    1 N=5
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,5
      X(I)=2.D+0
      LXL(I)=.FALSE.
      XU(I)=15.0
    6 LXU(I)=.TRUE.
      LXL(1)=.TRUE.
      LXL(2)=.TRUE.
      LXL(5)=.TRUE.
      XL(1)=0.0
      XL(2)=0.0
      XL(5)=0.0
      LEX=.TRUE.
      NEX=2
      FEX=0.D+0
      XEX(1)=1.D+0
      XEX(2)=1.D+1
      XEX(3)=1.D+0
      XEX(4)=5.D+0
      XEX(5)=4.D+0
      XEX(6)=1.D+1
      XEX(7)=1.D+0
      XEX(8)=-5.D+0
      XEX(9)=-1.D+0
      XEX(10)=4.D+0
      RETURN
    2 DO 20 I=1,11
      H=.1D+0*DBLE(I)
   20 F(I)=X(3)*DEXP(-X(1)*H)-X(4)*DEXP(-X(2)*H)+3.D+0*DEXP(-X(5)*H)
     F      -(DEXP(-H)-5.D+0*DEXP(-1.D+1*H)+3.D+0*DEXP(-4.D+0*H))
      IF (MODE.EQ.3) GOTO 3 
      FX=0.D+0
      DO 7 I=1,11
    7 FX=FX+F(I)**2
      RETURN
    3 DO 8 J=1,11
      H=.1D+0*DBLE(J)
      DF(J,3)=DEXP(-X(1)*H)
      DF(J,4)=-DEXP(-X(2)*H)
      DF(J,1)=-H*X(3)*DF(J,3)
      DF(J,2)=-H*X(4)*DF(J,4)
    8 DF(J,5)=-H*3.D+0*DEXP(-X(5)*H)
      DO 13 I=1,5
      GF(I)=0.D+0
      DO 13 J=1,11
   13 GF(I)=GF(I)+2.D+0*F(J)*DF(J,I)
    4 RETURN
      END
