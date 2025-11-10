      SUBROUTINE TP260(MODE)
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
      GOTO (1,2,3,4,4),MODE
    1 N=4
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      X(1)=-3.D+0
      X(2)=-1.D+0
      X(3)=-3.D+0
      X(4)=-1.D+0
      DO 6 I=1,4
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.TRUE.
      NEX=1
      FEX=0.D+0
      DO 7 I=1,4
    7 XEX(I)=1.D+0
      DO 8 I=1,7
      DO 8 J=1,4
    8 DF(I,J)=0.D+0
      DF(1,2)=10.D+0
      DF(2,1)=-1.D+0
      DF(3,4)=DSQRT(9.D+1)
      DF(4,3)=-1.D+0
      DF(5,2)=DSQRT(9.9D+0)
      DF(5,4)=DSQRT(9.9D+0)
      DF(6,2)=DSQRT(.2D+0)
      DF(7,4)=DSQRT(.2D+0)
      RETURN
    2 F(1)=10.D+0*(X(2)-X(1)**2)
      F(2)=1.D+0-X(1)
      F(3)=DSQRT(9.D+1)*(X(4)-X(3)**2)
      F(4)=1.D+0-X(3)
      F(5)=DSQRT(9.9D+0)*((X(2)-1.D+0)+(X(4)-1.D+0))
      F(6)=DSQRT(.2D+0)*(X(2)-1.D+0)
      F(7)=DSQRT(.2D+0)*(X(4)-1.D+0)
      IF (MODE.EQ.3) GOTO 3
      FX=0.D+0
      DO 9 I=1,7
    9 FX=FX+F(I)**2
      RETURN
    3 DF(1,1)=-20.D+0*X(1)
      DF(3,3)=-DSQRT(9.D+1)*2.0*X(3)
      DO 10 I=1,4
      GF(I)=0.D+0
      DO 10 J=1,7
   10 GF(I)=GF(I)+2.D+0*F(J)*DF(J,I)      
    4 RETURN
      END
