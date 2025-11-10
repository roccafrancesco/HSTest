      SUBROUTINE TP282(MODE)
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
      GOTO (1,2,2,4,4), MODE
    1 N=10
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,10
      X(I)=0.D+0
      LXL(I)=.FALSE.
      LXU(I)=.FALSE.
    6 XEX(I)=1.D+0
      X(1)=-1.2D+0
      LEX=.TRUE.
      NEX=1
      FEX=0.D+0
      RETURN
    2 F(10)=X(1)-1.D+0
      F(11)=X(10)-1.D+0
      DO 20 I=1,9
   20 F(I)=DSQRT((1.D+1-DBLE(I))*1.D+1)*(X(I)**2-X(I+1))
      IF (MODE.EQ.3) GOTO 3
      FX=F(10)**2+F(11)**2
      DO 7 I=1,9
    7 FX=FX+F(I)**2
      RETURN
    3 DO 8 I=1,11
      DO 8 J=1,10
    8 DF(I,J)=0.D+0
      DO 13 I=1,9
      H=DSQRT((1.D+1-DBLE(I))*1.D+1)
      DF(I,I)=2.D+0*H*X(I)
   13 DF(I,I+1)=-H
      DF(10,1)=1.D+0
      DF(11,10)=1.D+0
      DO 18 I=1,10
      GF(I)=0.D+0
      DO 18 J=1,11
   18 GF(I)=GF(I)+2.D+0*DF(J,I)*F(J)
    4 RETURN
      END
