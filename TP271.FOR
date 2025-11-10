      SUBROUTINE TP271(MODE) 
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
      DOUBLEPRECISION  F,DF
      INTEGER I,J,LSUM
      GOTO (1,2,2,4,4),MODE 
 1    N=6 
      LSUM=6 
      NILI=0 
      NINL=0 
      NELI=0 
      NENL=0 
      DO 6 I=1,6 
      LXL(I)=.FALSE. 
      LXU(I)=.FALSE. 
      X(I)=0.D+0 
 6    XEX(I)=0.1D+1 
      LEX=.TRUE. 
      NEX=1
      FEX=0.D+0 
      RETURN 
 2    DO 10 I=1,6
 10   F(I)=DSQRT(DBLE(10*(16-I)))*(X(I)-1.D+0)
      IF (MODE.EQ.3) GOTO 3
      FX=0.D+0 
      DO 7 I=1,6 
 7    FX=FX+F(I)**2
      RETURN 
 3    DO 8 I=1,6 
      GF(I)=DBLE(20*(16-I))*(X(I)-0.1D+1) 
      DO 9 J=1,6 
 9    DF(I,J)=0.D+0 
 8    DF(I,I)=DSQRT(DBLE(10*(16-I))) 
 4    RETURN 
      END
