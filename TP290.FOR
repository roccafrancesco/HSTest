      SUBROUTINE TP290(MODE)
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
      DOUBLEPRECISION F,DF
      N=2
      GOTO 10
      ENTRY TP291(MODE)
      N=10
      GOTO 10
      ENTRY TP292(MODE)
      N=30
      GOTO 10
      ENTRY TP293(MODE)
      N=50
   10 GOTO(1,3,3,4,4),MODE
    1 NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,N
      X(I)=.1D+1
      XEX(I)=.0D+0
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      FEX=.0D+0
      LEX=.TRUE.
      NEX=1
      RETURN
    2 FX=F(1)**2
      RETURN
    3 F(1)=.0D+0
      DO 7 I=1,N
    7 F(1)=F(1)+DBLE(I)*X(I)**2
      IF (MODE.EQ.2) GOTO 2
      DO 8 I=1,N
    8 DF(1,I)=DBLE(I)*.2D+1*X(I)
      DO 9 J=1,N
    9 GF(J)=.2D+1*F(1)*DF(1,J)
    4 RETURN
      END
