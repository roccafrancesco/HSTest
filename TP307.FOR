      SUBROUTINE TP307(MODE)
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
      DOUBLEPRECISION F,DF,WI,XI1,XI2
      INTEGER I
      GOTO (1,2,3,4,4),MODE
    1 N=2
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      X(1)=0.3D0
      X(2)=0.4D0
      DO 6 I=1,2
      LXL(I)=.TRUE.
      XL(I)=0.0
      LXU(I)=.TRUE.
C      XU(I)=0.26
      XU(I)=1.0D+10
    6 XEX(I)=.25783D+0
      NEX=0
      LEX=.FALSE.
      FEX=0.12436D+3
      RETURN
    2 FX=0.0D0
      GOTO 9
    3 GF(1)=0.0D0
      GF(2)=0.0D0
    9 DO 7 I=1,10
      WI=DBLE(I)
      XI1=WI*X(1)
      IF (XI1.GT.20) XI1=0.0D0
      XI2=WI*X(2)
      IF (XI2.GT.20) XI2=0.0D0
      F(I)=2.0D0 + 2.0D0*WI - DEXP(XI1) - DEXP(XI2)
      IF (MODE.EQ.2) GOTO 8
      DF(I,1)=-WI*DEXP(XI1)
      DF(I,2)=-WI*DEXP(XI2)
      GF(1)=GF(1) + 2.0D0*F(I)*DF(I,1)
      GF(2)=GF(2) + 2.0D0*F(I)*DF(I,2)
      GOTO 7
    8 FX=FX+F(I)**2
    7 CONTINUE
    4 RETURN
      END
