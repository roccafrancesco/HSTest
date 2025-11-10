      SUBROUTINE TP308(MODE)
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
      INTEGER I
      DOUBLEPRECISION F,DF
      GOTO (1,2,2,4,4),MODE
    1 N=2
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      X(1)=.3D+1
      X(2)=.1D+0
      LXU(1)=.FALSE.
      LXU(2)=.FALSE.
      LXL(1)=.FALSE.
      LXL(2)=.FALSE.
      LEX=.FALSE.
      NEX=1
      FEX=.77319906D+0
      XEX(1)=-.15543724D+0
      XEX(2)=.69456378D+0
      RETURN
    2 F(1)=X(1)**2+X(2)**2+X(1)*X(2)
      F(2)=DSIN(X(1))
      F(3)=DCOS(X(2))
      IF (MODE.EQ.3) GOTO 3
      FX=0.D+0
      DO 5 I=1,3
    5 FX=FX+F(I)**2
      RETURN
    3 DF(1,1)=2.D+0*X(1)+X(2)
      DF(1,2)=2.D+0*X(2)+X(1)
      DF(2,1)=2.D+0*DSIN(X(1))*DCOS(X(1))
      DF(2,2)=0.D+0
      DF(3,1)=0.D+0
      DF(3,2)=-2.D+0*DCOS(X(2))*DSIN(X(2))
      GF(1)=0.D+0
      GF(2)=0.D+0
      DO 6 I=1,3
      GF(1)=GF(1)+2.D+0*F(I)*DF(I,1)
    6 GF(2)=GF(2)+2.D+0*F(I)*DF(I,2)
    4 RETURN
      END
