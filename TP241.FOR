      SUBROUTINE TP241(MODE)
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
      INTEGER I
      GOTO (1,2,2,4,4),MODE
    1 N=3
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      X(1)=1.D+0
      X(2)=2.D+0
      X(3)=0.D+0
      DO 6 I=1,3
      LXL(I)=.FALSE.
    6 LXU(I)=.FALSE.
      LEX=.TRUE.
      NEX=1
      FEX=0.D+0
      XEX(1)=0.D+0
      XEX(2)=0.D+0
      XEX(3)=1.D+0
      RETURN
    2 F(1)=X(1)**2+X(2)**2+X(3)**2-1.D+0
      F(2)=X(1)**2+X(2)**2+(X(3)-2.D+0)**2-1.D+0
      F(3)=X(1)+X(2)+X(3)-1.D+0
      F(4)=X(1)+X(2)-X(3)+1.D+0
      F(5)=X(1)**3+3.D+0*X(2)**2+(5.D+0*X(3)-X(1)+1.D+0)**2-3.6D+1
      IF (MODE.EQ.3) GOTO 3
      FX=F(1)**2+F(2)**2+F(3)**2+F(4)**2+F(5)**2
      RETURN
    3 DO 7 I=1,3
      DF(1,I)=2.D+0*X(I)
    7 DF(3,I)=1.D+0
      DF(2,1)=DF(1,1)
      DF(2,2)=DF(1,2)
      DF(2,3)=2.D+0*(X(3)-2.D+0)
      DF(4,1)=1.D+0
      DF(4,2)=1.D+0
      DF(4,3)=-1.D+0
      DF(5,1)=3.D+0*X(1)**2-2.D+0*(5.D+0*X(3)-X(1)+1.D+0)
      DF(5,2)=6.D+0*X(2)
      DF(5,3)=1.D+1*(5.D+0*X(3)-X(1)+1.D+0)
      GF(1)=0.D+0
      GF(2)=0.D+0
      GF(3)=0.D+0
      DO 8 I=1,5
      GF(1)=GF(1)+F(I)*DF(I,1)*2.D+0
      GF(2)=GF(2)+F(I)*DF(I,2)*2.D+0
    8 GF(3)=GF(3)+F(I)*DF(I,3)*2.D+0
    4 RETURN
      END
