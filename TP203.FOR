      SUBROUTINE TP203(MODE)
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
      DOUBLEPRECISION C(3),F,DF
      DATA C/1.5D+0,2.25D+0,2.625D+0/
      INTEGER I
      GOTO (1,2,3,4,4),MODE
    1 N=2
      NILI=0
      NINL=0
      NELI=0 
      NENL=0
      X(1)=2.D+0
      X(2)=0.2D+0
      DO 6 I=1,2 
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.TRUE.
      NEX=1
      FEX=0.D+0
      XEX(1)=3.D+0
      XEX(2)=0.5D+0
      RETURN 
    2 FX=0.D+0
      DO 7 I=1,3
      F(I)=C(I)-X(1)*(1.D+0-X(2)**I) 
    7 FX=FX+F(I)**2
      RETURN
    3 DO 8 I=1,3
    8 F(I)=C(I)-X(1)*(1.D+0-X(2)**I) 
      DF(1,1)=-1.D+0+X(2)
      DF(1,2)=X(1)
      DF(2,1)=-1.D+0+X(2)**2
      DF(2,2)=2.D+0*X(1)*X(2)
      DF(3,1)=-1.D+0+X(2)**3
      DF(3,2)=3.D+0*X(1)*X(2)**2
      DO 9 I=1,2
    9 GF(I)=2.D+0*F(1)*DF(1,I)+2.D+0*F(2)*DF(2,I)+2.D+0*F(3)*DF(3,I)
    4 RETURN
      END
