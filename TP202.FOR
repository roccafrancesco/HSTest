      SUBROUTINE TP202(MODE)
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
      GOTO(1,2,3,4,4),MODE
    1 N=2
      NILI=0
      NINL=0
      NELI=0 
      NENL=0
      X(1)=15.D+0
      X(2)=-2.D+0
      DO 6 I=1,2 
      LXU(I)=.TRUE.
    6 LXL(I)=.TRUE.
      XU(1)=20.0
      XU(2)=5.0
      XL(1)=1.0
      XL(2)=-5.0
      LEX=.TRUE.
      NEX=2
      FEX=0.D+0
      XEX(1)=5.D+0
      XEX(2)=4.D+0
      XEX(3)=48.98425D+0
      XEX(4)=-0.89681D+0
      RETURN 
    2 F(1)=-13.D+0+X(1)-2.D+0*X(2)+5.D+0*X(2)**2-X(2)**3
      F(2)=-29.D+0+X(1)-14.D+0*X(2)+X(2)**2+X(2)**3
      FX=F(1)**2+F(2)**2
      RETURN
    3 F(1)=-13.D+0+X(1)-2.D+0*X(2)+5.D+0*X(2)**2-X(2)**3
      F(2)=-29.D+0+X(1)-14.D+0*X(2)+X(2)**2+X(2)**3     
      DF(1,1)=1.D+0
      DF(1,2)=-2.D+0+10.D+0*X(2)-3.D+0*X(2)**2
      DF(2,1)=1.D+0
      DF(2,2)=-14.D+0+2.D+0*X(2)+3.D+0*X(2)**2
      DO 7 I=1,2
    7 GF(I)=2.D+0*F(1)*DF(1,I)+2.D+0*F(2)*DF(2,I)
    4 RETURN
      END
