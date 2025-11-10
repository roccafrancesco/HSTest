      SUBROUTINE TP331(MODE)
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
      DOUBLEPRECISION F,DF,A,B,C,X1
      GOTO(1,2,3,4,5),MODE
    1 N=2
      NILI=1
      NINL=0
      NELI=0
      NENL=0
      X(1)=0.5D0
      X(2)=0.1D0
      LXU(1)=.TRUE.
      LXU(2)=.TRUE.
      XU(1)=0.7D0
      XU(2)=0.2D0
      LXL(1)=.TRUE.
      XL(1)=0.3D0
      LXL(2)=.TRUE.
      XL(2)=0.1D0
      GG(1,1)=-0.1D+1
      GG(1,2)=-0.1D+1
      LEX=.FALSE.
      NEX=1
      FEX=0.4258D+1
      XEX(1)=0.6175D+0
      XEX(2)=0.1039D+0
      RETURN
    2 CONTINUE
      IF (X(1).LT.XL(1)) X(1)=XL(1)
      IF (X(2).LT.XL(2)) X(2)=XL(2)
      FX=(DLOG(DABS(2.0D0*DLOG(X(2))/DLOG(X(1)+X(2)))))/X(1)
      RETURN
    3 CONTINUE
      IF (X(1).LT.XL(1)) X(1)=XL(1)
      IF (X(2).LT.XL(2)) X(2)=XL(2)
      A=X(1)+X(2)
      B=DLOG(A)
      C=2.0D0*DLOG(X(2))
      GF(1)=(-0.1D+1/X(1))*(DLOG(DMAX1(C/B,1.0D-4))/X(1)+0.1D+1/(B*A))
      GF(2)=((0.2D+1*B)/X(2)-C/A)/(C*B*X(1))
      RETURN
    4 IF (INDEX1(1)) G(1)=1.0D0-X(1)-X(2)
    5 RETURN
      END
