      SUBROUTINE TP289(MODE)
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
     /           /L20/ LEX,NEX,FEX,XEX(NMAX) 
      INTEGER I
      GOTO(1,2,2,4,4),MODE
    1 N=30
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,30
      X(I)=(-.1D+1)**I*(.1D+1+DBLE(I)/.3D+2)
      XEX(I)=.0D+0
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.TRUE.
      NEX=1
      FEX=.0D+0
      RETURN
    2 FX=.0D+0
      DO 7 I=1,30
    7 FX=FX+X(I)**2
      FX=.1D+1-DEXP(-FX/.6D+2)
      IF (MODE.EQ.2) RETURN
      DO 8 I=1,30
    8 GF(I)=(FX-.1D+1)*(-.2D+1*X(I)/.6D+2)
    4 RETURN
      END  
