      SUBROUTINE TP318(MODE)
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
      GOTO (1,2,3,4,5),MODE
    1 N=2
      NILI=0
      NINL=0
      NELI=0
      NENL=1
      DO 6 I=1,2
      X(I)=.0D+0
      LXL(I)=.FALSE.
    6 LXU(I)=.FALSE.
      LEX=.FALSE.
      NEX=1
      FEX=.41275005D+3
      XEX(1)=.78091266D+1
      XEX(2)=-.37478414D+1
      RETURN
    2 FX=(X(1)-.2D+2)**2+(X(2)+.2D+2)**2
      RETURN
    3 GF(1)=.2D+1*X(1)-.4D+2
      GF(2)=.2D+1*X(2)+.4D+2
      RETURN
    4 IF (INDEX1(1)) G(1)=X(1)**2*.1D-1+X(2)**2/.36D+2-.1D+1
      RETURN
    5 IF (.NOT.INDEX2(1)) RETURN
      GG(1,1)=.2D-1*X(1)
      GG(1,2)=.2D+1*X(2)/.36D+2
      RETURN
      END
