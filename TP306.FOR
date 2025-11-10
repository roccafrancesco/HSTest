      SUBROUTINE TP306(MODE)
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
      DOUBLEPRECISION A,B
      GOTO(1,2,3,4,4),MODE
    1 N=2
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,2
      X(I)=1.D+0
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.TRUE.
      NEX=1
      FEX=-.11036D+1
      XEX(1)=.0D+0
      XEX(2)=.1D+1
      RETURN
    2 FX=-DEXP(-X(1)-X(2))*(.2D+1*X(1)**2+.3D+1*X(2)**2)
      RETURN
    3 A=DEXP(-X(1)-X(2))
      B=.2D+1*X(1)**2+.3D+1*X(2)**2
      GF(1)=A*(B-.4D+1*X(1))
      GF(2)=A*(B-.6D+1*X(2))
    4 RETURN
      END
