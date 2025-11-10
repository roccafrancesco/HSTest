      SUBROUTINE TP314(MODE)
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
      DOUBLEPRECISION G1,H1,A,B
      INTEGER I
      GOTO (1,2,3,4,4),MODE
    1 N=2
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,2
      X(I)=.2D+1
      LXU(I)=.FALSE.
      XL(I)=1.0D0      
    6 LXL(I)=.TRUE.
      LEX=.FALSE.
      NEX=2
C      FEX=-0.79712534D+00
C      XEX(1)=0.10326462D+01
C      XEX(2)=0.86695956D+00 
      FEX=.16904D+0
      XEX(1)=.1789039D+1
      XEX(2)=.13740024D+1
      RETURN
    2 A=X(1)-.2D+1
      B=X(2)-.1D+1
      G1=(X(1)**2)/(-.4D+1)-X(2)**2+.1D+1
      H1=X(1)-.2D+1*X(2)+.1D+1
      FX=A**2+B**2+.4D-1/G1+H1**2/.2D+0
      RETURN
    3 G1=(X(1)**2)/(-.4D+1)-X(2)**2+.1D+1 
      H1=X(1)-.2D+1*X(2)+.1D+1
      GF(1)=.2D+1*(X(1)-.2D+1+X(1)*.1D-1/G1**2+.5D+1*H1)
      GF(2)=.2D+1*(X(2)-.1D+1+X(2)*.4D-1/G1**2-.1D+2*H1)
    4 RETURN
      END
