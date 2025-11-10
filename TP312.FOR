      SUBROUTINE TP312(MODE)
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
      DOUBLEPRECISION A,B
      INTEGER I
      GOTO (1,2,3,4,4),MODE
    1 N=2
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,2
      X(I)=.1D+1
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.FALSE.
      NEX=1
      FEX=0.D+0
      XEX(1)=-.21026652D+2
      XEX(2)=-.36760009D+2
      RETURN
    2 A=X(1)**2+.12D+2*X(2)-.1D+1
      B=.49D+2*(X(1)**2+X(2)**2)+.84D+2*X(1)+.2324D+4*X(2)-.681D+3
      FX=A**2+B**2
      RETURN 
    3 A=X(1)**2+.12D+2*X(2)-.1D+1
      B=.49D+2*(X(1)**2+X(2)**2)+.84D+2*X(1)+.2324D+4*X(2)-.681D+3
      GF(1)=.2D+1*(.2D+1*X(1)*A+B*(.98D+2*X(1)+.84D+2))
      GF(2)=.2D+1*(.12D+2*A+B*(.98D+2*X(2)+.2324D+4))
    4 RETURN
      END
