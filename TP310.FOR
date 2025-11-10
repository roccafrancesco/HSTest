      SUBROUTINE TP310(MODE)
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
      DOUBLEPRECISION A,B,C
      GOTO (1,2,3,4,4),MODE
    1 N=2
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      X(1)=-.12D+1
      X(2)=.1D+1
      DO 7 I=1,2
      LXU(I)=.FALSE.
    7 LXL(I)=.FALSE.
      LEX=.TRUE.
      NEX=1
      FEX=0.0D0
      XEX(1)=0.0D0
      XEX(2)=0.0D0
      RETURN
    2 FX=(X(1)*X(2))**2*(.1D+1-X(1))**2
     /    *(.1D+1-X(1)-X(2)*(.1D+1-X(1))**5)**2
      RETURN
    3 A=X(1)*X(2)
      B=.1D+1-X(1)
      C=B-X(2)*(B**5)
      GF(1)=2.D+0*A*B*C*(X(2)-1.D+0-5.D+0*X(2)*(B**4))
      GF(2)=2.D+0*A*B*C*(X(1)-(B**5))
    4 RETURN
      END
