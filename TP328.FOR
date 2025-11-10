      SUBROUTINE TP328(MODE)
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
      X(I)=.5D+0
      LXU(I)=.TRUE.
      XU(I)=.3D+1
      LXL(I)=.TRUE.
    6 XL(I)=.1D+0
      LEX=.FALSE.
      NEX=1
      FEX=.1744152D+1
      XEX(1)=.1743439D+1
      XEX(2)=.20297056D+1
      RETURN
    2 A=(.1D+1+X(2)**2)/X(1)**2
      B=((X(1)*X(2))**2+.1D+3)/(X(1)*X(2))**4
      FX=(.12D+2+X(1)**2+A+B)/.1D+2
      RETURN
    3 A=(.1D+1+X(2)**2)/X(1)**3
      B=.1D+1/(X(1)**3*X(2)**2)+.2D+3/(X(1)**5*X(2)**4)
      GF(1)=(X(1)-A-B)/.5D+1
      A=.1D+1/(X(1)**2*X(2)**3)+.2D+3/(X(1)**4*X(2)**5)
      GF(2)=(X(2)/X(1)**2-A)/.5D+1
    4 RETURN
      END
