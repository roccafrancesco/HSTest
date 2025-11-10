      SUBROUTINE TP210(MODE)
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
      DOUBLEPRECISION C
      DATA C/1000000.D+0/
      INTEGER I
      GOTO(1,2,3,4,4),MODE
    1 N=2
      NILI=0
      NINL=0
      NELI=0 
      NENL=0
      X(1)=-1.2D+0
      X(2)=1.D+0
      DO 6 I=1,2 
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.TRUE.
      NEX=1
      FEX=0.D+0
      XEX(1)=1.D+0
      XEX(2)=1.D+0
      RETURN 
    2 FX=(C*(X(2)-X(1)**2)**2+(1.D+0-X(1))**2)/C
      RETURN
    3 GF(1)=(-4.D+0*C*(X(2)-X(1)**2)*X(1)-2.D+0*(1.D+0-X(1)))/C
      GF(2)=(2.D+0*C*(X(2)-X(1)**2))/C
    4 RETURN
      END
