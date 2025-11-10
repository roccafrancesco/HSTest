      SUBROUTINE TP283(MODE)
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
      DOUBLEPRECISION H
      GOTO (1,2,3,4,4),MODE
    1 N=10
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,10
      X(I)=0.D+0
      LXL(I)=.FALSE.
      LXU(I)=.FALSE.
    6 XEX(I)=1.D+0
      LEX=.TRUE.
      NEX=1
      FEX=0.D+0
      RETURN
    2 FX=0.D+0
      DO 7 I=1,10
    7 FX=FX+DBLE(I**3)*(X(I)-1.D+0)**2
      FX=FX**3
      RETURN
    3 H=0.D+0
      DO 8 I=1,10
    8 H=H+DBLE(I**3)*(X(I)-1.D+0)**2
      DO 13 I=1,10
   13 GF(I)=6.0*DBLE(I**3)*(X(I)-1.D+0)*H**2
    4 RETURN
      END
