      SUBROUTINE TP330(MODE)
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
      DOUBLEPRECISION F,DF
      INTEGER I
      GOTO (1,2,3,4,5),MODE
    1 N=2
      NILI=0
      NINL=1
      NELI=0
      NENL=0
      DO 6 I=1,2
      X(I)=0.25D+1
      LXL(I)=.TRUE.
      LXU(I)=.TRUE.
      XL(I)=0.1D-4
    6 XU(I)=0.5D+1
      LEX=.FALSE.
      NEX=1
      FEX=.16205833D+1
      XEX(1)=0.12866773D+1
      XEX(2)=0.53046181D+0
      RETURN
    2 CONTINUE
      IF (X(1).LT.XL(1)) X(1)=XL(1)
      IF (X(2).LT.XL(2)) X(2)=XL(2)
      FX=0.44D-1*X(1)**3/X(2)**2+1.D+0/X(1)+0.592D-1*X(1)/X(2)**3
      RETURN
    3 GF(1)=.132D+0*X(1)**2/X(2)**2-X(1)**(-2)+.592D-1/X(2)**3
      GF(2)=-.88D-1*X(1)**3/X(2)**3-.1776D+0*X(1)/X(2)**4
      RETURN
    4 CONTINUE
      IF (X(1).LT.XL(1)) X(1)=XL(1)
      IF (INDEX1(1)) G(1)=.1D+1-.862D+1*X(2)**3/X(1)
      RETURN
    5 CONTINUE
      IF (X(1).LT.XL(1)) X(1)=XL(1)
      IF (.NOT.INDEX2(1)) RETURN
      GG(1,1)=.862D+1*X(2)**3/X(1)**2
      GG(1,2)=-.2586D+2*X(2)**2/X(1)
      RETURN
      END
