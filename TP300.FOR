      SUBROUTINE TP300(MODE)
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
      N=20
      FEX=-.2D+2
      GOTO 10
      ENTRY TP301(MODE)
      N=50
      FEX=-.5D+2
      GOTO 10
      ENTRY TP302(MODE)
      N=100
      FEX=-.1D+3
   10 GOTO(1,2,3,4,4),MODE
    1 NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,N
      X(I)=.0D+0
      XEX(I)=DBLE(N-I)+.1D+1
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.TRUE.
      NEX=1
      RETURN
    2 FX=X(1)**2-.2D+1*X(1)
      DO 7 I=2,N
    7 FX=FX+.2D+1*X(I)**2-.2D+1*X(I-1)*X(I)
      RETURN
    3 GF(1)=.2D+1*X(1)-.2D+1*X(2)-.2D+1
      DO 8 I=2,(N-1)
    8 GF(I)=.4D+1*X(I)-.2D+1*(X(I-1)+X(I+1))
      GF(N)=.4D+1*X(N)-.2D+1*X(N-1)
    4 RETURN                
      END
