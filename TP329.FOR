      SUBROUTINE TP329(MODE)
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
      GOTO (1,2,3,4,5),MODE
    1 N=2
      NILI=0
      NINL=3
      NELI=0
      NENL=0
      LXL(1)=.TRUE.
      LXL(2)=.TRUE.
      LXU(1)=.TRUE.
      LXU(2)=.TRUE.
      X(1)=.1435D+2
      X(2)=.86D+1
      XL(1)=.13D+2
      XL(2)=.0D+0
      XU(1)=.16D+2
      XU(2)=.15D+2
      LEX=.FALSE.
      NEX=1
      FEX=-.69618139D+4
      XEX(1)=.14095D+2
      XEX(2)=.84296079D+0
      RETURN
    2 FX=(X(1)-.1D+2)**3+(X(2)-.2D+2)**3
      RETURN
    3 GF(1)=.3D+1*(X(1)-.1D+2)**2
      GF(2)=.3D+1*(X(2)-.2D+2)**2
      RETURN
    4 IF (INDEX1(1)) G(1)=(X(1)-.5D+1)**2+(X(2)-.5D+1)**2-.1D+3
      IF (INDEX1(2)) G(2)=(X(1)-.6D+1)**2+(X(2)-.5D+1)**2
      IF (INDEX1(3)) G(3)=.8281D+2-(X(1)-.6D+1)**2-(X(2)-.5D+1)**2
      RETURN
    5 IF (.NOT.INDEX2(1)) GOTO 6
      GG(1,1)=.2D+1*X(1)-.1D+2
      GG(1,2)=.2D+1*X(2)-.1D+2
    6 IF (.NOT.INDEX2(2)) GOTO 7
      GG(2,1)=.2D+1*X(1)-.12D+2
      GG(2,2)=.2D+1*X(2)-.1D+2
    7 IF (.NOT.INDEX2(3)) RETURN
      GG(3,1)=-.2D+1*X(1)+.12D+2
      GG(3,2)=-.2D+1*X(2)+.1D+2
      RETURN
      END
