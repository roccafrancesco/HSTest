      SUBROUTINE TP287(MODE)
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
      GOTO(1,2,3,4,4),MODE
    1 N=20
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,5
      X(I)=-.3D+1
      X(I+5)=-.1D+1
      X(I+10)=-.3D+1
   6  X(I+15)=-.1D+1
      DO 7 I=1,20
      LXU(I)=.FALSE.
      LXL(I)=.FALSE.
    7 XEX(I)=.1D+1
      LEX=.TRUE.
      NEX=1
      FEX=.0D+0
      RETURN
    2 FX=.0D+0
      DO 8 I=1,5
    8 FX=FX+.1D+3*(X(I)**2-X(I+5))**2+(X(I)-.1D+1)**2+
     F   .9D+2*(X(I+10)**2-X(I+15))**2+(X(I+10)-.1D+1)**2+
     F   .101D+2*((X(I+5)-.1D+1)**2+(X(I+15)-.1D+1)**2)+
     F   .198D+2*(X(I+5)-.1D+1)*(X(I+15)-.1D+1)
      FX=FX*1.0D-5
      RETURN
    3 DO 9 I=1,5
      GF(I)=.4D+3*(X(I)**2-X(I+5))*X(I)+.2D+1*(X(I)-.1D+1)
      GF(I)=GF(I)*1.0d-5
      GF(I+5)=-.2D+3*(X(I)**2-X(I+5))+.202D+2*(X(I+5)-.1D+1)+
     F        .198D+2*(X(I+15)-.1D+1)
      GF(I+5)=GF(I+5)*1.0d-5
      GF(I+10)=.36D+3*X(I+10)*(X(I+10)**2-X(I+15))+.2D+1*(X(I+10)-.1D+1)
      GF(I+10)=GF(I+10)
      GF(I+15)=-.18D+3*(X(I+10)**2-X(I+15))+.202D+2*(X(I+15)-.1D+1)+
     F         .198D+2*(X(I+5)-.1D+1)
    9 GF(I+15)=GF(I+15)*1.0d-5
    4 RETURN
      END
