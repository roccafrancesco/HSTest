      SUBROUTINE TP277(MODE)
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
      INTEGER I,J
      DOUBLEPRECISION H
      N=4
      GOTO 10
      ENTRY TP278(MODE)
      N=6
      GOTO 10
      ENTRY TP279(MODE)
      N=8
      GOTO 10
      ENTRY TP280(MODE)
      N=10
   10 GOTO (1,2,3,4,5), MODE
    1 NILI=N
      NINL=0
      NELI=0
      NENL=0
      FEX=0.D+0
      DO 6 I=1,N
      X(I)=0.D+0
      DO 16 J=1,N
      FEX=FEX+1.D+0/DBLE(I+J-1)
   16 GG(J,I)=1.D+0/DBLE(I+J-1)
      LXL(I)=.TRUE.
      LXU(I)=.FALSE.
      XL(I)=0.D+0
    6 XEX(I)=1.D+0
      LEX=.TRUE.
      NEX=1
      RETURN
    2 FX=0.D+0
      DO 7 I=1,N
      H=0.D+0
      DO 17 J=1,N
   17 H=H+1.D+0/DBLE(I+J-1)
    7 FX=FX+H*X(I)
      RETURN
    3 DO 8 I=1,N
      H=0.D+0
      DO 18 J=1,N
   18 H=H+1.D+0/DBLE(I+J-1)
    8 GF(I)=H
      RETURN
    4 DO 9 I=1,N
      H=0.D+0
      DO 19 J=1,N
   19 H=H+(X(J)-1.D+0)/DBLE(I+J-1)
    9 IF (INDEX1(I)) G(I)=H
    5 RETURN
      END
