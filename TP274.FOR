      SUBROUTINE TP274(MODE)
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
      COMMON     /D274/A(6,6)
      DOUBLEPRECISION A
      INTEGER I,J
      N=2
      GOTO 10
      ENTRY TP275(MODE)
      N=4
      GOTO 10
      ENTRY TP276(MODE)
      N=6
   10 GOTO (1,2,3,4,4), MODE
    1 NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,N
      X(I)=-4.D+0/DBLE(I)
      DO 11 J=1,N
   11 A(I,J)=1.D+0/DBLE(I+J-1)
      XEX(I)=0.D+0
      LXL(I)=.FALSE.
    6 LXU(I)=.FALSE.
      LEX=.TRUE.
      NEX=1
      FEX=0.D+0
      RETURN
    2 FX=0.D+0
      DO 7 I=1,N
      DO 7 J=1,N
    7 FX=FX+A(I,J)*X(I)*X(J)
      RETURN
    3 DO 8 I=1,N
      GF(I)=0.D+0
      DO 8 J=1,N
    8 GF(I)=GF(I)+X(J)*(A(I,J)+A(J,I))
    4 RETURN
      END
