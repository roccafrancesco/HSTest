      SUBROUTINE TP363(MODE)
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
      DOUBLEPRECISION TP363A
      GOTO (1,2,3,4,5),MODE
    1 N=5
      NILI=0
      NINL=3
      NELI=0
      NENL=0
      X(1)=-.3359769D+0
      X(2)=-.1432398D+1
      X(3)=0.D+0
      X(4)=4.D+0
      X(5)=9.D+0
      DO 11 I=1,5
      LXL(I)=.TRUE.
      LXU(I)=.TRUE.
   11 XU(I)=1.D+1
      XL(1)=-1.D+1
      XL(2)=-1.D+1
      XL(3)=-1.D+1
      XL(4)=.1D+0
      XL(5)=1.D+0
      LEX=.FALSE.
      NEX=1
      XEX(1)=.19852438D+0
      XEX(2)=-3.01059794D+0
      XEX(3)=-0.0530266138D+0
      XEX(4)=2.83165094D+0
      XEX(5)=10.D+0
      FEX=-5.55840576
      RETURN
    2 FX=TP363A(X)
    3 RETURN
    4 CALL TP363B(INDEX1,X,G)
    5 RETURN
      END
