      SUBROUTINE TP224(MODE)
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
      GOTO(1,2,3,4,5),MODE
    1 N=2
      NILI=4
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,2
      X(I)=.1D+0
      LXU(I)=.TRUE.
      LXL(I)=.TRUE.
      XU(I)=6.D+0
    6 XL(I)=0.D+0
      GG(1,1)=1.D+0
      GG(1,2)=3.D+0
      GG(2,1)=-1.D+0
      GG(2,2)=-3.D+0
      GG(3,1)=1.D+0
      GG(3,2)=1.D+0
      GG(4,1)=-1.D+0
      GG(4,2)=-1.D+0
      LEX=.TRUE.
      NEX=1
      FEX=-304.D+0
      XEX(1)=4.D+0
      XEX(2)=4.D+0
      RETURN
    2 FX=2.D+0*X(1)**2+X(2)**2-48.D+0*X(1)-40.D+0*X(2)
      RETURN
    3 GF(1)=4.D+0*X(1)-48.D+0
      GF(2)=2.D+0*X(2)-40.D+0
      RETURN
    4 IF (INDEX1(1)) G(1)=X(1)+3.D+0*X(2)
      IF (INDEX1(2)) G(2)=18.D+0-X(1)-3.D+0*X(2)
      IF (INDEX1(3)) G(3)=X(1)+X(2)
      IF (INDEX1(4)) G(4)=8.D+0-X(1)-X(2)
    5 RETURN
      END
