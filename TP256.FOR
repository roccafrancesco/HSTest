      SUBROUTINE TP256(MODE)
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
      GOTO (1,2,3,4,4),MODE
    1 N=4
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      X(1)=3.D+0
      X(2)=-1.D+0
      X(3)=0.D+0
      X(4)=1.D+0
      DO 6 I=1,4
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.TRUE.
      NEX=1
      FEX=0.D+0
      DO 7 I=1,4
    7 XEX(I)=0.D+0
      RETURN
    2 FX=((X(1)+1.D+1*X(2))**2+5.D+0*(X(3)-X(4))**2 +
     /   (X(2)-2.D+0*X(3))**4+1.D+1*(X(1)-X(4))**4)
      RETURN
    3 GF(1)=(2.D+0*(X(1)+1.D+1*X(2))+4.D+1*(X(1)-X(4))**3)
      GF(2)=(2.D+1*(X(1)+1.D+1*X(2))+4.D+0*(X(2)-2.D+0*X(3))**3)
      GF(3)=(1.D+1*(X(3)-X(4))-8.D+0*(X(2)-2.D+0*X(3))**3)
      GF(4)=(-1.D+1*(X(3)-X(4))-4.D+1*(X(1)-X(4))**3)
    4 RETURN
      END
