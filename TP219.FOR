      SUBROUTINE TP219(MODE)
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
    1 N=4
      NILI=0
      NINL=0
      NELI=0 
      NENL=2
      DO 8 I=1,4
      X(I)=10.D+0
      LXU(I)=.FALSE.
    8 LXL(I)=.FALSE.
      GG(1,2)=1.D+0
      GG(1,4)=0.D+0
      GG(2,2)=-1.D+0
      GG(2,3)=0.D+0  
      LEX=.TRUE.
      NEX=1
      FEX=-1.D+0
      XEX(1)=1.D+0
      XEX(2)=1.D+0
      XEX(3)=0.D+0
      XEX(4)=0.D+0
      RETURN 
    2 FX=-X(1)
      RETURN
    3 GF(1)=-1.D+0
      GF(2)=0.D+0
      GF(3)=0.D+0
      GF(4)=0.D+0
      RETURN
    4 IF (INDEX1(1)) G(1)=X(2)-X(1)**3-X(3)**2
      IF (INDEX1(2)) G(2)=X(1)**2-X(2)-X(4)**2
      RETURN
    5 IF (.NOT.INDEX2(1)) GOTO 6
      GG(1,1)=-3.D+0*X(1)**2
      GG(1,3)=-2.D+0*X(3)
    6 IF (.NOT.INDEX2(2)) GOTO 7
      GG(2,1)=2.D+0*X(1)
      GG(2,4)=-2.D+0*X(4)  
    7 RETURN
      END
