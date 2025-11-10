      SUBROUTINE TP238(MODE)
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
      GOTO (1,2,3,4,5),MODE
    1 N=2
      NILI=0
      NINL=3
      NELI=0
      NENL=0
      X(1)=1.D+1
      X(2)=1.D+1
      DO 6 I=1,2
      LXU(I)=.TRUE.
    6 LXL(I)=.FALSE.
      XU(1)=75.D+0
      XU(2)=65.D+0
      GG(2,2)=1.D+0
      GG(3,1)=-5.D+0
      LEX=.TRUE.
      NEX=1
      FEX=-58.9034360D+0
      XEX(1)=75.D+0
      XEX(2)=65.D+0
      RETURN
    2 CALL TP236239(1)
      RETURN
    3 CALL TP236239(2)
      RETURN
    4 IF (INDEX1(1)) G(1)=X(1)*X(2)-7.D+2
      IF (INDEX1(2)) G(2)=X(2)-5.D+0*((X(1)/25.D+0)**2)
      IF (INDEX1(3)) G(3)=(X(2)-5.D+1)**2-5.D+0*(X(1)-55.D+0)
      RETURN
    5 IF (.NOT. INDEX2(1)) GOTO 7
      GG(1,1)=X(2)
      GG(1,2)=X(1)
    7 IF (INDEX2(2)) GG(2,1)=-10.D+0/625.D+0*X(1)
      IF (INDEX2(3)) GG(3,2)=2.D+0*X(2)-1.D+2
      RETURN
      END
