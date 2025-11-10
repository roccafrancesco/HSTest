      SUBROUTINE TP353(MODE)
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
      DOUBLEPRECISION Q
      GOTO (1,2,3,4,4),MODE
    1 N=4
      NILI=1
      NINL=1
      NELI=1
      NENL=0
      X(1)=0.0D+0
      X(2)=0.0D+0
      X(3)=0.4D+0
      X(4)=0.6D+0
      GG(1,1)=0.23D+1
      GG(1,2)=0.56D+1
      GG(1,3)=0.111D+2
      GG(1,4)=0.13D+1
      DO 6 I=1,4
      GG(3,I)=0.1D+1
      LXL(I)=.TRUE.
      LXU(I)=.FALSE.
    6 XL(I)=0.0D+0
      LEX=.FALSE.
      NEX=1
      FEX=-0.39933673D+2
      XEX(1)=0.D+0
      XEX(2)=0.D+0
      XEX(3)=0.37755102D+0
      XEX(4)=0.62244898D+0
      RETURN
    2 FX=0.2455D+2*X(1)+0.2675D+2*X(2)+0.39D+2*X(3)+0.405D+2*X(4)
      FX=-FX
      RETURN
    3 GF(1)=-0.2455D+2
      GF(2)=-0.2675D+2
      GF(3)=-0.39D+2
      GF(4)=-0.405D+2
      RETURN
    4 Q=(0.53D+0*X(1))**2+(0.44D+0*X(2))**2+(0.45D+1*X(3))**2
     F  +(0.79D+0*X(4))**2
      IF (MODE.EQ.5) GOTO 5
      IF (INDEX1(1)) G(1)=0.23D+1*X(1)+0.56D+1*X(2)+0.111D+2*X(3)
     F                    +0.13D+1*X(4)-0.5D+1
      IF (INDEX1(2)) G(2)=0.12D+2*X(1)+0.119D+2*X(2)+0.418D+2*X(3)
     F               +0.521D+2*X(4)-0.1645D+1*DSQRT(Q)-0.12D+2
      IF (INDEX1(3)) G(3)=X(1)+X(2)+X(3)+X(4)-0.1D+1
      RETURN
    5 IF (.NOT. INDEX2(2)) GOTO 7
      GG(2,1)=0.12D+2-0.1645D+1*0.53D+0**2*X(1)/DSQRT(Q)
      GG(2,2)=0.119D+2-0.1645D+1*0.44D+0**2*X(2)/DSQRT(Q)
      GG(2,3)=0.418D+2-0.1645D+1*0.45D+1**2*X(3)/DSQRT(Q)
      GG(2,4)=0.521D+2-0.1645D+1*0.79D+0**2*X(4)/DSQRT(Q)
    7 RETURN
      END
