      SUBROUTINE TP374(MODE)	
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
      INTEGER I,K
      DOUBLEPRECISION Z,PI,TP374A,TP374B,TP374G
      GOTO(1,2,3,4,5)MODE
    1 N=10
      NILI=0
      NINL=35
      NELI=0
      NENL=0
      DO 6 I=1,10
      X(I)=0.1D+0
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.FALSE.
      NEX=2
      FEX=0.233264D+0
      XEX(1)=0.218212D+0
      XEX(2)=0.232640D+0
      XEX(3)=0.278457D+0
      XEX(4)=0.268125D+0
      XEX(5)=0.212010D+0
      XEX(6)=0.125918D+0
      XEX(7)=0.34102D-1
      XEX(8)=-0.26136D-1
      XEX(9)=-0.142233D+0
      XEX(10)=0.233264D+0
      XEX(11)=-0.142233D+0
      XEX(12)=-0.26136D-1
      XEX(13)=0.34102D-1
      XEX(14)=0.125918D+0
      XEX(15)=0.212010D+0
      XEX(16)=0.268125D+0
      XEX(17)=0.278457D+0
      XEX(18)=0.23264D+0
      XEX(19)=0.218212D+0
      XEX(20)=0.233264D+0
      DO 46 I=1,9
   46 GF(I)=0.0D+0
      GF(10)=0.1D+1
      RETURN
    2 FX=X(10)
    3 RETURN
    4 PI=0.4D+1*DATAN(0.1D+1)
      DO 8 I=1,10
      Z=PI/4.D+0*(DBLE(I-1)*0.1D+0)
    8 IF (INDEX1(I)) G(I)=TP374G(Z,X)-(1.D+0-X(10))**2
      DO 9 I=11,20
      Z=PI/4.D+0*(DBLE(I-11)*0.1D+0)
    9 IF(INDEX1(I)) G(I)=(1.D+0+X(10))**2-TP374G(Z,X)
      DO 10 I=21,35
      Z=PI/4.D+0*(1.2D+0+DBLE(I-21)*0.2D+0)
   10 IF(INDEX1(I)) G(I)=X(10)**2-TP374G(Z,X)
      RETURN
    5 PI=0.4D+1*DATAN(0.1D+1)
      DO 50 I=1,10
      IF (.NOT.INDEX2(I)) GOTO 50
      Z=PI/4.D+0*(DBLE(I-1)*0.1D+0)
      DO 51 K=1,9
      GG(I,K)=2.D+0*(TP374A(Z,X)*DCOS(K*Z)+TP374B(Z,X)*DSIN(K*Z))
   51 GG(I,10)=2.D+0*(1.D+0-X(10))
   50 CONTINUE
      DO 52 I=11,20
      IF (.NOT.INDEX2(I)) GOTO 52
      Z=PI/4.D+0*(DBLE(I-11)*0.1D+0)
      DO 53 K=1,9
      GG(I,K)=-2.D+0*(TP374A(Z,X)*DCOS(K*Z)+TP374B(Z,X)*DSIN(K*Z))
   53 GG(I,10)=2.D+0*(1.D+0+X(10))
   52 CONTINUE
      DO 54 I=21,35
      IF (.NOT.INDEX2(I)) GOTO 54
      Z=PI/4.D+0*(1.2D+0+DBLE(I-21)*0.2D+0)
      DO 55 K=1,9
      GG(I,K)=-2.D+0*(TP374A(Z,X)*DCOS(K*Z)+TP374B(Z,X)*DSIN(K*Z))
   55 GG(I,10)=2.D+0*X(10)
   54 CONTINUE
      RETURN
      END
