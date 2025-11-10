      SUBROUTINE TP253(MODE)
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
      DOUBLEPRECISION A(3,8)
      DATA ((A(I,J),I=1,3),J=1,8)
     1     /3*0.D+0,10.D+0,2*0.D+0,2*10.D+0,2*0.D+0,10.D+0,3*0.D+0,
     2      2*10.D+0,0.D+0,4*10.D+0,0.D+0,2*10.D+0/
      INTEGER I,J
      GOTO (1,2,3,4,5),MODE
    1 N=3
      NILI=1
      NINL=0
      NELI=0
      NENL=0
      X(1)=0.D+0
      X(2)=2.D+0
      X(3)=0.D+0
      DO 6 I=1,3
      LXU(I)=.FALSE.
      LXL(I)=.TRUE.
    6 XL(I)=0.D+0
      GG(1,1)=-3.D+0
      GG(1,2)=0.D+0
      GG(1,3)=-3.D+0
      LEX=.TRUE.
      NEX=1
      FEX=0.69282032D+02
      DO 7 I=1,3
    7 XEX(I)=5.0D0
      RETURN
    2 FX=0.D+0
      DO 8 J=1,8
    8 FX=FX+DSQRT((A(1,J)-X(1))**2+(A(2,J)-X(2))**2 +
     /      (A(3,J)-X(3))**2)
      RETURN
    3 DO 9 I=1,3
      GF(I)=0.D+0
      DO 9 J=1,8
    9 GF(I)=GF(I)+(X(I)-A(I,J))/DSQRT((A(1,J)-X(1))**2 +
     /      (A(2,J)-X(2))**2+(A(3,J)-X(3))**2)
      RETURN
    4 IF (INDEX1(1)) G(1)=3.D+1-3.D+0*X(1)-3.D+0*X(3)
    5 RETURN
      END
