      SUBROUTINE TP268(MODE) 
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
      DOUBLEPRECISION HF
      INTEGER HGG(5,5),DD(5,5),DDVEKT(5),DVDV 
      DATA HGG /-1,10,-8,8,-4,-1,10,1,-1,-2,-1,-3,-2,2,3,-1,5, 
     1          -5,5,-5,-1,4,3,-3,1/
C KONSTANTE DATEN DER ZIELFUNKTION:
C         DD=D'*D
C         DDVEKT=DVEKT'*D
C         DVDV=DVEKT'*DVEKT=14463
C MIT
C          -                      -
C          |  -74   80   18  -11  -4  |
C          |   14  -69   21   28   0  |
C     D= |   66  -72   -5    7   1  |
C          |  -12   66  -30  -23   3  |
C          |    3    8   -7   -4   1  |
C          |    4  -12    4    4   0  |
C          -                       -
C
C     DVEKT=( 51, -61, -56, 69, 10, -12 )'
      DATA DD /10197,-12454,-1013,1948,329,-12454,20909,-1733,-4914,
     1         -186,-1013,-1733,1755,1089,-174,1948,-4914,1089,1515,
     2         -22,329,-186,-174,-22,27/
      DATA DDVEKT,DVDV/-9170,17099,-2271,-4336,-43,14463/
      GOTO (1,2,3,4,5),MODE 
 1    N=5 
      NILI=5 
      NINL=0 
      NELI=0 
      NENL=0 
      DO 6 I=1,5 
      X(I)=0.1D+1 
      LXL(I)=.FALSE.
      LXU(I)=.FALSE.
      DO 6 J=1,5 
 6    GG(I,J)=DBLE(HGG(I,J)) 
      LEX=.TRUE.
      NEX=1 
      FEX=0.D+0 
      XEX(1)=0.1D+1 
      XEX(2)=0.2D+1 
      XEX(3)=-0.1D+1 
      XEX(4)=0.3D+1 
      XEX(5)=-0.4D+1 
      RETURN 
 2    FX=DVDV 
      DO 7 I=1,5 
      HF=0.D+0
      DO 8 J=1,5 
 8    HF=HF+DBLE(DD(I,J))*X(J) 
 7    FX=FX+X(I)*(HF-0.2D+1*DBLE(DDVEKT(I)))
      RETURN 
 3    DO 9 I=1,5 
      GF(I)=- 0.2D+1*DBLE(DDVEKT(I)) 
      DO 9 J=1,5 
 9    GF(I)=GF(I)+DBLE(DD(I,J)+DD(J,I))*X(J) 
      RETURN 
 4    IF (INDEX1(1)) G(1)=-X(1)-X(2)-X(3)-X(4)-X(5)+5.0D0 
      IF (INDEX1(2)) G(2)=0.1D+2*X(1)+0.1D+2*X(2) 
     1  -0.3D+1*X(3)+0.5D+1*X(4)+0.4D+1*X(5)-0.2D+2 
      IF (INDEX1(3)) G(3)=-0.8D+1*X(1)+X(2)-0.2D+1*X(3) 
     1  -0.5D+1*X(4)+0.3D+1*X(5)+0.4D+2 
      IF (INDEX1(4)) G(4)=0.8D+1*X(1)-X(2)+0.2D+1*X(3) 
     1  +0.5D+1*X(4)-0.3D+1*X(5)-0.11D+2 
      IF (INDEX1(5)) G(5)=-0.4D+1*X(1)-0.2D+1*X(2) 
     1  +0.3D+1*X(3)-0.5D+1*X(4)+X(5)+0.3D+2 
 5    RETURN 
      END 
