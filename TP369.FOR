      SUBROUTINE TP369(MODE)
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
      DOUBLEPRECISION C(16)
      DATA C/833.33252D+0,100.D+0,-83333.333D+0,1250.D+0,1.D+0,-1250.D+0
     F ,1250000.D+0,1.D+0,-2500.D+0,2.5D-3,2.5D-3,2.5D-3,2.5D-3,-2.5D-3
     F ,1.D-2,-1.D-2/   
      GOTO (1,2,3,4,5),MODE
    1 N=8 
      NILI=3
      NINL=3 
      NELI=0 
      NENL=0
      X(1)=5.D+3
      X(2)=5.D+3
      X(3)=5.D+3
      X(4)=2.D+2
      X(5)=3.5D+2
      X(6)=1.5D+2
      X(7)=225.D+0
      X(8)=425.D+0        
      DO 11 I=1,8
      LXU(I)=.TRUE.
   11 LXL(I)=.TRUE.
      XL(1)=1.D+2
      XL(2)=1.D+3
      XL(3)=1.D+3
      DO 12 I=1,3
   12 XU(I)=1.D+4
      DO 13 I=4,8
      XL(I)=10.D+0
   13 XU(I)=1.D+3
      LEX=.FALSE.
      NEX=1
      FEX=0.70492480D+4
      XEX(1)=0.57930657D+3
      XEX(2)=0.13599705D+4
      XEX(3)=0.51099709D+4
      XEX(4)=0.18201769D+3
      XEX(5)=0.29560116D+3
      XEX(6)=0.21798231D+3
      XEX(7)=0.28641653D+3
      XEX(8)=0.39560116D+3
      RETURN
    2 FX=X(1)+X(2)+X(3)
    3 RETURN
    4 CONTINUE
      DO I=1,5
      IF (X(I).GT.XU(I)) X(I)=XU(I)
      IF (X(I).LT.XL(I)) X(I)=XL(I)
      ENDDO 
      IF (INDEX1(1)) G(1)=1.0-C(10)*X(4)-C(11)*X(6)
      IF (INDEX1(2)) G(2)=1.0-C(12)*X(5)-C(13)*X(7)-C(14)*X(4)
      IF (INDEX1(3)) G(3)=1.0-C(15)*X(8)-C(16)*X(5)
      IF (INDEX1(4)) G(4)=1.0-C(1)/X(1)*X(4)/X(6)-C(2)/X(6)
     F -C(3)/X(1)/X(6)
      IF (INDEX1(5)) G(5)=1.0-C(4)/X(2)*X(5)/X(7)-C(5)*X(4)/X(7)
     F -C(6)/X(2)*X(4)/X(7)
      IF (INDEX1(6)) G(6)=1.0-C(7)/X(3)/X(8)-C(8)*X(5)/X(8)            
     F -C(9)/X(3)*X(5)/X(8)
    5 RETURN
      END
