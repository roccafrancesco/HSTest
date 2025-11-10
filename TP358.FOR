      SUBROUTINE TP358(MODE)
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
     /           /L15/ F(200)    
     /           /L16/ DF(200,200)    
     /           /L20/ LEX,NEX,FEX,XEX(NMAX) 
      DOUBLEPRECISION Y(33),TI,F,DF
      INTEGER I,J,LSUM
      DATA(Y(I),I=1,33)/0.844D+0,0.908D+0,0.932D+0,0.936D+0,
     F          0.925D+0,0.908D+0,0.881D+0,0.850D+0,0.818D+0,
     F          0.784D+0,0.751D+0,0.718D+0,0.685D+0,0.658D+0,
     F          0.628D+0,0.603D+0,0.580D+0,0.558D+0,0.538D+0,
     F          0.522D+0,0.506D+0,0.490D+0,0.478D+0,0.467D+0,
     F          0.457D+0,0.448D+0,0.438D+0,0.431D+0,0.424D+0,
     F          0.420D+0,0.414D+0,0.411D+0,0.406D+0/
      GOTO(1,2,2,4,4),MODE
    1 N=5
      NILI=0
      NINL=0
      NENL=0
      NENL=0
      X(1)=0.5D+0
      X(2)=0.15D+1
      X(3)=-0.1D+1
      X(4)=0.1D-1
      X(5)=0.2D-1
      DO 6 I=1,5
      LXU(I)=.TRUE.
    6 LXL(I)=.TRUE.
      XL(1)=-0.5D+0
      XU(1)=0.5D+0
      XL(2)=.15D+1
      XU(2)=.25D+1
      XL(3)=-0.2D+1
      XU(3)=-0.1D+1
      XL(4)=0.1D-2
      XU(4)=0.1D+0
      XL(5)=0.1D-2
      XU(5)=0.1D+0
      LEX=.FALSE.
      NEX=1
      FEX=0.546D-4
      XEX(1)=0.3754D+0
      XEX(2)=0.19358D+1
      XEX(3)=-0.14647D+1
      XEX(4)=0.1287D-1
      XEX(5)=0.2212D-1
      LSUM=33
      RETURN
    2 CONTINUE
      DO I=1,5
      IF (X(I).GT.XU(I)) X(I)=XU(I)
      IF (X(I).LT.XL(I)) X(I)=XL(I)
      ENDDO 
      DO 20 I=1,33
      TI=DBLE((I-1))*0.1D+2
   20 F(I)=Y(I)-(X(1)+X(2)*DEXP(-X(4)*TI)+X(3)*DEXP(-X(5)*TI))
      IF (MODE.EQ.3) GOTO 3
      FX=0.0D+0
      DO 7 I=1,33
    7 FX=FX+F(I)**2
      RETURN
    3 DO 8 J=1,5
    8 GF(J)=0.0D+0
      DO 10 I=1,33
      TI=DBLE((I-1))*0.1D+2
      DF(I,1)=-0.1D+1
      DF(I,2)=-DEXP(-X(4)*TI)
      DF(I,3)=-DEXP(-X(5)*TI)
      DF(I,4)=X(2)*DEXP(-X(4)*TI)*TI
      DF(I,5)=X(3)*DEXP(-X(5)*TI)*TI
      DO 10 J=1,5
   10 GF(J)=GF(J)+2.D+0*F(I)*DF(I,J)
    4 RETURN
      END
