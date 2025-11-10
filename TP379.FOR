      SUBROUTINE TP379(MODE)
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
      INTEGER I,J,LSUM
      DOUBLEPRECISION F,DF,Y(65),T
      DATA Y/1.366D+0,1.191D+0,1.112D+0,1.013D+0,0.991D+0,
     1 0.885D+0,0.831D+0,0.847D+0,0.786D+0,0.725D+0,0.746D+0,
     2 0.679D+0,0.608D+0,0.655D+0,0.616D+0,0.606D+0,0.602D+0,
     3 0.626D+0,0.651D+0,0.724D+0,0.649D+0,0.649D+0,0.694D+0,
     4 0.644D+0,0.624D+0,0.661D+0,0.612D+0,0.558D+0,0.533D+0,
     5 0.495D+0,0.500D+0,0.423D+0,0.395D+0,0.375D+0,0.372D+0,
     6 0.391D+0,0.396D+0,0.405D+0,0.428D+0,0.429D+0,0.523D+0,
     7 0.562D+0,0.607D+0,0.653D+0,0.672D+0,0.708D+0,0.633D+0,
     8 0.668D+0,0.645D+0,0.632D+0,0.591D+0,0.559D+0,0.597D+0,
     9 0.625D+0,0.739D+0,0.710D+0,0.729D+0,0.720D+0,0.636D+0,
     A 0.581D+0,0.428D+0,0.292D+0,0.162D+0,0.098D+0,0.054D+0/
      GOTO(1,2,2,4,4),MODE
    1 N=11
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      LSUM=65
      X(1)=1.3D+0
      X(2)=0.65D+0
      X(3)=0.65D+0
      X(4)=0.7D+0
      X(5)=0.6D+0
      X(6)=3.0D+0
      X(7)=5.0D+0
      X(8)=7.0D+0
      X(9)=2.0D+0
      X(10)=4.5D+0
      X(11)=5.5D+0
      DO 6 I=1,11
      LXU(I)=.FALSE.
      XL(I)=0.0
    6 LXL(I)=.TRUE.
      LEX=.FALSE.
      NEX=1
      FEX=0.401377D-1
      XEX(1)=0.130997D+1
      XEX(2)=0.431554D+0
      XEX(3)=0.63366D+0
      XEX(4)=0.59943D+0
      XEX(5)=0.754183D+0
      XEX(6)=0.904286D+0
      XEX(7)=0.136581D+1
      XEX(8)=0.482369D+1
      XEX(9)=0.239868D+1
      XEX(10)=0.456887D+1
      XEX(11)=0.567534D+1
      RETURN
    2 CONTINUE
      DO I=1,11
      IF (X(I).GT.XU(I)) X(I)=XU(I)
      IF (X(I).LT.XL(I)) X(I)=XL(I)
      ENDDO 
      DO 7 I=1,65
      T=.1D+0*DBLE(I-1)
    7 F(I)=Y(I)-(X(1)*DEXP(-X(5)*T)+X(2)*DEXP(-X(6)*(T-X(9))**2)
     2 +X(3)*DEXP(-X(7)*(T-X(10))**2)+X(4)*DEXP(-X(8)*(T-X(11))**2))
      IF (MODE.EQ.3) GOTO 3
      FX=0.D+0
      DO 70 I=1,65
   70 FX=FX+F(I)**2
      RETURN
    3 DO 8 I=1,65
      T=.1D+0*DBLE(I-1)
      DF(I,1)=-DEXP(-X(5)*T)
      DF(I,2)=-DEXP(-X(6)*(T-X(9))**2)
      DF(I,3)=-DEXP(-X(7)*(T-X(10))**2)
      DF(I,4)=-DEXP(-X(8)*(T-X(11))**2)
      DF(I,5)=X(1)*T*DEXP(-X(5)*T)
      DF(I,6)=X(2)*(T-X(9))**2*DEXP(-X(6)*(T-X(9))**2)
      DF(I,7)=X(3)*(T-X(10))**2*DEXP(-X(7)*(T-X(10))**2)
      DF(I,8)=X(4)*(T-X(11))**2*DEXP(-X(8)*(T-X(11))**2)
      DF(I,9)=-X(2)*X(6)*2.D+0*(T-X(9))*DEXP(-X(6)*(T-X(9))**2)
      DF(I,10)=-X(3)*X(7)*2.D+0*(T-X(10))*DEXP(-X(7)*(T-X(10))**2)
    8 DF(I,11)=-X(4)*X(8)*2.D+0*(T-X(11))*DEXP(-X(8)*(T-X(11))**2)
      DO 19 I=1,11
   19 GF(I)=0.D+0
      DO 20 J=1,11
      DO 20 I=1,65
   20 GF(J)=GF(J)+2.D+0*F(I)*DF(I,J)
    4 RETURN
      END
