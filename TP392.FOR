      SUBROUTINE TP392 (MODE)
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
      INTEGER I,J,K,L
      DOUBLE PRECISION F,DF,SUM1,SUM,R1(3,5),R2(3,5),KA(3,5),K1(3,5),
     F       KP(3,5),K3(3,5),KL1(3,5),KL2(3,5),H(3,5),B(3,5),T(3,3)
      DATA R1/1.D+3,5.2D+2,9.1D+2,1.D+3,5.2D+2,9.1D+2,1.D+3,5.2D+2,
     F   1.D+3,1.1D+3,6.D+2,1.D+3,1.1D+3,6.D+2,1.D+3/
      DATA R2/0.3D+0,0.1D+0,0.2D+0,0.3D+0,0.1D+0,0.2D+0,0.3D+0,0.1D+0,
     F   0.2D+0,0.3D+0,0.1D+0,0.2D+0,0.3D+0,0.1D+0,0.2D+0/
      DATA KA/1.2D+2,6.5D+1,1.05D+2,1.5D+2,6.5D+1,1.05D+2,1.5D+2,8.D+1,
     F   1.2D+2,1.7D+2,8.D+1,1.2D+2,1.7D+2,8.D+1,1.2D+2/
      DATA K1/1.5D+2,7.5D+1,1.4D+2,1.5D+2,7.5D+1,1.4D+2,1.5D+2,7.5D+1,
     F   1.4D+2,1.7D+2,9.D+1,1.5D+2,1.7D+2,9.D+1,1.5D+2/
      DATA KP/1.6D+2,7.5D+1,1.4D+2,1.6D+2,7.5D+1,1.4D+2,1.6D+2,7.5D+1,
     F   1.4D+2,1.8D+2,9.D+1,1.5D+2,1.8D+2,9.D+1,1.5D+2/
      DATA K3/.2D-1,.1D-1,.15D-1,.2D+0,.1D+0,.15D+0,.25D+0,.1D+0,.15D+0,
     F   .25D+0,2*.15D+0,.25D+0,2*.15D+0/
      DATA KL1/3*.5D-2,3*.5D-1,9*.6D-1/
      DATA KL2/8.D+1,4.5D+1,7.5D+1,8.D+1,4.5D+1,7.5D+1,1.D+2,4.5D+1,
     F   9.D+1,1.D+2,5.D+1,9.D+1,1.D+2,5.D+1,9.D+1/
      DATA H/1.D+2,2.8D+2,5.2D+2,1.8D+2,2*4.D+2,2.2D+2,4.5D+2,5.D+2,
     F   1.5D+2,4.5D+2,6.3D+2,1.D+2,4.D+2,6.D+2/
      DATA T/.6D+0,.3D+0,.36D+0,.4D+0,.1D+0,.8D-1,.1D+0,.12D+0,.6D-1/
      DATA B/2*1.7D+2,1.8D+2,2*1.7D+2,1.8D+2,2*1.7D+2,1.8D+2,2*1.7D+2,
     F   1.8D+2,2*1.7D+2,1.8D+2/
      GOTO (1,2,3,4,5),MODE
    1 N=30
      NILI=45
      NINL=0
      NELI=0
      NENL=0
      X(1)=80.D+0
      X(2)=100.D+0
      X(3)=400.D+0
      X(4)=100.D+0
      X(5)=200.D+0
      X(6)=200.D+0
      X(7)=100.D+0
      X(8)=250.D+0
      X(9)=400.D+0
      X(10)=50.D+0
      X(11)=200.D+0
      X(12)=500.D+0
      X(13)=50.D+0
      X(14)=200.D+0
      X(15)=500.D+0
      X(16)=100.D+0
      X(17)=120.D+0
      X(18)=410.D+0
      X(19)=120.D+0
      X(20)=250.D+0
      X(21)=250.D+0
      X(22)=150.D+0
      X(23)=300.D+0
      X(24)=410.D+0
      X(25)=600.D+0
      X(26)=250.D+0
      X(27)=510.D+0
      X(28)=100.D+0
      X(29)=250.D+0
      X(30)=510.D+0
      DO 6 I=1,30
      LXU(I)=.FALSE.
      LXL(I)=.TRUE.
    6 XL(I)=0.D+0
      LEX=.FALSE.
      NEX=1
      FEX=-1.6960671D+6
      XEX(1)=100.00D+0
      XEX(2)=145.00D+0
      XEX(3)=520.00D+0
      XEX(4)=171.43D+0
      XEX(5)=67.86D+0
      XEX(6)=400.00D+0
      XEX(7)=177.37+0
      XEX(8)=4.74D+0
      XEX(9)=500.D+0
      XEX(10)=150.00D+0
      XEX(11)=71.70D+0
      XEX(12)=630.00D+0
      XEX(13)=100.00D+0
      XEX(14)=125.00D+0
      XEX(15)=600.00D+0
      XEX(16)=100.00D+0
      XEX(17)=145.00D+0
      XEX(18)=520.00D+0
      XEX(19)=171.43D+0
      XEX(20)=67.86D+0
      XEX(21)=400.00D+0
      XEX(22)=177.37D+0
      XEX(23)=33.94D+0
      XEX(24)=500.00D+0
      XEX(25)=150.00D+0
      XEX(26)=42.50D+0
      XEX(27)=630.00D+0
      XEX(28)=100.00D+0
      XEX(29)=125.00D+0
      XEX(30)=600.00D+0
      RETURN
    2 FX=0.D+0
      DO 70 I=1,5
      SUM=0.D+0
      DO 71 J=1,3
      SUM1=0.D+0
      DO 72 K=1,I
   72 SUM1=SUM1+X(12+J+3*K)-X(J-3+3*K)
   71 SUM=SUM+X(3*(I-1)+J)*(R1(J,I)-KA(J,I))-X(3*(I-1)+J)**2*R2(J,I)
     F    -X(12+3*I+J)*(K1(J,I)+KP(J,I))-(X(12+3*I+J)-X(J+3*I-3))**2
     F    *(K3(J,I)+KL1(J,I))-KL2(J,I)*SUM1
   70 FX=FX-SUM
    3 RETURN
    4 DO 8 I=1,5
      DO 8 J=1,3
      L=3*(I-1)+J
    8 IF(INDEX1(L)) G(L)=H(J,I)-X(L)
      DO 9 I=1,5
      DO 9 J=1,3
      L=3*(I-1)+J+15
      IF (.NOT.INDEX1(L)) GOTO 9
      G(L)=B(J,I)
      DO 10 K=1,3
   10 G(L)=G(L)-T(J,K)*X(12+3*I+K)
    9 CONTINUE
      DO 11 I=1,5
      DO 11 J=1,3
      L=3*(I-1)+J+30
      IF (.NOT. INDEX1(L)) GOTO 11
      G(L)=0.D+0
      DO 12 K=1,I
   12 G(L)=G(L)+X(12+3*K+J)-X(J-3+3*K)
   11 CONTINUE
    5 RETURN
      END           
