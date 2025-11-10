      SUBROUTINE TP243(MODE)
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
      DOUBLEPRECISION XBX,DXBX(3),A(4),B(3,3),D(4),E(4,3),F,DF
      DATA A,D,B /0.14272D+0,-0.184981D+0,-0.521869D+0,-0.685306D+0,
     F             1.75168D+0,-1.35195D+0,-0.479048D+0,-0.3648D+0,
     F             2.95137D+0,4.87407D+0,-2.0506D+0,
     F             4.87407D+0,9.39321D+0,-3.93185D+0,
     F             -2.0506D+0,-3.93189D+0,2.64745D+0 /
      INTEGER I
      E(1,1)=-0.564255D+0
      E(1,2)=0.392417D+0
      E(1,3)=-0.404979D+0
      E(2,1)=0.927589D+0
      E(2,2)=-0.0735083D+0
      E(2,3)=0.535493D+0
      E(3,1)=0.658799D+0
      E(3,2)=-0.636666D+0
      E(3,3)=-0.681091D+0
      E(4,1)=-0.869487D+0
      E(4,2)=0.586387D+0
      E(4,3)=0.289826D+0
      GOTO (1,2,2,4,4),MODE
    1 N=3
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,3
      LXL(I)=.FALSE.
      LXU(I)=.FALSE.
      X(I)=0.1D+0
    6 XEX(I)=0.D+0
      LEX=.TRUE.
      NEX=1
      FEX=0.79657853D+00
      RETURN
    2 FX=0.D+0
      XBX=(X(1)*B(1,1)+X(2)*B(2,1)+X(3)*B(3,1))*X(1)+
     /    (X(1)*B(1,2)+X(2)*B(2,2)+X(3)*B(3,2))*X(2)+
     /    (X(1)*B(1,3)+X(2)*B(2,3)+X(3)*B(3,3))*X(3)
      DO 7 I=1,4
    7 F(I)=A(I)+E(I,1)*X(1)+E(I,2)*X(2)+E(I,3)*X(3)+
     /     0.5D+0*XBX*D(I)
      IF (MODE.EQ.3) GOTO 3
      DO 10 I=1,4
   10 FX=FX+F(I)**2
      RETURN
    3 DXBX(1)=(X(1)*B(1,1)+X(2)*B(2,1)+X(3)*B(3,1))*2.D+0
      DXBX(2)=(X(1)*B(1,2)+X(2)*B(2,2)+X(3)*B(3,2))*2.D+0
      DXBX(3)=(X(1)*B(1,3)+X(2)*B(2,3)+X(3)*B(3,3))*2.D+0
      DO 9 I=1,3
    9 GF(I)=0.D+0
      DO 8 I=1,4
      DF(I,1)=E(I,1)+DXBX(1)*D(I)*0.5D+0
      DF(I,2)=E(I,2)+DXBX(2)*D(I)*0.5D+0
      DF(I,3)=E(I,3)+DXBX(3)*D(I)*0.5D+0
      GF(1)=GF(1)+F(I)*DF(I,1)*2.D+0
      GF(2)=GF(2)+F(I)*DF(I,2)*2.D+0
    8 GF(3)=GF(3)+F(I)*DF(I,3)*2.D+0
    4 RETURN
      END
