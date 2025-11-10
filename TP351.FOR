      SUBROUTINE TP351(MODE)
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
      DOUBLEPRECISION XH1,XH2,XH3,XH4,XH5,F,DF,A(7),B(7)
      DATA(A(I),I=1,7)/0.0D+0,0.428D-3,0.1D-2,0.161D-2,0.209D-2,
     F                 0.348D-2,0.525D-2/
     F    (B(I),I=1,7)/0.7391D+1,0.1118D+2,0.1644D+2,0.162D+2,
     F                 0.222D+2,0.2402D+2,0.3132D+2/
      XH1=X(1)**2
      XH2=X(2)**2
      XH3=X(3)**2
      XH4=X(4)**2
      GOTO (1,2,2,4,4),MODE
    1 N=4
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      X(1)=0.27D+1
      X(2)=0.9D+2
      X(3)=0.15D+4
      X(4)=0.1D+2
      DO 6 I=1,4
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.FALSE.
      NEX=1
C      FEX=0.31858082D+3
      FEX=0.31857175D+3  
      XEX(1)=0.27143661D+1
      XEX(2)=0.14043580D+3
      XEX(3)=0.17075155D+4
      XEX(4)=0.31512867D+2
      LSUM=7
      RETURN
    2 DO 20 I=1,7
   20 F(I)=(((XH1+A(I)*XH2+A(I)*A(I)*XH3)/(0.1D+1+A(I)*XH4))
     F      -B(I))/B(I)*1.D+2
      IF (MODE.EQ.3) GOTO 3
      FX=0.0D+0
      DO 7 I=1,7
    7 FX=FX+F(I)**2
      RETURN
    3 DO 8 J=1,4
    8 GF(J)=0.0D+0
      DO 10 I=1,7
      XH5=(0.1D+1+XH4*A(I))*B(I)
      DF(I,1)=0.2D+3*X(1)/XH5
      DF(I,2)=0.2D+3*X(2)*A(I)/XH5
      DF(I,3)=0.2D+3*X(3)*A(I)**2/XH5
      DF(I,4)=-0.2D+3*X(4)*A(I)*B(I)*(XH1+XH2*A(I)+XH3*A(I)**2)/XH5**2
      DO 10 J=1,4    
   10 GF(J)=GF(J)+2.D+0*F(I)*DF(I,J)
    4 RETURN
      END
