      SUBROUTINE TP350(MODE)
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
      DOUBLEPRECISION  F,DF,Y(11),U(11),H(11)
      DATA (Y(I),I=1,11)/0.1957D+0,0.1947D+0,0.1735D+0,0.1600D+0,
     F                   0.844D-1,0.627D-1,0.456D-1,0.342D-1,
     F                   0.323D-1,0.235D-1,0.246D-1/
     F     (U(I),I=1,11)/0.4D+1,0.2D+1,0.1D+1,0.5D+0,0.25D+0,0.167D+0,
     F                   0.125D+0,0.1D+0,0.833D-1,0.714D-1,0.625D-1/
      DO 12 I=1,11
   12 H(I)=U(I)**2+X(3)*U(I)+X(4)
      GOTO (1,2,2,4,4),MODE
    1 N=4
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      X(1)=0.25D+0
      X(2)=0.39D+0
      X(3)=0.415D+0
      X(4)=0.39D+0
      DO 6 I=1,4
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LSUM=11
      LEX=.FALSE.
      NEX=1
      FEX=0.30750560D-3
      XEX(1)=0.19280644D+0
      XEX(2)=0.19126279D+0
      XEX(3)=0.12305098D+0
      XEX(4)=0.13605235D+0
      RETURN
    2 DO 20 I=1,11
   20 F(I)=Y(I)-X(1)/H(I)*(U(I)**2+X(2)*U(I))
      IF (MODE.EQ.3) GOTO 3
      FX=0.0D+0
      DO 7 I=1,11
    7 FX=FX+F(I)**2
      RETURN
    3 DO 8 I=1,11
      DF(I,1)=(-U(I)**2-X(2)*U(I))/H(I)
      DF(I,2)=(-X(1)*U(I))/H(I)
      DF(I,3)=X(1)*U(I)*(U(I)**2+X(2)*U(I))/H(I)**2
    8 DF(I,4)=X(1)*(U(I)**2+X(2)*U(I))/H(I)**2
      DO 9 J=1,4 
      GF(J)=0.0D+0
      DO 10 I=1,11
   10 GF(J)=GF(J)+2.D+0*F(I)*DF(I,J)
    9 CONTINUE
    4 RETURN
      END
