      SUBROUTINE TP373(MODE)
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
      DOUBLEPRECISION F,DF
      INTEGER I,J,LSUM
      GOTO (1,2,3,4,5), MODE
    1 NILI=0
      NINL=0
      NELI=0
      NENL=6
      N=9 
      DO 11 I=1,9
      LXL(I)=.FALSE.
   11 LXU(I)=.FALSE.
      LXL(3)=.TRUE.
      LXU(3)=.TRUE.
      XL(3)=-1.0
      XU(3)=0.0
      X(1)=3.D+2
      X(2)=-1.D+2
      X(3)=-.1997D+0
      X(4)=-127.D+0
      X(5)=-151.D+0
      X(6)=379.D+0
      X(7)=421.D+0
      X(8)=460.D+0
      X(9)=426.D+0
      GF(1)=0.D+0
      GF(2)=0.D+0
      GF(3)=0.D+0
      DO 17 I=1,6
      DO 17 J=4,9
   17 GG(I,J)=0.D+0
      DO 18 I=1,6
      GG(I,1)=1.D+0
   18 GG(I,I+3)=1.D+0
      LEX=.TRUE.
      NEX=1
      FEX=0.13390093D+5
      XEX(1)=0.52330542D+3
      XEX(2)=-0.15694770D+3
      XEX(3)=-0.19966472D+0
      XEX(4)=0.29608061D+2 
      XEX(5)=-0.86615571D+2
      XEX(6)=0.47326669D+2
      XEX(7)=0.26235575D+2
      XEX(8)=0.22915982D+2
      XEX(9)=-0.39470718D+2
      LSUM=6
      RETURN
    2 FX=0.D+0
      DO 21 I=4,9
      F(I-3)=X(I)
   21 FX=FX+X(I)**2 
      RETURN
    3 DO 35 I=4,9
   35 F(I-3)=X(I)
      DO 31 I=4,9
   31 GF(I)=2.D+0*X(I)
      DO 33 I=1,6
      DO 33 J=1,9
   33 DF(I,J)=0.D+0
      DO 34 I=1,6
   34 DF(I,I+3)=1.D+0
      RETURN
    4 IF (INDEX1(1)) G(1)=X(1)+X(2)*DEXP(-5.D+0*X(3))+X(4)-127.D+0
      IF (INDEX1(2)) G(2)=X(1)+X(2)*DEXP(-3.D+0*X(3))+X(5)-151.D+0
      IF (INDEX1(3)) G(3)=X(1)+X(2)*DEXP(-X(3))+X(6)-379.D+0
      IF (INDEX1(4)) G(4)=X(1)+X(2)*DEXP(X(3))+X(7)-421.D+0
      IF (INDEX1(5)) G(5)=X(1)+X(2)*DEXP(3.D+0*X(3))+X(8)-460.D+0
      IF (INDEX1(6)) G(6)=X(1)+X(2)*DEXP(5.D+0*X(3))+X(9)-426.D+0
      RETURN
    5 DO 52 I=1,6
      IF (.NOT.INDEX2(I)) GOTO 52
      GG(I,2)=DEXP(DBLE(I*2-7)*X(3))
      GG(I,3)=X(2)*DBLE(I*2-7)*GG(I,2)
   52 CONTINUE
      RETURN 
      END
