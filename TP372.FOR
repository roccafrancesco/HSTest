      SUBROUTINE TP372(MODE)
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
      GOTO (1,2,3,4,5),MODE
    1 N=9 
      NILI=0
      NINL=12
      NELI=0
      NENL=0
      X(1)=3.D+2
      X(2)=-1.D+2
      X(3)=-.1997D+0
      X(4)=+127.D+0
      X(5)=+151.D+0
      X(6)=379.D+0
      X(7)=421.D+0
      X(8)=460.D+0
      X(9)=426.D+0
      DO 12 I=1,12
      DO 12 J=4,9
   12 GG(I,J)=0.D+0
      DO 18 I=1,6
      GG(I,1)=1.D+0
   18 GG(I,I+3)=1.D+0
      DO 13 I=7,12
      GG(I,1)=-1.D+0
   13 GG(I,I-3)=1.D+0
      GF(1)=0.D+0
      GF(2)=0.D+0
      GF(3)=0.D+0
      DO 14 I=4,9
      LXL(I)=.TRUE.
      LXU(I)=.FALSE.
   14 XL(I)=0.D+0
      DO 15 I=1,2
      LXL(I)=.FALSE.
   15 LXU(I)=.FALSE.
      XL(3)=-1.0
      XU(3)=0.0
      LXL(3)=.TRUE.
      LXU(3)=.TRUE.
      XEX(1)=0.52330555D+3
      XEX(2)=-0.15694787D+3
      XEX(3)=-0.19966457D+0
      XEX(4)=0.29608067D+2
      XEX(5)=0.86615521D+2
      XEX(6)=0.47326718D+2
      XEX(7)=0.26235604D+2
      XEX(8)=0.22915985D+2
      XEX(9)=0.39470742D+2
      LEX=.FALSE.
      NEX=1
      FEX=0.13390093D+5
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
      IF (INDEX1(7)) G(7)=-X(1)-X(2)*DEXP(-5.D+0*X(3))+X(4)+127.D+0
      IF (INDEX1(8)) G(8)=-X(1)-X(2)*DEXP(-3.D+0*X(3))+X(5)+151.D+0
      IF (INDEX1(9)) G(9)=-X(1)-X(2)*DEXP(-X(3))+X(6)+379.D+0
      IF (INDEX1(10)) G(10)=-X(1)-X(2)*DEXP(X(3))+X(7)+421.D+0
      IF (INDEX1(11)) G(11)=-X(1)-X(2)*DEXP(3.D+0*X(3))+X(8)+460.D+0
      IF (INDEX1(12)) G(12)=-X(1)-X(2)*DEXP(5.D+0*X(3))+X(9)+426.D+0
      RETURN
    5 DO 51 I=1,6
      IF (.NOT.INDEX2(I+6)) GOTO 51
      GG(I+6,2)=-DEXP(DBLE(I*2-7)*X(3))
      GG(I+6,3)=-X(2)*DBLE(I*2-7)*GG(I+6,2)
   51 CONTINUE
      DO 52 I=1,6
      IF (.NOT.INDEX2(I)) GOTO 52
      GG(I,2)=DEXP(DBLE(I*2-7)*X(3))
      GG(I,3)=X(2)*DBLE(I*2-7)*GG(I,2)
   52 CONTINUE
      RETURN 
      END
