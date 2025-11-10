      SUBROUTINE TP375(MODE)	
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
      DOUBLEPRECISION F,DF,TP375A
      INTEGER I,J,LSUM
      GOTO(1,2,2,4,5),MODE
    1 N=10
      NILI=0
      NINL=0
      NELI=8
      NENL=1
      DO 6 I=1,10
      X(I)=0.1D+1
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.FALSE.
      NEX=1
      FEX=-0.15161D+2
      DO 60 I=1,8
   60 XEX(I)=0.1064812D+0
      XEX(9)=0.28438742D+1
      XEX(10)=-0.26424832D+1
      DO 7 J=1,8
      DO 7 I=1,10
    7 GG(J,I)=0.1D+1
      DO 8 I=1,8
    8 GG(I,I)=0.5D+0
      LSUM=10
      DO 17 I=1,10
      DO 17 J=1,10
      DF(I,J)=0.D+0
   17 DF(I,I)=-0.1D+1
      RETURN
    2 DO 16 I=1,10
   16 F(I)=-X(I)
      IF (MODE.EQ.3) GOTO 3
      FX=0.0D+0
      DO 9 I=1,10
    9 FX=FX-X(I)**2
      RETURN
    3 DO 10 I=1,10
   10 GF(I)=-0.2D+1*X(I)
    4 DO 11 J=1,8
      IF(.NOT.INDEX1(J)) GOTO 11
      G(J)=0.0D+0
      DO 12 I=1,10
   12 G(J)=G(J)+X(I)/TP375A(I,J)
      G(J)=G(J)-0.1D+1
   11 CONTINUE
      IF(.NOT.INDEX1(9)) GOTO 18
      G(9)=0.0D+0
      DO 13 I=1,10
   13 G(9)=G(9)+X(I)**2/(0.1D+1+DBLE(I-1)/0.3D+1)
      G(9)=G(9)-0.4D+1
   18 RETURN
    5 IF(.NOT.INDEX2(9)) GOTO 15
      DO 14 I=1,10
   14 GG(9,I)=0.2D+1*X(I)/(0.1D+1+DBLE(I-1)/0.3D+1)
   15 RETURN
      END
