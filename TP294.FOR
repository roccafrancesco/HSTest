      SUBROUTINE TP294(MODE)
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
      INTEGER I,J,K
      N=6
      GOTO 20
      ENTRY TP295(MODE)
      N=10
      GOTO 20
      ENTRY TP296(MODE)
      N=16
      GOTO 20
      ENTRY TP297(MODE)
      N=30
      GOTO 20
      ENTRY TP298(MODE)
      N=50
      GOTO 20
      ENTRY TP299(MODE)
      N=100
   20 K=N-1
      GOTO(1,3,3,4,4),MODE
    1 NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,N
      X(I)=-.12D+1
      XEX(I)=.1D+1
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      DO 7 I=1,(N/2)
    7 X(2*I)=.1D+1  
      FEX=.0D+0
      LEX=.TRUE.
      NEX=1
      RETURN
    2 FX=.0D+0
      DO 8 I=1,K
    8 FX=FX+F(I)**2+F(I+K)**2
      FX=FX*1.0D-4
      RETURN
    3 DO 12 I=1,K  
      F(I)=.1D+2*(X(I+1)-X(I)**2)
   12 F(I+K)=.1D+1-X(I)
      IF (MODE.EQ.2) GOTO 2
      DO 9 I=1,K
      DO 9 J=1,N
      DF(I,J)=.0D+0
      IF (J.EQ.I) DF(I,J)=-.2D+2*X(I)
      IF (J.EQ.(I+1)) DF(I,J)=.1D+2
      DF(I+K,J)=.0D+0
    9 IF (J.EQ.I) DF(I+K,J)=-.1D+1
      DO 11 J=1,N
      GF(J)=.0D+0
      DO 13 I=1,2*K
   13 GF(J)=GF(J)+.2D+1*F(I)*DF(I,J)
      GF(J)=GF(J)*1.0D-4
   11 CONTINUE
    4 RETURN
      END
