      SUBROUTINE TP352(MODE)
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
      DOUBLEPRECISION F,DF,TI
      GOTO (1,2,2,4,4),MODE
    1 N=4
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      X(1)=0.25D+2
      X(2)=0.5D+1
      X(3)=-0.5D+1
      X(4)=-0.1D+1
      DO 6 I=1,4
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.FALSE.
      NEX=1
      FEX=0.90323433D+3
      XEX(1)=-0.10223574D+2
      XEX(2)=0.11908429D+2
      XEX(3)=-0.45804134D+0
      XEX(4)=0.58031996D+0
      LSUM=40
      RETURN
    2 DO 20 I=1,20
      TI=I*0.2D+0
      F(I)=X(1)+X(2)*TI-DEXP(TI)
   20 F(I+20)=X(3)+X(4)*DSIN(TI)-DCOS(TI)
      IF (MODE.EQ.3) GOTO 3
      FX=0.0D+0
      DO 7 I=1,20
    7 FX=FX+F(I)**2+F(I+20)**2
      RETURN
    3 DO 8 J=1,4
    8 GF(J)=0.0D+0
      DO 10 I=1,20
      TI=I*0.2D+0
      DF(I,1)=0.1D+1
      DF(I+20,1)=0.0D+0
      DF(I,2)=TI
      DF(I+20,2)=0.0D+0
      DF(I,3)=0.0D+0
      DF(I+20,3)=0.1D+1
      DF(I,4)=0.0D+0
      DF(I+20,4)=DSIN(TI)
      DO 10 J=1,4
  10  GF(J)=GF(J)+2.D+0*F(I)*DF(I,J)+2.D+0*F(I+20)*DF(I+20,J)
    4 RETURN
      END
