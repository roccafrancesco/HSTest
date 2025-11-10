      SUBROUTINE TP367(MODE)
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
     /           /L20/ LEX,NEX,FEX,XEX(NMAX) 
      INTEGER I
      GOTO (1,2,3,4,5), MODE
    1 N=7
      NILI=2
      NINL=1
      NELI=1
      NENL=1
      DO 11 I=1,7
      X(I)=.1D+0
      LXL(I)=.TRUE.
      LXU(I)=.FALSE.
   11 XL(I)=0.D+0
      LEX=.FALSE.
      NEX=1
      FEX=-0.37412960D+2
      XEX(1)=0.14688103D+1
      XEX(2)=0.19839711D+1
      XEX(3)=0.35187754D+0
      XEX(4)=0.11953411D+1
      XEX(5)=0.56940029D+0
      XEX(6)=0.78474478D+0
      XEX(7)=0.14121216D+1
      DO 12 I=1,7
   12 GG(1,I)=-1.D+0
      DO 13 I=1,4
   13 GG(2,I)=-1.D+0
      DO 14 I=5,7
   14 GG(2,I)=0.D+0
      GG(3,1)=-1.D+0
      GG(3,2)=0.D+0
      GG(3,3)=-1.D+0
      GG(3,4)=0.D+0
      GG(3,5)=-1.D+0
      GG(4,1)=0.D+0
      GG(4,2)=0.D+0
      GG(4,3)=0.D+0
      GG(4,4)=2.D+0
      GG(4,5)=1.D+0
      GG(4,6)=.8D+0
      GG(4,7)=1.D+0
      GG(5,1)=0.D+0
      GG(5,4)=0.D+0
      GG(5,7)=0.D+0
      GF(2)=-5.D+0
      GF(4)=-6.D+0
      RETURN
    2 FX=-5.D+0*X(1)-5.D+0*X(2)-4.D+0*X(3)-X(1)*X(3)-6.D+0*X(4)
     F -5.D+0*X(5)/(1.D+0+X(5))-8.D+0*X(6)/(1.D+0+X(6))
     F -10.D+0*(1.D+0-2.D+0*DEXP(-X(7))+DEXP(-2.D+0*X(7)))
      RETURN
    3 GF(1)=-5.D+0-X(3)
      GF(3)=-4.D+0-X(1)
      GF(5)=-5.D+0/(1.D+0+X(5))**2
      GF(6)=-8.D+0/(1.D+0+X(6))**2
      GF(7)=-20.D+0*(DEXP(-X(7))-DEXP(-2.D+0*X(7)))
      RETURN
    4 IF (INDEX1(1)) G(1)=10.D+0-X(1)-X(2)-X(3)-X(4)-X(5)-X(6)-X(7)
      IF (INDEX1(2)) G(2)=5.D+0-X(1)-X(2)-X(3)-X(4)
      IF (INDEX1(3)) G(3)=5.D+0-X(1)-X(3)-X(5)-X(6)**2-X(7)**2
      IF (INDEX1(4)) G(4)=2.D+0*X(4)+X(5)+.8D+0*X(6)+X(7)-5.D+0
      IF (INDEX1(5)) G(5)=X(2)**2+X(3)**2+X(5)**2+X(6)**2-5.D+0
      RETURN
    5 IF (.NOT.INDEX2(3)) GOTO 51
      GG(3,6)=-2.D+0*X(6)
      GG(3,7)=-2.D+0*X(7)
   51 IF (.NOT.INDEX2(5)) GOTO 52
      GG(5,2)=2.D+0*X(2)
      GG(5,3)=2.D+0*X(3)
      GG(5,5)=2.D+0*X(5)
      GG(5,6)=2.D+0*X(6)
   52 RETURN 
      END
