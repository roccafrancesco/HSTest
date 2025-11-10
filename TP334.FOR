      SUBROUTINE TP334(MODE)
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
      INTEGER I,LSUM
      DOUBLEPRECISION F,DF,Y(15),UI,VI,WI
      DATA(Y(I),I=1,15)/0.14D+0,0.18D+0,0.22D+0,0.25D+0,0.29D+0,
     F          0.32D+0,0.35D+0,0.39D+0,0.37D+0,0.58D+0,0.73D+0,
     F          0.96D+0,0.134D+1,0.21D+1,0.439D+1/
      GOTO(1,2,2,4,4),MODE
 1    N=3
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      LSUM=15
      DO 6  I=1,3
      X(I)=0.1D+1
      LXL(I)=.FALSE.
 6    LXU(I)=.FALSE.
      LEX=.FALSE.
      NEX=1
      XEX(1)=0.82410544D-1
      XEX(2)=0.11330358D+1
      XEX(3)=0.23436955D+1
C      FEX=0.82149184D-2
      FEX=0.82148773D-2 
      RETURN
 2    DO 7  I=1,15
      UI=DBLE(I)
      VI=DBLE(16-I)
      WI=DMIN1(UI,VI)
 7    F(I)=Y(I)-(X(1)+I/(X(2)*VI+X(3)*WI))
      IF (MODE .EQ. 3) GOTO 3
      FX=0.D+0
      DO 10 I=1,15
 10   FX=FX+F(I)**2
      RETURN
 3    DO 8  I=1,3
 8    GF(I)=0.0D+0
      DO 9  I=1,15
      UI=DBLE(I)
      VI=DBLE(16-I)
      WI=DMIN1(UI,VI)
      DF(I,1)=-0.1D+1
      DF(I,2)=(UI*VI)/(X(2)*VI+X(3)*WI)**2
      DF(I,3)=(UI*WI)/(X(2)*VI+X(3)*WI)**2
      GF(1)=GF(1)+DF(I,1)*F(I)*.2D+1
      GF(2)=GF(2)+DF(I,2)*F(I)*.2D+1
 9    GF(3)=GF(3)+DF(I,3)*F(I)*.2D+1
 4    RETURN
      END             
