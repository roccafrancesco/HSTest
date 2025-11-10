      SUBROUTINE TP381(MODE)
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
      DOUBLEPRECISION S(13),U(13),V(13),R(13)
      DATA R/0.8D+0,0.11D+1,0.85D+0,0.345D+1,0.2D+1,0.21D+1,
     1 0.3D+1,0.8D+0,0.45D+0,0.72D+0,0.18D+1,0.3D+1,0.6D+0/
      DATA S/0.116D+2,0.137D+2,0.95D+1,0.485D+2,0.319D+2,0.511D+2,
     1 0.655D+2,0.0D+0,0.D+0,0.0D+0,0.218D+2,0.469D+2,0.D+0/
      DATA U/0.5D-1,0.7D-1,0.D+0,0.33D+0,0.0D+0,0.127D+1,0.127D+1,
     1 0.2335D+2,0.3584D+2,0.81D+0,0.179D+1,0.734D+1,0.0D+0/
      DATA V/0.35D+0,0.37D+0,0.1D+0,0.62D+0,0.0D+0,0.103D+1,
     1 0.169D+1,0.1821D+2,0.1D-1,0.8D-1,0.31D+0,0.159D+1,0.2245D+2/	
      GOTO(1,2,3,4,5),MODE
    1 N=13
      NILI=3
      NINL=0
      NELI=1
      NENL=0
      DO 6 I=1,13
      X(I)=0.1D+0
      LXU(I)=.FALSE.
      LXL(I)=.TRUE.
    6 XL(I)=0.0D+0  	
      DO 7 I=1,13
      GG(1,I)=S(I)
      GG(2,I)=U(I)
    7 GG(4,I)=0.1D+1
      DO 8 I=1,13
    8 GG(3,I)=V(I)
      LEX=.FALSE.
      NEX=1
      FEX=1.0149D0
      XEX(1)=0.7864989D+0
      XEX(2)=0.0D+0
      XEX(3)=0.0D+0
      XEX(4)=0.0D+0
      XEX(5)=0.0D+0
      XEX(6)=0.17105694D+0
      XEX(7)=0.0D+0
      XEX(8)=0.0D+0
      XEX(9)=0.20676337D-1
      XEX(10)=0.0D+0
      XEX(11)=0.0D+0
      XEX(12)=0.0D+0
      XEX(13)=0.19883712D-1
      DO 11 I=1,13
   11 GF(I)=R(I)
      RETURN
    2 FX=0.0D+0
      DO 10 I=1,13
   10 FX=FX+R(I)*X(I)
    3 RETURN
    4 IF(.NOT.INDEX1(1)) GOTO 12
      G(1)=0.0D+0
      DO 13 I=1,13
   13 G(1)=G(1)+S(I)*X(I)
      G(1)=G(1)-0.18D+2
   12 IF(.NOT.INDEX1(2)) GOTO 14
      G(2)=0.0D+0
      DO 15 I=1,13
   15 G(2)=G(2)+U(I)*X(I)
      G(2)=G(2)-0.1D+1
   14 IF(.NOT.INDEX1(3)) GOTO 16
      G(3)=0.0D+0
      DO 17 I=1,13
   17 G(3)=G(3)+V(I)*X(I)
      G(3)=G(3)-0.9D+0
   16 IF(.NOT.INDEX1(4)) GOTO 5
      G(4)=0.0D+0
      DO 19 I=1,13
   19 G(4)=G(4)+X(I)
      G(4)=G(4)-0.1D+1
    5 RETURN
      END   
