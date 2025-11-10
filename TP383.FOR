      SUBROUTINE TP383(MODE)
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
      DIMENSION A(14),B(14),C(14)
      DOUBLEPRECISION A,B,C
      DATA A/0.12842275D+5,0.63425D+3,0.63425D+3,0.634125D+3,
     1 0.1268D+4,0.633875D+3,0.63375D+3,0.1267D+4,0.76005D+3,
     2 0.63325D+3,0.126625D+4,0.632875D+3,0.39446D+3,0.940838D+3/
      DATA B/0.25D+2,0.26D+2,0.26D+2,0.27D+2,0.28D+2,0.29D+2,
     1 0.30D+2,0.32D+2,0.33D+2,0.34D+2,0.35D+2,0.37D+2,
     2 0.38D+2,0.36D+2/
      DATA C/0.547934D+1,0.83234D+0,0.94749D+0,0.111082D+1,
     1 0.264824D+1,0.155868D+1,0.173215D+1,0.390896D+1,
     2 0.274284D+1,0.260541D+1,0.596184D+1,0.329522D+1,
     3 0.183517D+1,0.281372D+1/
      GOTO (1,2,3,4,5),MODE
    1 N=14
      NILI=0
      NINL=0
      NELI=1
      NENL=0
      DO 6 I=1,14
      X(I)=0.1D-1
      LXU(I)=.TRUE.
      LXL(I)=.TRUE.
      XL(I)=0.1D-3
      XU(I)=0.1D+1/B(I)
    6 GG(1,I)=C(I)
      LEX=.FALSE.
      NEX=1
      FEX=0.728566D+6
      XEX(1)=0.4D-1
      XEX(2)=0.382D-1
      XEX(3)=0.358D-1
      XEX(4)=0.33D-1
      XEX(5)=0.303D-1
      XEX(6)=0.279D-1
      XEX(7)=0.265D-1
      XEX(8)=0.249D-1
      XEX(9)=0.23D-1
      XEX(10)=0.216D-1
      XEX(11)=0.202D-1
      XEX(12)=0.192D-1
      XEX(13)=0.203D-1
      XEX(14)=0.253D-1
      RETURN
    2 CONTINUE
      DO I=1,14
      IF (X(I).LT.XL(I)) X(I)=XL(I)
      ENDDO 
      FX=0.D+0
      DO 7 I=1,14
    7 FX=FX+A(I)/X(I)
      RETURN
    3 DO 8 I=1,14
    8 GF(I)=-A(I)/X(I)**2
      RETURN
    4 IF(.NOT.INDEX1(1)) GOTO 5
      G(1)=0.D+0
      DO 10 I=1,14
   10 G(1)=G(1)+(C(I)*X(I))
      G(1)=G(1)-0.1D+1
    5 RETURN
      END       	      	
