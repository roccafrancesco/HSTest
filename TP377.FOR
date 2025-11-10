      SUBROUTINE TP377(MODE)
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
      DOUBLEPRECISION A(10),SUM
      DATA A/-0.6089D+1,-0.17164D+2,-0.34054D+2,-0.5914D+1,
     1 -0.24721D+2,-0.14986D+2,-0.24100D+2,-0.10708D+2,
     1 -0.26662D+2,-0.22179D+2/	
      GOTO (1,2,3,4,5),MODE
    1 N=10
      NILI=0
      NINL=0
      NELI=3
      NENL=0
      DO 6 I=1,10
      X(I)=0.1D+0
      LXU(I)=.TRUE.
      LXL(I)=.TRUE.
      XL(I)=0.1D-3
    6 XU(I)=0.1D+2
      GG(1,1)=0.1D+1
      GG(1,2)=-0.2D+1
      GG(1,3)=0.2D+1
      GG(1,4)=0.D+0
      GG(1,5)=0.D+0
      GG(1,6)=0.1D+1
      DO 7 I=7,9
    7 GG(1,I)=0.D+0
      GG(1,10)=0.1D+1
      DO 8 I=1,3
    8 GG(2,I)=0.D+0
      GG(2,4)=0.1D+1
      GG(2,5)=-0.2D+1
      GG(2,6)=0.1D+1
      GG(2,7)=0.1D+1
      DO 9 I=8,10
    9 GG(2,I)=0.D+0
      GG(3,1)=0.D+0
      GG(3,2)=0.D+1
      GG(3,3)=0.1D+1
      DO 10 I=4,6
   10 GG(3,I)=0.D+0
      GG(3,7)=0.1D+1
      GG(3,8)=0.1D+1
      GG(3,9)=0.2D+1
      GG(3,10)=0.1D+1
      LEX=.FALSE.
      NEX=1
      FEX=-795.001
      XEX(1)=10.0
      XEX(2)=10.0
      XEX(3)=1.0
      XEX(4)=10.0      	        			  	
      XEX(5)=9.5
      XEX(6)=10.0
      XEX(7)=0.1D-3
      XEX(8)=0.1D-3
      XEX(9)=0.1D-3
      XEX(10)=0.1D-3
      RETURN
    2 FX=0.D+0
      SUM=0.D+0
      DO 11 I=1,10
   11 SUM=SUM+X(I)
      DO 12 I=1,10
   12 FX=FX+X(I)*(A(I)+DLOG(DMAX1(X(I)/SUM,1.0D-5)))
    3 RETURN
      SUM=0.D+0
      DO 45 I=1,10
   45 SUM=SUM+X(I)
      DO 46 I=1,10
   46 GF(I)=A(I)+DLOG(X(I)/SUM)
      RETURN
    4 IF(INDEX1(1)) G(1)=X(1)-2.D+0*X(2)+2.D+0*X(3)+X(6)+X(10)-2.D+0
      IF(INDEX1(2)) G(2)=X(4)-2.D+0*X(5)+X(6)+X(7)-1.D+0
      IF(INDEX1(3)) G(3)=X(3)+X(7)+X(8)+2.D+0*X(9)+X(10)-1.D+0
    5 RETURN
      END
