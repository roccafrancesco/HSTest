      SUBROUTINE TP86(MODE)     
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
      INTEGER I,J,I1
      COMMON     /D86/ E,D,B,C,A,T,T1 
      DIMENSION E(5),D(5),B(10),C(5,5),A(10,5)  
      DOUBLEPRECISION E,D,B,C,A,T,T1     
      GOTO (1,2,3,4,5),MODE     
1     N=5       
      NILI=10   
      NINL=0    
      NELI=0    
      NENL=0    
      DO 26 I=1,4       
26    X(I)=0.D0 
      X(5)=1.D0 
      DO 6 I=1,5
      LXL(I)=.TRUE.     
      LXU(I)=.FALSE.    
6     XL(I)=0.D0
      E(1)=-15.D0       
      E(2)=-27.D0       
      E(3)=-36.D0       
      E(4)=-18.D0       
      E(5)=-12.D0       
      C(1,1)=30.D0      
      C(1,2)=-20.D0     
      C(1,3)=-10.D0     
      C(1,4)=32.D0      
      C(1,5)=-10.D0     
      C(2,2)=39.D0      
      C(2,3)=-6.D0      
      C(2,4)=-31.D0     
      C(2,5)=32.D0      
      C(3,3)=10.D0      
      C(3,4)=-6.D0      
      C(3,5)=-10.D0     
      C(4,4)=39.D0      
      C(4,5)=-20.D0     
      C(5,5)=30.D0      
      DO 27 I=1,4       
      I1=I+1    
      DO 27 J=I1,5      
27    C(J,I)=C(I,J)     
      D(1)=4.D0 
      D(2)=8.D0 
      D(3)=10.D0
      D(4)=6.D0 
      D(5)=2.D0 
      A(1,1)=-16.D0     
      A(1,2)=2.D0       
      A(1,3)=0.D0       
      A(1,4)=1.D0       
      A(1,5)=0.D0       
      A(2,1)=0.D0       
      A(2,2)=-2.D0      
      A(2,3)=0.D0       
      A(2,4)=0.4D0      
      A(2,5)=2.D0       
      A(3,1)=-3.5D0     
      A(3,2)=0.D0       
      A(3,3)=2.D0       
      A(3,4)=0.D0       
      A(3,5)=0.D0       
      A(4,1)=0.D0       
      A(4,2)=-2.D0      
      A(4,3)=0.D0       
      A(4,4)=-4.D0      
      A(4,5)=-1.D0      
      A(5,1)=0.D0       
      A(5,2)=-9.D0      
      A(5,3)=-2.D0      
      A(5,4)=1.D0       
      A(5,5)=-2.8D0     
      A(6,1)=2.D0       
      A(6,2)=0.D0       
      A(6,3)=-4.D0      
      A(6,4)=0.D0       
      A(6,5)=0.D0       
      A(8,1)=-1.D0      
      A(8,2)=-2.D0      
      A(8,3)=-3.D0      
      A(8,4)=-2.D0      
      A(8,5)=-1.D0      
      DO 29 I=1,5       
      A(7,I)=-1.D0      
      A(9,I)=I  
29    A(10,I)=1.D0      
      B(1)=-40.D0       
      B(2)=-2.D0
      B(3)=-0.25D0      
      B(4)=-4.D0
      B(5)=-4.D0
      B(6)=-1.D0
      B(7)=-40.D0       
      B(8)=-60.D0       
      B(9)=5.D0 
      B(10)=1.D0
      DO 25 I=1,10      
      DO 25 J=1,5       
25    GG(I,J)=A(I,J)    
      LEX=.FALSE.
      NEX=1       
      XEX(1)=0.299999999948D+00     
      XEX(2)=0.333467606492D+00     
      XEX(3)=0.400000000107D+00     
      XEX(4)=0.428310104740D+00     
      XEX(5)=0.223964873676D+00     
      FEX=-0.323486789716D+02 
      RETURN    
2     T=0.D0    
      DO 20 I=1,5       
      T1=0.D0   
      DO 21 J=1,5       
21    T1=T1+C(J,I)*X(I)*X(J)    
20    T=T+E(I)*X(I)+D(I)*X(I)**3+T1     
      FX=T      
      RETURN    
3     DO 23 I=1,5       
      T=0.D0    
      DO 22 J=1,5       
22    T=T+(C(I,J)+C(J,I))*X(J)  
23    GF(I)=E(I)+T+3.D0*D(I)*X(I)**2    
      RETURN    
4     DO 24 I=1,10      
24    IF (INDEX1(I)) G(I)=A(I,1)*X(1)+A(I,2)*X(2)+A(I,3 
     -)*X(3)+A(I,4)*    
     -X(4)+A(I,5)*X(5)-B(I)     
5     RETURN    
      END       
