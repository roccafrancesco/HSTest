      SUBROUTINE TP57(MODE)     
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
      INTEGER I,J
      COMMON     /D57/ S,A,B
      DIMENSION S(2),A(44),B(44) 
      DOUBLEPRECISION F,DF,S,A,B,T,V1       
      IF (MODE - 2) 1,18,18     
1     N=2       
      NILI=0    
      NINL=1    
      NELI=0    
      NENL=0    
      X(1)=0.42D0       
      X(2)=5.D0 
      DO 6 I=1,2
      LXL(I)=.TRUE.     
6     LXU(I)=.FALSE.    
      XL(1)=0.4D0       
      XL(2)=-4.D0       
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.419952674511D+00     
      XEX(2)=0.128484562930D+01     
      FEX=0.284596697213D-01
      RETURN    
   18 CONTINUE
      DO 20 I=1,2       
      A(I)=8.D0 
      A(16+I)=18.D0     
      A(30+I)=28.D0     
      A(35+I)=32.D0     
      A(38+I)=36.D0     
      A(40+I)=38.D0     
      B(I)=0.49D0       
      B(6+I)=0.46D0     
      B(11+I)=0.43D0    
      B(14+I)=0.43D0    
      B(18+I)=0.42D0    
      B(21+I)=0.41D0    
      B(25+I)=0.4D0     
      B(29+I)=0.41D0    
      B(36+I)=0.4D0     
      B(40+I)=0.4D0     
  20  B(42+I)=0.39D0    
      DO 21 I=1,3       
      A(10+I)=14.D0     
      A(13+I)=16.D0     
      A(18+I)=20.D0     
      A(21+I)=22.D0     
      A(24+I)=24.D0     
      A(27+I)=26.D0     
      A(32+I)=30.D0     
21    B(31+I)=0.4D0     
      DO  22 I=1,4      
      A(2+I)=10.D0      
22    A(6+I)=12.D0      
      A(38)=34.D0       
      A(43)=40.D0       
      A(44)=42.D0       
      B(3)=0.48D0       
      B(4)=0.47D0       
      B(5)=0.48D0       
      B(6)=0.47D0       
      B(9)=0.45D0       
      B(10)=0.43D0      
      B(11)=0.45D0      
      B(14)=0.44D0      
      B(17)=0.46D0      
      B(18)=0.45D0      
      B(21)=0.43D0      
      B(24)=0.4D0       
      B(25)=0.42D0      
      B(28)=0.41D0      
      B(29)=0.4D0       
      B(35)=0.38D0      
      B(36)=0.41D0      
      B(39)=0.41D0      
      B(40)=0.38D0      
      IF (MODE - 4) 17,4,5      
17    DO 30 I=1,44      
30    F(I)=B(I)-X(1)-(0.49D0-X(1))*DEXP(-X(2)*(A(I)-8.D0))      
      GOTO (1,2,3),MODE 
2     T=0.D0    
      DO 19 I=1,44      
19    T=T+F(I)**2       
      FX=T
      RETURN    
3     S(1)=0.D0 
      S(2)=0.D0 
      DO 31 I=1,44      
      V1=DEXP(-X(2)*(A(I)-8.D0))
      DF(I,1)=-1.D0+V1  
      DF(I,2)=(A(I)-8.D0)*(0.49D0-X(1))*V1      
      DO 32 J=1,2       
32    S(J)=S(J)+2.D0*F(I)*DF(I,J)       
31    CONTINUE  
      GF(1)=S(1)
      GF(2)=S(2)
      RETURN    
4     IF (INDEX1(1)) G(1)=-X(1)*X(2)+0.49D0*X(2)-0.09D0 
      RETURN    
5     IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,1)=-X(2)     
      GG(1,2)=-X(1)+0.49D0      
7     RETURN    
      END       
