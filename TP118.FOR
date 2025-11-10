      SUBROUTINE TP118(MODE)    
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
      INTEGER I,J,K,M
      DOUBLEPRECISION T  
      GOTO (1,2,3,4,5),MODE     
1     N=15      
      NILI=29   
      NINL=0    
      NELI=0    
      NENL=0    
      DO 6 I=1,15       
6     X(I)=20.D0
      X(2)=55.D0
      X(3)=15.D0
      X(5)=60.D0
      X(8)=60.D0
      X(11)=60.D0       
      X(14)=60.D0       
      XL(1)=8.D0
      XL(2)=43.D0       
      XL(3)=3.D0
      XU(1)=21.D0       
      XU(2)=57.D0       
      XU(3)=16.D0       
      DO 22 I=1,4       
      XL(3*I+1)=0.D0    
      XL(3*I+2)=0.D0    
      XL(3*I+3)=0.D0    
      XU(3*I+1)=90.D0   
      XU(3*I+2)=120.D0  
      XU(3*I+3)=60.D0   
22    CONTINUE  
      DO 25 I=1,15      
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
      DO 25 J=1,29      
25    GG(J,I)=0.D0      
      DO 20 K=1,4       
      DO 20 I=1,3       
      GG(K+4*I-4,3*K+I)=1.D0    
      GG(K+4*I-4,3*K+I-3)=-1.D0 
      GG(K+8+4*I,3*K+I)=-1.D0   
      GG(K+8+4*I,3*K+I-3)=1.D0  
20    CONTINUE  
      DO 21 K=1,5       
      DO 21 I=1,3       
21    GG(24+K,3*K-3+I)=1.D0     
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.800000000000D+01     
      XEX(2)=0.490000000000D+02     
      XEX(3)=0.300000000000D+01     
      XEX(4)=0.100000000000D+01     
      XEX(5)=0.560000000000D+02     
      XEX(6)=0.0D0  
      XEX(7)=0.999999999545D+00     
      XEX(8)=0.630000000000D+02     
      XEX(9)=0.600000000000D+01     
      XEX(10)=0.299999999965D+01     
      XEX(11)=0.700000000000D+02     
      XEX(12)=0.120000000000D+02     
      XEX(13)=0.499999999971D+01     
      XEX(14)=0.770000000000D+02     
      XEX(15)=0.180000000000D+02     
      FEX=0.664820449993D+03 
      RETURN    
2     T=0.D0    
      DO 30 M=1,5       
      I=M-1     
30    T=T+2.3D0*X(3*I+1)+1.D-4*X(3*I+1)**2+1.7D0*X(3*I+2)+1.D-4
     /    *X(3*I+2)**2+2.2D0*X(3*I+3)+1.5D-4*X(3*I+3)**2       
      FX=T      
      RETURN    
3     DO 31 I=1,5       
      GF(3*I-2)=2.3D0+2.D-4*X(3*I-2)    
      GF(3*I-1)=1.7D0+2.D-4*X(3*I-1)    
31    GF(3*I)=2.2D0+3.D-4*X(3*I)
      RETURN    
4     DO 32 I=1,4       
      IF (INDEX1(I)) G(I)=X(3*I+1)-X(3*I-2)+7.D0
      IF (INDEX1(I+4)) G(I+4)=X(3*I+2)-X(3*I-1)+7.D0    
      IF (INDEX1(I+8)) G(I+8)=X(3*I+3)-X(3*I)+7.D0      
      IF (INDEX1(I+12)) G(I+12)=X(3*I-2)-X(3*I+1)+6.D0  
      IF (INDEX1(I+16)) G(I+16)=X(3*I-1)-X(3*I+2)+7.D0  
32    IF (INDEX1(I+20)) G(I+20)=X(3*I)-X(3*I+3)+6.D0    
      IF (INDEX1(25)) G(25)=X(1)+X(2)+X(3)-60.D0
      IF (INDEX1(26)) G(26)=X(4)+X(5)+X(6)-50.D0
      IF (INDEX1(27)) G(27)=X(7)+X(8)+X(9)-70.D0
      IF (INDEX1(28)) G(28)=X(10)+X(11)+X(12)-85.D0     
      IF (INDEX1(29)) G(29)=X(13)+X(14)+X(15)-100.D0    
5     RETURN    
      END       
