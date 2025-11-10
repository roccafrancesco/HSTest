      SUBROUTINE TP119(MODE)    
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
      COMMON     /D119/A(16,16),B(8,16),C(8),S(16),T
      DOUBLEPRECISION  A,B,C,S,T  
      INTEGER I,J
      GOTO (1,2,3,4,5),MODE     
1     N=16      
      NILI=0    
      NINL=0    
      NELI=8    
      NENL=0    
      DO 80 I=1,16      
      DO 21 J=1,16      
21    A(I,J)=0.D0       
      A(I,I)=1.D0       
      DO 81 J=1,8       
81    B(J,I)=0.D0       
80    CONTINUE  
      DO 22 I=1,8       
22    B(I,8+I)=1.D0     
      A(1,4)=1.D0       
      A(1,7)=1.D0       
      A(1,8)=1.D0       
      A(1,16)=1.D0      
      A(2,3)=1.D0       
      A(2,7)=1.D0       
      A(2,10)=1.D0      
      A(3,7)=1.D0       
      A(3,9)=1.D0       
      A(3,10)=1.D0      
      A(3,14)=1.D0      
      A(4,7)=1.D0       
      A(4,11)=1.D0      
      A(4,15)=1.D0      
      A(5,6)=1.D0       
      A(5,10)=1.D0      
      A(5,12)=1.D0      
      A(5,16)=1.D0      
      A(6,8)=1.D0       
      A(6,15)=1.D0      
      A(7,11)=1.D0      
      A(7,13)=1.D0      
      A(8,10)=1.D0      
      A(8,15)=1.D0      
      A(9,12)=1.D0      
      A(9,16)=1.D0      
      A(10,14)=1.D0     
      A(11,13)=1.D0     
      A(12,14)=1.D0     
      A(13,14)=1.D0     
      B(1,1)=0.22D0     
      B(1,2)=0.2D0      
      B(1,3)=0.19D0     
      B(1,4)=0.25D0     
      B(1,5)=0.15D0     
      B(1,6)=0.11D0     
      B(1,7)=0.12D0     
      B(1,8)=0.13D0     
      B(2,1)=-1.46D0    
      B(2,3)=-1.3D0     
      B(2,4)=1.82D0     
      B(2,5)=-1.15D0    
      B(2,7)=0.8D0      
      B(3,1)=1.29D0     
      B(3,2)=-0.89D0    
      B(3,5)=-1.16D0    
      B(3,6)=-0.96D0    
      B(3,8)=-0.49D0    
      B(4,1)=-1.1D0     
      B(4,2)=-1.06D0    
      B(4,3)=0.95D0     
      B(4,4)=-0.54D0    
      B(4,6)=-1.78D0    
      B(4,7)=-0.41D0    
      B(5,4)=-1.43D0    
      B(5,5)=1.51D0     
      B(5,6)=0.59D0     
      B(5,7)=-0.33D0    
      B(5,8)=-0.43D0    
      B(6,2)=-1.72D0    
      B(6,3)=-0.33D0    
      B(6,5)=1.62D0     
      B(6,6)=1.24D0     
      B(6,7)=0.21D0     
      B(6,8)=-0.26D0    
      B(7,1)=1.12D0     
      B(7,4)=0.31D0     
      B(7,7)=1.12D0     
      B(7,9)=-0.36D0    
      B(8,2)=0.45D0     
      B(8,3)=0.26D0     
      B(8,4)=-1.1D0     
      B(8,5)=0.58D0     
      B(8,7)=-1.03D0    
      B(8,8)=0.1D0      
      C(1)=2.5D0
      C(2)=1.1D0
      C(3)=-3.1D0       
      C(4)=-3.5D0       
      C(5)=1.3D0
      C(6)=2.1D0
      C(7)=2.3D0
      C(8)=-1.5D0       
      DO 20 I=1,16      
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
      X(I)=10.D0
      XL(I)=0.D0
      XU(I)=5.D0
      DO 20 J=1,8       
20    GG(J,I)=B(J,I)    
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.398473514099D-01     
      XEX(2)=0.791983155694D+00     
      XEX(3)=0.202870330224D+00     
      XEX(4)=0.844357916347D+00     
      XEX(5)=0.126990645286D+01     
      XEX(6)=0.934738707827D+00     
      XEX(7)=0.168196196924D+01     
      XEX(8)=0.155300877490D+00     
      XEX(9)=0.156787033356D+01     
      XEX(10)=-0.359021173251D-11     
      XEX(11)=-0.612900888082D-11     
      XEX(12)=-0.886794857449D-12     
      XEX(13)=0.660204066000D+00     
      XEX(14)=-0.254340725727D-11     
      XEX(15)=0.674255926901D+00     
      XEX(16)=-0.110433723798D-10     
      FEX=0.244899697515D+03 
      RETURN    
2     T=0.D0    
      DO 30 I=1,16      
      DO 30 J=1,16      
30    T=T+A(I,J)*(X(I)**2+X(I)+1.D0)*(X(J)**2+X(J)+1.D0)
      FX=T      
      RETURN    
3     DO 31 I=1,16       
      S(I)=0.D0 
      DO 32 J=1,16      
32    S(I)=S(I)+(A(I,J)+A(J,I))*(X(J)**2+X(J)+1.D0)*(2.D0*X(I)+1.D0) 
31    GF(I)=S(I)
      RETURN    
4     DO 33 I=1,8       
      IF (.NOT.INDEX1(I)) GOTO 33       
      S(I)=0.D0 
      DO 34 J=1,16      
34    S(I)=S(I)+B(I,J)*X(J)     
      G(I)=S(I)-C(I)    
33    CONTINUE  
5     RETURN    
      END       
