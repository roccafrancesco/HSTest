      SUBROUTINE TP55(MODE)     
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
      INTEGER I,J
      DOUBLEPRECISION V1,X14    
      GOTO (1,2,3,4,5),MODE     
1     N=6       
      NILI=0    
      NINL=0    
      NELI=6    
      NENL=0    
      X(1)=1.D0 
      X(2)=2.D0 
      X(3)=0.D0 
      X(4)=0.D0 
      X(5)=0.D0 
      X(6)=2.D0 
      DO 6 I=1,6
      LXL(I)=.TRUE.     
      LXU(I)=.FALSE.    
6     XL(I)=0.D0
      LXU(1)=.TRUE.     
      LXU(4)=.TRUE.     
      XU(1)=1.D+0
      XU(4)=1.D+0
      DO 15 I=1,6       
      DO 15 J=1,6       
15    GG(I,J)=0.D0      
      GF(2)=2.D0
      GF(3)=0.D0
      GF(5)=4.D0
      GF(6)=0.D0
      GG(1,1)=1.D0      
      GG(1,2)=2.D0      
      GG(1,5)=5.D0      
      GG(2,1)=1.D0      
      GG(2,2)=1.D0      
      GG(2,3)=1.D0      
      GG(3,4)=1.D0      
      GG(3,5)=1.D0      
      GG(3,6)=1.D0      
      GG(4,1)=1.D0      
      GG(4,4)=1.D0      
      GG(5,2)=1.D0      
      GG(5,5)=1.D0      
      GG(6,3)=1.D0      
      GG(6,6)=1.D0      
      LEX=.TRUE.
      NEX=1     
      XEX(1)=0.D0       
      XEX(2)=4.D0/3.D0  
      XEX(3)=5.D0/3.D0  
      XEX(4)=1.D0       
      XEX(5)=2.D0/3.D0  
      XEX(6)=1.D0/3.D0  
      FEX=19.D0/3.D0    
      RETURN  
2     X14 = X(1)*X(4)
      IF (X14.GT.1.0D1) X14 = 1.0D1   
      FX=X(1)+2.D0*X(2)+4.D0*X(5)+DEXP(X14)       
      RETURN    
3     V1=DEXP(X(1)*X(4))
      GF(1)=1.D0+X(4)*V1
      GF(4)=X(1)*V1     
      RETURN    
4     IF (INDEX1(1)) G(1)=X(1)+2.D0*X(2)+5.D0*X(5)-6.D0 
      IF (INDEX1(2)) G(2)=X(1)+X(2)+X(3)-3.D0   
      IF (INDEX1(3)) G(3)=X(4)+X(5)+X(6)-2.D0   
      IF (INDEX1(4)) G(4)=X(1)+X(4)-1.D0
      IF (INDEX1(5)) G(5)=X(2)+X(5)-2.D0
      IF (INDEX1(6)) G(6)=X(3)+X(6)-2.D0
5     RETURN    
      END       
