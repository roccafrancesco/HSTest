      SUBROUTINE TP44(MODE)     
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
      GOTO (1,2,3,4,5),MODE     
1     N=4       
      NILI=6    
      NINL=0    
      NELI=0    
      NENL=0    
      DO 6 I=1,4
      X(I)=0.D0 
      XL(I)=0.D0
      LXL(I)=.TRUE.     
6     LXU(I)=.FALSE.    
      DO 15 I=1,6       
      DO 15 J=1,4       
15    GG(I,J)=0.D0      
      GG(1,1)=-1.D0     
      GG(1,2)=-2.D0     
      GG(2,1)=-4.D0     
      GG(2,2)=-1.D0     
      GG(3,1)=-3.D0     
      GG(3,2)=-4.D0     
      GG(4,3)=-2.D0     
      GG(4,4)=-1.D0     
      GG(5,3)=-1.D0     
      GG(5,4)=-2.D0     
      GG(6,3)=-1.D0     
      GG(6,4)=-1.D0     
      LEX=.TRUE.
      NEX=1     
      XEX(1)=0.D0       
      XEX(2)=3.D0       
      XEX(3)=0.D0       
      XEX(4)=4.D0       
      FEX=-15.D0
      RETURN    
2     FX=X(1)-X(2)-X(3)-X(1)*X(3)+X(1)*X(4)+X(2)*X(3)-X(2)*X(4) 
      RETURN    
3     GF(1)=1.D0-X(3)+X(4)      
      GF(2)=-1.D0+X(3)-X(4)     
      GF(3)=-1.D0-X(1)+X(2)     
      GF(4)=X(1)-X(2)   
      RETURN    
4     IF (INDEX1(1)) G(1)=8.D0-X(1)-2.D0*X(2)   
      IF (INDEX1(2)) G(2)=12.D0-4.D0*X(1)-X(2)  
      IF (INDEX1(3)) G(3)=12.D0-3.D0*X(1)-4.D0*X(2)     
      IF (INDEX1(4)) G(4)=8.D0-2.D0*X(3)-X(4)   
      IF (INDEX1(5)) G(5)=8.D0-X(3)-2.D0*X(4)   
      IF (INDEX1(6)) G(6)=5.D0-X(3)-X(4)
5     RETURN    
      END       
