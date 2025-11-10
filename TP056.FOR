      SUBROUTINE TP56(MODE)     
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
1     N=7       
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=4    
      X(1)=1.D0 
      X(2)=1.D0 
      X(3)=1.D0 
      DO 6 I=1,7
      LXL(I)=.FALSE.    
6     LXU(I)=.FALSE.    
      X(4)=DASIN(DSQRT(1.D0/4.2D0))    
      X(5)=X(4) 
      X(6)=X(4) 
      X(7)=DASIN(DSQRT(5.D0/7.2D0))    
      DO 30 I=4,7       
30    GF(I)=0.D0
      DO 15 I=1,4       
      DO 15 J=1,7       
15    GG(I,J)=0.D0      
      GG(1,1)=1.D0      
      GG(2,2)=1.D0      
      GG(3,3)=1.D0      
      GG(4,1)=1.D0      
      GG(4,2)=2.D0      
      GG(4,3)=2.D0      
      LEX=.TRUE.
      NEX=1    
      XEX(1)=2.4D0      
      XEX(2)=1.2D0      
      XEX(3)=1.2D0      
      XEX(4)=DASIN(DSQRT(4.D0/7.D0))   
      XEX(5)=DASIN(DSQRT(2.D0/7.D0))   
      XEX(6)=XEX(5)     
      XEX(7)=2.D0*DATAN(1.D0)   
      FEX=-3.456D0      
      RETURN    
2     FX=-X(1)*X(2)*X(3)
      RETURN    
3     GF(1)=-X(2)*X(3)  
      GF(2)=-X(1)*X(3)  
      GF(3)=-X(1)*X(2)  
      RETURN    
4     IF (INDEX1(1)) G(1)=X(1)-4.2D0*DSIN(X(4))**2      
      IF (INDEX1(2)) G(2)=X(2)-4.2D0*DSIN(X(5))**2      
      IF (INDEX1(3)) G(3)=X(3)-4.2D0*DSIN(X(6))**2      
      IF (INDEX1(4)) G(4)=X(1)+2.D0*X(2)+2.D0*X(3)-7.2D0*DSIN(X(7))**2  
      RETURN    
5     IF (INDEX2(1)) GG(1,4)=-8.4D0*DSIN(X(4))*DCOS(X(4))       
      IF (INDEX2(2)) GG(2,5)=-8.4D0*DSIN(X(5))*DCOS(X(5))       
      IF (INDEX2(3)) GG(3,6)=-8.4D0*DSIN(X(6))*DCOS(X(6))       
      IF (INDEX2(4)) GG(4,7)=-14.4D0*DSIN(X(7))*DCOS(X(7))      
      RETURN    
      END       
