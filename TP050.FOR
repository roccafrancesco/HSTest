      SUBROUTINE TP50(MODE)     
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
      DOUBLEPRECISION  V1,V2,V3,V4
      GOTO (1,2,3,4,5),MODE     
1     N=5       
      NILI=0    
      NINL=0    
      NELI=3    
      NENL=0    
      X(1)=35.D0
      X(2)=-31.D0       
      X(3)=11.D0
      X(4)=5.D0 
      X(5)=-5.D0
      DO 6 I=1,5
      LXL(I)=.FALSE.    
6     LXU(I)=.FALSE.    
      DO 15 I=1,3       
      DO 15 J=1,5       
15    GG(I,J)=0.D0      
      GG(1,1)=1.D0      
      GG(1,2)=2.D0      
      GG(1,3)=3.D0      
      GG(2,2)=1.D0      
      GG(2,3)=2.D0      
      GG(2,4)=3.D0      
      GG(3,3)=1.D0      
      GG(3,4)=2.D0      
      GG(3,5)=3.D0      
      LEX=.TRUE.
      NEX=1     
      DO 30 I=1,5       
30    XEX(I)=1.D0       
      FEX=0.D0  
      RETURN    
2     FX=(X(1)-X(2))**2+(X(2)-X(3))**2+(X(3)-X(4))**4+(X(4)-X(5))**4    
      RETURN    
3     V1=2.D0*(X(1)-X(2))       
      V2=2.D0*(X(2)-X(3))       
      V3=4.D0*(X(3)-X(4))**3    
      V4=4.D0*(X(4)-X(5))**3    
      GF(1)=V1  
      GF(2)=-V1+V2      
      GF(3)=-V2+V3      
      GF(4)=-V3+V4      
      GF(5)=-V4 
      RETURN    
4     IF (INDEX1(1)) G(1)=X(1)+2.D0*X(2)+3.D0*X(3)-6.D0 
      IF (INDEX1(2)) G(2)=X(2)+2.D0*X(3)+3.D0*X(4)-6.D0 
      IF (INDEX1(3)) G(3)=X(3)+2.D0*X(4)+3.D0*X(5)-6.D0 
5     RETURN    
      END       
