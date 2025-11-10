      SUBROUTINE TP37(MODE)     
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
      GOTO (1,2,3,4,5),MODE     
1     N=3       
      NILI=2    
      NINL=0    
      NELI=0    
      NENL=0    
      DO 6 I=1,3
      X(I)=10.D0
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
      XU(I)=42.D0       
6     XL(I)=0.D0
      LEX=.TRUE.
      NEX=1     
      XEX(1)=24.D0      
      XEX(2)=12.D0      
      XEX(3)=12.D0      
      FEX=-3.456D+3     
      GG(1,1)=-1.D0     
      GG(1,2)=-2.D0     
      GG(1,3)=-2.D0     
      GG(2,1)=1.D0      
      GG(2,2)=2.D0      
      GG(2,3)=2.D0      
      RETURN    
2     FX=-X(1)*X(2)*X(3)
      RETURN    
3     GF(1)=-X(2)*X(3)  
      GF(2)=-X(1)*X(3)  
      GF(3)=-X(1)*X(2)  
      RETURN    
4     IF (INDEX1(1)) G(1)=72.D0-X(1)-2.D0*X(2)-2.D0*X(3)
      IF (INDEX1(2)) G(2)=X(1)+2.D0*X(2)+2.D0*X(3)      
5     RETURN    
      END       
