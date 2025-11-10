      SUBROUTINE TP21(MODE)     
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
1     N=2       
      NILI=1    
      NINL=0    
      NELI=0    
      NENL=0    
      X(1)=2.D0
      X(2)=-1.D0
      DO 6 I=1,2
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
6     XU(I)=50.D0       
      XL(1)=2.D0
      XL(2)=-50.D0      
      LEX=.TRUE.
      NEX=1     
      XEX(1)=2.D0       
      XEX(2)=0.D0       
      FEX=-99.96D0      
      GG(1,1)=10.D0     
      GG(1,2)=-1.D0     
      RETURN    
2     FX=(0.01*X(1)**2 + X(2)**2 - 100.D0)
      RETURN    
3     GF(1)=0.02*X(1)
      GF(2)=2.0*X(2)
      RETURN    
4     IF (INDEX1(1)) G(1)=10.D0*X(1)-X(2)-10.D0 
5     RETURN    
      END       
