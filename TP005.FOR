      SUBROUTINE TP5(MODE)      
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
      DOUBLEPRECISION A,V1,V2      
      GOTO (1,2,3,4,4),MODE     
1     N=2       
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=0    
      X(1)=0.D0 
      X(2)=0.D0 
      DO 6 I=1,2
      LXL(I)=.TRUE.     
6     LXU(I)=.TRUE.     
      XL(1)=-1.5D0      
      XL(2)=-3.D0       
      XU(1)=4.D0
      XU(2)=3.D0
      A=4.D0*DATAN(1.D0)
      LEX=.TRUE.
      NEX=1     
      XEX(1)=0.5D0-A/3.D0       
      XEX(2)=XEX(1)-1.D0
      FEX=-DSQRT(3.D0)/2.D0-A/3.D0      
      RETURN    
2     FX=DSIN(X(1)+X(2))+(X(1)-X(2))**2-1.5D0*X(1)+2.5D0*X(2)+1.D0      
      RETURN    
3     V1=DCOS(X(1)+X(2))
      V2=2.D0*(X(1)-X(2))       
      GF(1)=V1+V2-1.5D0 
      GF(2)=V1-V2+2.5D0 
4     RETURN    
      END       
