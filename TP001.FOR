      SUBROUTINE TP1(MODE)      
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
      GOTO (1,2,3,4,4),MODE     
1     N=2       
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=0    
      X(1)=-2.D0
      X(2)=1.D0 
      LXL(1)=.FALSE.    
      LXL(2)=.TRUE.     
      LXU(1)=.FALSE.    
      LXU(2)=.FALSE.    
      XL(2)=-1.5D0      
      LEX=.TRUE.
      NEX=1     
      XEX(1)=1.D0       
      XEX(2)=1.D0       
      FEX=0.D0  
      RETURN    
2     FX=100.D0*(X(2)-X(1)**2)**2+(1.D0-X(1))**2
      RETURN    
3     GF(2)=200.D0*(X(2)-X(1)**2)       
      GF(1)=-2.D0*(X(1)*(GF(2)-1.D0)+1.D0)      
4     RETURN    
      END       
