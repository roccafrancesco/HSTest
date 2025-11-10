      SUBROUTINE TP45(MODE)     
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
      GOTO (1,2,3,4,4),MODE     
1     N=5       
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=0    
      DO 6 I=1,5
      X(I)=2.D0 
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
      XL(I)=0.D0
6     XU(I)=DBLE(I)   
      LEX=.TRUE.
      NEX=1     
      DO 30 I=1,5       
30    XEX(I)=DBLE(I)  
      FEX=1.D0  
      RETURN    
2     FX=2.D0-X(1)*X(2)*X(3)*X(4)*X(5)/120.D0   
      RETURN    
3     GF(1)=-X(2)*X(3)*X(4)*X(5)/120.D0 
      GF(2)=-X(1)*X(3)*X(4)*X(5)/120.D0 
      GF(3)=-X(1)*X(2)*X(4)*X(5)/120.D0 
      GF(4)=-X(1)*X(2)*X(3)*X(5)/120.D0 
      GF(5)=-X(1)*X(2)*X(3)*X(4)/120.D0 
4     RETURN    
      END       
