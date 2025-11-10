      SUBROUTINE TP35(MODE)     
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
      NILI=1    
      NINL=0    
      NELI=0    
      NENL=0    
      DO 6 I=1,3
      X(I)=0.5D0
      LXL(I)=.TRUE.     
      LXU(I)=.FALSE.    
6     XL(I)=0.D0
      LEX=.TRUE.
      NEX=1     
      XEX(1)=4.D0/3.D0  
      XEX(2)=7.D0/9.D0  
      XEX(3)=4.D0/9.D0  
      FEX=1.D0/9.D0     
      GG(1,1)=-1.D0     
      GG(1,2)=-1.D0     
      GG(1,3)=-2.D0     
      RETURN    
2     FX=9.D0-8.D0*X(1)-6.D0*X(2)-4.D0*X(3)+2.D0*X(1)**2+2.D0*X(2)**2   
     /     +X(3)**2+2.D0*X(1)*X(2)+2.D0*X(1)*X(3)    
      RETURN    
3     GF(1)=-8.D0+4.D0*X(1)+2.D0*X(2)+2.D0*X(3) 
      GF(2)=-6.D0+4.D0*X(2)+2.D0*X(1)   
      GF(3)=-4.D0+2.D0*X(3)+2.D0*X(1)   
      RETURN    
4     IF (INDEX1(1)) G(1)=-X(1)-X(2)-2.D0*X(3)+3.D0     
5     RETURN    
      END       
