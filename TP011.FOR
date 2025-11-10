      SUBROUTINE TP11(MODE)     
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
      DOUBLEPRECISION AEX,AW,QAW     
      GOTO (1,2,3,4,5),MODE     
1     N=2       
      NILI=0    
      NINL=1    
      NELI=0    
      NENL=0    
      DO 6 I=1,2
      LXL(I)=.FALSE.    
6     LXU(I)=.FALSE.    
      X(1)=4.9D0
      X(2)=0.1D0
      LEX=.TRUE.
      NEX=1     
      AEX=7.5D0*DSQRT(6.D0)     
      AW=(DSQRT(AEX**2+1.D0)+AEX)**(1.D0/3.D0)  
      QAW=AW**2 
      XEX(1)=(AW-1.D0/AW)/DSQRT(6.D0)   
      XEX(2)=(QAW-2.D0+1.D0/QAW)/6.D0   
      FEX=(XEX(1)-5.D0)**2+XEX(2)**2-25.D0      
      GG(1,2)=1.D0      
      RETURN    
2     FX=(X(1)-5.D0)**2+X(2)**2-25.D0   
      RETURN    
3     GF(1)=2.D0*(X(1)-5.D0)    
      GF(2)=2.D0*X(2)   
      RETURN    
4     IF (INDEX1(1)) G(1)=-X(1)**2+X(2) 
      RETURN    
5     IF (INDEX2(1)) GG(1,1)=-2.D0*X(1) 
      RETURN    
      END       
