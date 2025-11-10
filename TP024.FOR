      SUBROUTINE TP24(MODE)     
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
      COMMON     /L24/ A
      DOUBLEPRECISION A    
      INTEGER I
      A=DSQRT(3.0D0)     
      GOTO (1,2,3,4,5),MODE     
1     N=2       
      NILI=3    
      NINL=0    
      NELI=0    
      NENL=0    
      X(1)=1.D0 
      X(2)=0.5D0
      DO 6 I=1,2
      LXL(I)=.TRUE.     
      LXU(I)=.FALSE.    
6     XL(I)=0.D0
      LEX=.TRUE.
      NEX=1     
      XEX(1)=3.D0       
      XEX(2)=A  
      FEX=-1.D0 
      GG(1,1)=1.D0/A    
      GG(1,2)=-1.D0     
      GG(2,1)=1.D0      
      GG(2,2)=A 
      GG(3,1)=-1.D0     
      GG(3,2)=-A
      RETURN    
2     FX=((X(1)-3.D0)**2-9.D0)*X(2)**3/(27.D0*A)
      RETURN    
3     GF(1)=2.D0*(X(1)-3.D0)*X(2)**3/(27.D0*A)  
      GF(2)=((X(1)-3.D0)**2-9.D0)*X(2)**2/(9.D0*A)      
      RETURN    
4     IF (INDEX1(1)) G(1)=X(1)/A-X(2)   
      IF (INDEX1(2)) G(2)=X(1)+X(2)*A   
      IF (INDEX1(3)) G(3)=6.D0-X(2)*A-X(1)      
5     RETURN    
      END       
