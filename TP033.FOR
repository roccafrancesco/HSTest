      SUBROUTINE TP33(MODE)     
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
      NILI=0    
      NINL=2    
      NELI=0    
      NENL=0    
      X(1)=0.D0 
      X(2)=0.D0 
      X(3)=3.D0 
      DO 6 I=1,3
      LXL(I)=.TRUE.     
6     XL(I)=0.D0
      LXU(1)=.FALSE.    
      LXU(2)=.FALSE.    
      LXU(3)=.TRUE.     
      XU(3)=5.D0
      LEX=.TRUE.
      NEX=1     
      XEX(1)=0.D0       
      XEX(2)=DSQRT(2.D0)
      XEX(3)=DSQRT(2.D0)
      FEX=DSQRT(2.D0)-6.0      
      GF(2)=0.D0
      GF(3)=1.D0
      RETURN    
2     FX=(X(1)-1.D0)*(X(1)-2.D0)*(X(1)-3.D0)+X(3)       
      RETURN    
3     GF(1)=3.D0*X(1)**2-12.D0*X(1)+11.D0       
      RETURN    
4     IF (INDEX1(1)) G(1)=X(3)**2-X(1)**2-X(2)**2       
      IF (INDEX1(2)) G(2)=X(1)**2+X(2)**2+X(3)**2-4.D0  
      RETURN    
5     IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,1)=-2.D0*X(1)
      GG(1,2)=-2.D0*X(2)
      GG(1,3)=2.D0*X(3) 
7     IF (.NOT.INDEX2(2)) GOTO 8
      GG(2,1)=2.D0*X(1) 
      GG(2,2)=2.D0*X(2) 
      GG(2,3)=2.D0*X(3) 
8     RETURN    
      END       
