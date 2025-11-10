      SUBROUTINE TP8(MODE)      
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
      DOUBLEPRECISION A,B
      GOTO (1,3,3,4,5),MODE     
1     N=2       
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=2    
      X(1)=2.D0 
      X(2)=1.D0 
      A=DSQRT((25.D0+DSQRT(301.D0))/2.D0)       
      B=DSQRT((25.D0-DSQRT(301.D0))/2.D0)       
      DO 6 I=1,2
      LXL(I)=.FALSE.    
6     LXU(I)=.FALSE.    
      LEX=.TRUE.
      NEX=4     
      XEX(1)=A  
      XEX(2)=9.D0/A     
      XEX(5)=B  
      XEX(6)=9.D0/B     
      DO 30 I=3,7,4     
      DO 30 J=1,2       
30    XEX(I+J-1)=-XEX(I+J-3)    
      FEX=-1.D0 
      GF(1)=0.D0
      GF(2)=0.D0
      FX=-1.D0  
3     RETURN    
4     IF (INDEX1(1)) G(1)=X(1)**2+X(2)**2-25.D0 
      IF (INDEX1(2)) G(2)=X(1)*X(2)-9.D0
      RETURN    
5     IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,1)=2.D0*X(1) 
      GG(1,2)=2.D0*X(2) 
7     IF (.NOT.INDEX2(2)) GOTO 8
      GG(2,1)=X(2)      
      GG(2,2)=X(1)      
8     RETURN    
      END       
