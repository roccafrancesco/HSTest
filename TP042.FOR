      SUBROUTINE TP42(MODE)     
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
1     N=4       
      NILI=0    
      NINL=0    
      NELI=1    
      NENL=1    
      DO 6 I=1,4
      X(I)=1.D0 
      LXU(I)=.FALSE.    
6     LXL(I)=.FALSE.    
      GG(1,1)=1.D0      
      GG(1,2)=0.D0      
      GG(1,3)=0.D0      
      GG(1,4)=0.D0      
      GG(2,1)=0.D0      
      GG(2,2)=0.D0      
      LEX=.TRUE.
      NEX=1     
      XEX(1)=2.D0       
      XEX(2)=2.D0       
      XEX(3)=DSQRT(0.72D0)      
      XEX(4)=DSQRT(1.28D0)      
      FEX=28.D0-10.D0*DSQRT(2.D0)       
      RETURN    
2     FX=(X(1)-1.D0)**2+(X(2)-2.D0)**2+(X(3)-3.D0)**2+(X(4)-4.D0)**2    
      RETURN    
3     DO 21 I=1,4       
21    GF(I)=2.D0*(X(I)-DBLE(I))       
      RETURN    
4     IF (INDEX1(1)) G(1)=X(1)-2.D0     
      IF (INDEX1(2)) G(2)=X(3)**2+X(4)**2-2.D0  
      RETURN    
5     IF (.NOT.INDEX2(2)) GOTO 8
      GG(2,3)=2.D0*X(3) 
      GG(2,4)=2.D0*X(4) 
8     RETURN    
      END       
