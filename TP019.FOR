      SUBROUTINE TP19(MODE)     
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
      COMMON     /L19/ AEX,SAEX 
      DOUBLEPRECISION AEX,SAEX
      INTEGER I
      GOTO (1,2,3,4,5),MODE     
1     N=2       
      NILI=0    
      NINL=2    
      NELI=0    
      NENL=0    
      X(1)=20.1D0       
      X(2)=5.84D0       
      DO 6 I=1,2
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
6     XU(I)=100.D0      
      XL(1)=13.D0       
      XL(2)=0.D0
      LEX=.TRUE.
      NEX=1     
      SAEX=1.7280975D+1 
      AEX=DSQRT(SAEX)   
      XEX(1)=14.095D0   
      XEX(2)=5.D0-AEX   
      FEX=(4.095D0**3-(15.D0+AEX)**3)
      RETURN    
2     FX=((X(1)-10.D0)**3+(X(2)-20.D0)**3)
      RETURN    
3     GF(1)=(3.D0*(X(1)-10.D0)**2)
      GF(2)=(3.D0*(X(2)-20.D0)**2)
      RETURN    
4     IF (INDEX1(1)) G(1)=(X(1)-5.D0)**2+(X(2)-5.D0)**2-100.D0  
      IF (INDEX1(2)) G(2)=82.81D0-(X(1)-6.D0)**2-(X(2)-5.D0)**2 
      RETURN    
5     IF(.NOT.INDEX2(1)) GOTO 7 
      GG(1,1)=2.D0*(X(1)-5.D0)  
      GG(1,2)=2.D0*(X(2)-5.D0)  
7     IF (.NOT.INDEX2(2)) GOTO 8
      GG(2,1)=-2.D0*(X(1)-6.D0) 
      GG(2,2)=-2.D0*(X(2)-5.D0) 
8     RETURN    
      END      
