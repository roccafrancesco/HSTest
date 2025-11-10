      SUBROUTINE TP60(MODE)     
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
      DOUBLEPRECISION V1 
      GOTO (1,2,3,4,5),MODE     
1     N=3       
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=1    
      DO 6 I=1,3
      X(I)=2.D0 
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
      XL(I)=-10.D0      
6     XU(I)=10.D0       
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.110485902423D+01     
      XEX(2)=0.119667419413D+01     
      XEX(3)=0.153526225739D+01     
      FEX= 0.325682002513D-01 
      RETURN    
2     FX=(X(1)-1.D0)**2+(X(1)-X(2))**2+(X(2)-X(3))**4   
      RETURN    
3     V1=2.D0*(X(1)-X(2))       
      GF(1)=2.D0*(X(1)-1.D0)+V1 
      GF(3)=-4.D0*(X(2)-X(3))**3
      GF(2)=-GF(3)-V1   
      RETURN    
4     IF (INDEX1(1)) G(1)=X(1)*(1.D0+X(2)**2)+X(3)**4-4.D0-3.D0 
     1     *DSQRT(2.D0) 
      RETURN    
5     IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,1)=1.D0+X(2)**2      
      GG(1,2)=2.D0*X(1)*X(2)    
      GG(1,3)=4.D0*X(3)**3      
7     RETURN    
      END       
