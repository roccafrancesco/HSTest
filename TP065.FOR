      SUBROUTINE TP65(MODE)     
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
      DOUBLEPRECISION V1,V2      
      GOTO (1,2,3,4,5),MODE     
1     N=3       
      NILI=0    
      NINL=1    
      NELI=0    
      NENL=0    
      X(1)=-5.D0
      X(2)=5.D0 
      X(3)=0.D0 
      DO 6 I=1,3
      LXL(I)=.TRUE.     
6     LXU(I)=.TRUE.     
      XL(1)=-4.5D0      
      XL(2)=-4.5D0      
      XL(3)=-5.D0       
      XU(1)=4.5D0       
      XU(2)=4.5D0       
      XU(3)=5.D0
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.365046182158D+01     
      XEX(2)=0.365046168940D+01     
      XEX(3)=0.462041750754D+01     
      FEX=0.953528856757D+00 
      RETURN    
2     FX=(X(1)-X(2))**2+((X(1)+X(2)-10.D0)/3.D0)**2+(X(3)-5.D0)**2      
      RETURN    
3     V1=2.D0*(X(1)-X(2))       
      V2=2.D0*(X(1)+X(2)-10.D0)/9.D0    
      GF(1)=V1+V2       
      GF(2)=-V1+V2      
      GF(3)=2.D0*(X(3)-5.D0)    
      RETURN    
4     IF (INDEX1(1)) G(1)=48.D0-X(1)**2-X(2)**2-X(3)**2 
      RETURN    
5     IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,1)=-2.D0*X(1)
      GG(1,2)=-2.D0*X(2)
      GG(1,3)=-2.D0*X(3)
7     RETURN    
      END       
