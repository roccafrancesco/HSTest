      SUBROUTINE TP73(MODE)     
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
      DIMENSION A(4)    
      DOUBLEPRECISION A  
      GOTO (1,2,3,4,5),MODE     
1     N=4       
      NILI=1    
      NINL=1    
      NELI=1    
      NENL=0    
      DO 6 I=1,4
      X(I)=1.D0 
      XL(I)=0.D0
      LXL(I)=.TRUE.     
6     LXU(I)=.FALSE.    
      GF(1)=24.55D0     
      GF(2)=26.75D0     
      GF(3)=39.D0       
      GF(4)=40.5D0      
      GG(1,1)=2.3D0     
      GG(1,2)=5.6D0     
      GG(1,3)=11.1D0    
      GG(1,4)=1.3D0     
      DO 31 I=1,4       
31    GG(3,I)=1.D0      
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.635521568605D+00     
      XEX(2)=-0.117862273760D-11     
      XEX(3)=0.312701880754D+00     
      XEX(4)=0.517765506011D-01     
      FEX=0.298943781573D+02 
      RETURN    
2     FX=24.55D0*X(1)+26.75D0*X(2)+39.D0*X(3)+40.50D0*X(4)      
3     RETURN    
4     IF (INDEX1(1)) G(1)=2.3D0*X(1)+5.6D0*X(2)+11.1D0*X(3)+1.3D0*X(4)  
     1     -5.D0
      IF (INDEX1(2)) G(2)=12.0D0*X(1)+11.9D0*X(2)+41.8D0*X(3)+52.1D0    
     /   *X(4)-1.645D0*    
     /   (0.28D0*X(1)**2+0.19D0*X(2)**2+20.5D0*X(3)**2+0.62D0*X(4)**2) 
     /   **.5D0-21.D0      
      IF (INDEX1(3)) G(3)=X(1)+X(2)+X(3)+X(4)-1.D0      
      RETURN    
5     IF (.NOT.INDEX2(2)) GOTO 8
      DO 30 I=1,4       
30    A(I)=1.645D0*X(I)*(0.28D0*X(1)**2+0.19D0*X(2)**2+20.5D0*X(3)**2   
     /     +0.62D0*X(4)**2)**(-0.5D0)
      GG(2,1)=12.D0-0.28D0*A(1) 
      GG(2,2)=11.9D0-0.19D0*A(2)
      GG(2,3)=41.8D0-20.5D0*A(3)
      GG(2,4)=52.1D0-0.62D0*A(4)
8     RETURN    
      END       
