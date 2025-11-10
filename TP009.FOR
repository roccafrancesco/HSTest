      SUBROUTINE TP9(MODE)      
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
      DOUBLEPRECISION V,V1,V2,V3,V4
      V=4.D0*DATAN(1.D0)
      GOTO (1,2,3,4,5),MODE     
1     N=2       
      NILI=0    
      NINL=0    
      NELI=1    
      NENL=0    
      X(1)=0.D0 
      X(2)=0.D0 
      DO 6 I=1,2
      LXL(I)=.FALSE.    
6     LXU(I)=.FALSE.    
      LEX=.TRUE.
      NEX=1    
      FEX=-0.5D0
      XEX(1)=-3.D0      
      XEX(2)=-4.D0      
      GG(1,1)=4.D0      
      GG(1,2)=-3.D0     
      RETURN    
2     FX=DSIN(V*X(1)/12.D0)*DCOS(V*X(2)/16.D0)  
      RETURN    
3     V3=V/12.D0
      V4=V/16.D0
      V1=V3*X(1)
      V2=V4*X(2)
      GF(1)=V3*DCOS(V1)*DCOS(V2)
      GF(2)=-V4*DSIN(V1)*DSIN(V2)       
      RETURN    
4     IF(INDEX1(1)) G(1)=4.D0*X(1)-3.D0*X(2)    
5     RETURN    
      END       
