      SUBROUTINE TP269(MODE) 
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
     /           /L15/ F(200)    
     /           /L16/ DF(200,200)    
     /           /L20/ LEX,NEX,FEX,XEX(NMAX) 
      INTEGER I,J
      DOUBLEPRECISION  F,DF,HGG(3,5),HDF(4,5) 
      DATA HGG/1.D+0,2*0.D+0,3.D+0,0.D+0,1.D+0,0.D+0,1.D+0,2*0.D+0,
     F         1.D+0,2*0.D+0,-2.D+0,-1.D+0/
      DATA HDF/1.D+0,3*0.D+0,-1.D+0,1.D+0,3*0.D+0,1.D+0,4*0.D+0,
     F         1.D+0,4*0.D+0,1.D+0/
      GOTO (1,2,2,4,5),MODE 
 1    N=5 
      NILI=0 
      NINL=0 
      NELI=3 
      NENL=0 
      DO 6 I=1,5 
      X(I)=2.D+0
      LXL(I)=.FALSE.
      LXU(I)=.FALSE.
      DO 7 J=1,3 
      GG(J,I)=HGG(J,I) 
 7    DF(J,I)=HDF(J,I) 
 6    DF(4,I)=HDF(4,I) 
      LEX=.TRUE. 
      NEX=1 
      FEX=0.176D+3/0.43D+2 
      XEX(1)=-0.33D+2/0.43D+2 
      XEX(2)=0.11D+2/0.43D+2 
      XEX(3)=0.27D+2/0.43D+2 
      XEX(4)=-0.5D+1/0.43D+2 
      XEX(5)=0.11D+2/0.43D+2 
      RETURN 
 2    F(1)=X(1)-X(2) 
      F(2)=X(2)+X(3)-0.2D+1 
      F(3)=X(4)-0.1D+1 
      F(4)=X(5)-0.1D+1 
      IF (MODE.EQ.3) GOTO 3
      FX=0.D+0 
      DO 8 I=1,4 
 8    FX=FX+F(I)**2
      RETURN 
 3    GF(1)=0.2D+1*(X(1)-X(2)) 
      GF(2)=0.2D+1*(0.2D+1*X(2)+X(3)-X(1)-0.2D+1) 
      GF(3)=0.2D+1*(X(2)+X(3)-0.2D+1) 
      GF(4)=0.2D+1*(X(4)-0.1D+1) 
      GF(5)=0.2D+1*(X(5)-0.1D+1) 
      RETURN 
 4    IF (INDEX1(1)) G(1)=X(1)+0.3D+1*X(2) 
      IF (INDEX1(2)) G(2)=X(3)+X(4)-0.2D+1*X(5) 
      IF (INDEX1(3)) G(3)=X(2)-X(5) 
 5    RETURN	      
      END 
