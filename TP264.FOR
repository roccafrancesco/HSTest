      SUBROUTINE TP264(MODE) 
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
 1    N=4 
      NILI=0 
      NINL=3 
      NELI=0 
      NENL=0 
      DO 6 I=1,4 
      X(I)=0.D+0 
      LXL(I)=.FALSE.
 6    LXU(I)=.FALSE.
      GG(3,4)=0.1D+1 
      LEX=.TRUE.
      NEX=1 
      FEX=-0.44D+2 	 
      XEX(1)=0.D+0 
      XEX(2)=0.1D+1 
      XEX(3)=0.2D+1 
      XEX(4)=-0.1D+1 
      RETURN 
 2    FX=(X(1)**2+X(2)**2+0.2D+1*X(3)**2+X(4)**2-0.5D+1*X(1)
     /       -0.5D+1*X(2)-0.21D+2*X(3)+0.7D+1*X(4))
      RETURN 
 3    GF(1)=(0.2D+1*X(1)-0.5D+1)    	       
      GF(2)=(0.2D+1*X(2)-0.5D+1)      	       
      GF(3)=(0.4D+1*X(3)-0.21D+2)      	       
      GF(4)=(0.2D+1*X(4)+0.7D+1) 
      RETURN 
 4    IF (INDEX1(1)) G(1)=0.8D+1-X(1)**2-X(2)**2-X(3)**2 
     /    -X(4)**2-X(1)+X(2)-X(3)+X(4)  
      IF (INDEX1(2)) G(2)=0.9D+1-X(1)**2-0.2D+1*X(2)**2
     /    -X(3)**2-0.2D+1*X(4)**2+X(1)+X(4) 
      IF (INDEX1(3)) G(3)=0.5D+1-0.2D+1*X(1)**2-X(2)**2
     /    -X(3)**2-0.2D+1*X(1)+X(2)+X(4) 
      RETURN 
 5    IF (.NOT.INDEX2(1)) GOTO 9 
      GG(1,1)=-0.1D+1-0.2D+1*X(1) 
      GG(1,2)=0.1D+1-0.2D+1*X(2) 
      GG(1,3)=-0.1D+1-0.2D+1*X(3) 
      GG(1,4)=0.1D+1-0.2D+1*X(4) 
 9    IF (.NOT.INDEX2(2)) GOTO 10 
      GG(2,1)=0.1D+1-0.2D+1*X(1) 
      GG(2,2)=-0.4D+1*X(2) 
      GG(2,3)=-0.2D+1*X(3) 
      GG(2,4)=-0.1D+1-0.4D+1*X(4)   
10    IF (.NOT.INDEX2(3)) GOTO 11 
      GG(3,1)=-0.2D+1-0.4D+1*X(1) 
      GG(3,2)=0.1D+1-0.2D+1*X(2) 
      GG(3,3)=-0.2D+1*X(3) 
11    RETURN 
      END       
