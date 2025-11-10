      SUBROUTINE TP53(MODE)     
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
      GOTO (1,2,3,4,5),MODE     
1     N=5       
      NILI=0    
      NINL=0    
      NELI=3    
      NENL=0    
      DO 6 I=1,5
      X(I)=2.D0 
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
      XL(I)=-10.D0      
6     XU(I)=10.D0       
      DO 15 I=1,3       
      DO 15 J=1,5       
15    GG(I,J)=0.D0      
      GG(1,1)=1.D0      
      GG(1,2)=3.D0      
      GG(2,3)=1.D0      
      GG(2,4)=1.D0      
      GG(2,5)=-2.D0     
      GG(3,2)=1.D0      
      GG(3,5)=-1.D0     
      LEX=.TRUE.
      NEX=1     
      XEX(1)=-33.D0/43.D0       
      XEX(2)=11.D0/43.D0
      XEX(3)=27.D0/43.D0
      XEX(4)=-5.D0/43.D0
      XEX(5)=11.D0/43.D0
      FEX=176.D0/43.D0  
      RETURN    
2     FX=(X(1)-X(2))**2+(X(2)+X(3)-2.D0)**2+(X(4)-1.D0)**2+(X(5)
     /     -1.D0)**2    
      RETURN    
3     GF(1)=2.D0*(X(1)-X(2))    
      GF(3)=2.D0*(X(2)+X(3)-2.D0)       
      GF(2)=GF(3)-GF(1) 
      GF(4)=2.D0*(X(4)-1.D0)    
      GF(5)=2.D0*(X(5)-1.D0)    
      RETURN    
4     IF (INDEX1(1)) G(1)=X(1)+3.D0*X(2)
      IF (INDEX1(2)) G(2)=X(3)+X(4)-2.D0*X(5)   
      IF (INDEX1(3)) G(3)=X(2)-X(5)     
5     RETURN    
      END       
