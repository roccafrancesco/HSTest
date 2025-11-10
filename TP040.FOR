      SUBROUTINE TP40(MODE)     
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
1     N=4       
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=3    
      DO 6 I=1,4
      X(I)=0.8D0
      LXL(I)=.FALSE.    
6     LXU(I)=.FALSE.    
      DO 15 I=1,3       
      DO 15 J=1,4       
15    GG(I,J)=0.D0      
      GG(2,3)=-1.D0     
      GG(3,2)=-1.D0     
      LEX=.TRUE.
      XEX(1)=2.D0**(-1.D0/3.D0) 
      XEX(2)=2.D0**(-0.5D0)     
      XEX(3)=2.D0**(-11.D0/12.D0)       
      XEX(4)=2.D0**(-0.25D0)    
      XEX(5)=XEX(1)     
      XEX(6)=XEX(2)     
      XEX(7)=-XEX(3)    
      XEX(8)=-XEX(4)    
      FEX=-0.25D0       
      NEX=2     
      RETURN    
2     FX=-X(1)*X(2)*X(3)*X(4)   
      RETURN    
3     GF(1)=-X(2)*X(3)*X(4)     
      GF(2)=-X(1)*X(3)*X(4)     
      GF(3)=-X(1)*X(2)*X(4)     
      GF(4)=-X(1)*X(2)*X(3)     
      RETURN    
4     IF (INDEX1(1)) G(1)=X(1)**3+X(2)**2-1.D0  
      IF (INDEX1(2)) G(2)=X(1)**2*X(4)-X(3)     
      IF (INDEX1(3)) G(3)=X(4)**2-X(2)  
      RETURN    
5     IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,1)=3.D0*X(1)**2      
      GG(1,2)=2.D0*X(2) 
7     IF (.NOT.INDEX2(2)) GOTO 8
      GG(2,1)=2.D0*X(1)*X(4)    
      GG(2,4)=X(1)**2   
8     IF (.NOT.INDEX2(3)) GOTO 9
      GG(3,4)=2.D0*X(4) 
9     RETURN    
      END       
