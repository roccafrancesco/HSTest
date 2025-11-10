      SUBROUTINE TP26(MODE)     
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
      DOUBLEPRECISION A    
      GOTO (1,2,3,4,5),MODE     
1     N=3       
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=1    
      X(1)=-2.6D0       
      X(2)=2.D0 
      X(3)=2.D0 
      DO 6 I=1,3
      LXL(I)=.FALSE.    
6     LXU(I)=.FALSE.    
      LEX=.TRUE.
      NEX=1     
      XEX(1)=1.D0       
      XEX(2)=1.D0       
      XEX(3)=1.D0       
      A=DSQRT(139.D0/108.D0)    
      XEX(4)=(A-61.D0/54.D0)**(1.D0/3.D0)-(61.D0/54.D0+A)**(1.D0/3.D0)  
     /     -2.D0/3.D0   
      XEX(5)=XEX(4)     
      XEX(6)=XEX(4)     
      FEX=0.D0  
      RETURN    
2     FX=(X(1)-X(2))**2+(X(2)-X(3))**4  
      RETURN    
3     GF(1)=2.D0*(X(1)-X(2))    
      GF(3)=-4.D0*(X(2)-X(3))**3
      GF(2)=-GF(1)-GF(3)
      RETURN    
4     IF (INDEX1(1)) G(1)=X(1)*(1.D0+X(2)**2)+X(3)**4-3.D0      
      RETURN    
5     IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,1)=1.D0+X(2)**2      
      GG(1,2)=2.D0*X(1)*X(2)    
      GG(1,3)=4.D0*X(3)**3      
7     RETURN    
      END       
