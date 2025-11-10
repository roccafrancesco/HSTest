      SUBROUTINE TP77(MODE)     
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
1     N=5       
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=2    
      DO 6 I=1,5
      X(I)=2.D0 
      LXL(I)=.FALSE.    
6     LXU(I)=.FALSE.    
      GG(1,2)=0.D0      
      GG(1,3)=0.D0      
      GG(2,1)=0.D0      
      GG(2,2)=1.D0      
      GG(2,5)=0.D0      
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.116617219726D+01     
      XEX(2)=0.118211138813D+01     
      XEX(3)=0.138025704044D+01     
      XEX(4)=0.150603627961D+01     
      XEX(5)=0.610920257517D+00     
      FEX=0.241505128786D+00 
      RETURN    
2     FX=(X(1)-1.D0)**2+(X(1)-X(2))**2+(X(3)-1.D0)**2+(X(4)-1.D0)**4    
     -+(X(5)-1.D0)**6   
      RETURN    
3     GF(1)=2.D0*(2.D0*X(1)-X(2)-1.D0)  
      GF(2)=-2.D0*(X(1)-X(2))   
      GF(3)=2.D0*(X(3)-1.D0)    
      GF(4)=4.D0*(X(4)-1.D0)**3 
      GF(5)=6.D0*(X(5)-1.D0)**5 
      RETURN    
4     IF (INDEX1(1)) G(1)=X(1)**2*X(4)+DSIN(X(4)-X(5))-2.D0*DSQRT(2.D0) 
      IF (INDEX1(2)) G(2)=X(2)+X(3)**4*X(4)**2-8.D0-DSQRT(2.D0) 
      RETURN    
5     IF (.NOT.INDEX2(1)) GOTO 7
      V1=DCOS(X(4)-X(5))
      GG(1,1)=2.D0*X(1)*X(4)    
      GG(1,4)=X(1)**2+V1
      GG(1,5)=-V1       
7     IF (.NOT.INDEX2(2)) GOTO 8
      GG(2,3)=4.D0*X(3)**3*X(4)**2      
      GG(2,4)=2.D0*X(3)**4*X(4) 
8     RETURN    
      END       
