      SUBROUTINE TP80(MODE)     
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
      DOUBLEPRECISION V1,V2,T
      GOTO (1,2,3,4,5),MODE     
1     N=5       
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=3    
      DO 6 I=1,5
      LXL(I)=.TRUE.     
6     LXU(I)=.TRUE.     
      X(1)=-2.D0
      X(2)=2.D0 
      X(3)=2.D0 
      X(4)=-1.D0
      X(5)=-1.D0
      DO 20 I=1,2       
      XL(I)=-2.3D0      
20    XU(I)=2.3D0       
      DO 21 I=3,5       
      XL(I)=-3.2D0      
21    XU(I)=3.2D0       
      GG(2,1)=0.D0      
      GG(3,3)=0.D0      
      GG(3,4)=0.D0      
      GG(3,5)=0.D0      
      LEX=.FALSE.       
      NEX=1
      XEX(1)=-0.171714294417D+01     
      XEX(2)=0.159570896503D+01     
      XEX(3)=0.182724691654D+01     
      XEX(4)=-0.763641279311D+00     
      XEX(5)=-0.763645016315D+00     
      FEX=0.539498477624D-01 
      RETURN    
2     FX=DEXP(X(1)*X(2)*X(3)*X(4)*X(5)) 
      RETURN    
3     V1=X(4)*X(5)      
      V2=X(1)*X(2)      
      T=DEXP(V2*X(3)*V1)
      GF(1)=X(2)*X(3)*V1*T      
      GF(2)=X(1)*X(3)*V1*T      
      GF(3)=V1*V2*T     
      GF(4)=V2*X(3)*X(5)*T      
      GF(5)=V2*X(3)*X(4)*T      
      RETURN    
4     IF (INDEX1(1)) G(1)=X(1)**2+X(2)**2+X(3)**2+X(4)**2+X(5)**2-10.D0 
      IF (INDEX1(2)) G(2)=X(2)*X(3)-5.D0*X(4)*X(5)      
      IF (INDEX1(3)) G(3)=X(1)**3+X(2)**3+1.D0  
      RETURN    
5     IF (.NOT.INDEX2(1)) GOTO 7
      DO 30 I=1,5       
30    GG(1,I)=2.D0*X(I) 
7     IF (.NOT.INDEX2(2)) GOTO 8
      GG(2,2)=X(3)      
      GG(2,3)=X(2)      
      GG(2,4)=-5.D0*X(5)
      GG(2,5)=-5.D0*X(4)
8     IF (.NOT.INDEX2(3)) GOTO 9
      GG(3,1)=3.D0*X(1)**2      
      GG(3,2)=3.D0*X(2)**2      
9     RETURN    
      END       
