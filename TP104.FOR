      SUBROUTINE TP104(MODE)    
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
      COMMON     /D104/A  
      DOUBLEPRECISION A,BX,V1,V2,EPS  
      INTEGER I,J
      GOTO (1,2,3,4,5),MODE     
1     N=8       
      NILI=0    
      NINL=6    
      NELI=0    
      NENL=0    
      DO 33 I=1,8       
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
      XL(I)=0.1D0       
      XU(I)=10.D0       
33    CONTINUE  
      A=0.0588D0
      X(1)=6.D0 
      X(2)=3.D0 
      X(3)=0.4D0
      X(4)=0.2D0
      X(5)=6.D0 
      X(6)=6.D0 
      X(7)=1.D0 
      X(8)=0.5D0
      GF(3)=0.D0
      GF(4)=0.D0
      GF(5)=0.D0
      GF(6)=0.D0
      DO 34 I=1,4       
      DO 34 J=1,8       
34    GG(I,J)=0.D0      
      GG(1,1)=-0.1D0    
      GG(2,1)=-0.1D0    
      GG(2,2)=-0.1D0    
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.646511402796D+01     
      XEX(2)=0.223270864907D+01     
      XEX(3)=0.667397491303D+00     
      XEX(4)=0.595756422907D+00     
      XEX(5)=0.593267567811D+01     
      XEX(6)=0.552723456506D+01     
      XEX(7)=0.101332200907D+01     
      XEX(8)=0.400668229166D+00     
      FEX=0.395116343955D+01 
      RETURN    
2     CONTINUE
      EPS = 1.0D-3
      X(1)=DMAX1(EPS,X(1))
      X(2)=DMAX1(EPS,X(2))
      X(7)=DMAX1(EPS,X(7))
      X(8)=DMAX1(EPS,X(8))
      FX = 0.4D0*(X(1)**0.67D0/X(7)**0.67D0 + X(2)**0.67D0/X(8)**0.67D0)
     /     + 10.D0 - X(1) - X(2) 
      RETURN    
3     CONTINUE
      GF(1)=0.268D0*X(1)**(-0.33D0)*X(7)**(-0.67D0)-1.D0
      GF(2)=0.268D0*X(2)**(-0.33D0)*X(8)**(-0.67D0)-1.D0
      GF(7)=-0.268D0*X(1)**0.67D0*X(7)**(-1.67D0)       
      GF(8)=-0.268D0*X(2)**0.67D0*X(8)**(-1.67D0)       
      RETURN    
4     BX = 0.4D0*(X(1)**0.67D0/X(7)**0.67D0 + X(2)**0.67D0/X(8)**0.67D0)
     /     + 10.D0 - X(1) - X(2)  
      IF (INDEX1(1)) G(1)=-A*X(5)*X(7)-0.1D0*X(1)+1.D0  
      IF (INDEX1(2)) G(2)=-A*X(6)*X(8)-0.1D0*X(1)-0.1D0*X(2)+1.D0    
      IF (INDEX1(3)) G(3)=(-4.D0*X(3)-2.D0*X(3)**(-0.71D0))/X(5)
     /                    -A*DABS(X(3))**(-1.3D0)*X(7)+1.D0    
      IF (INDEX1(4)) G(4)=(-4.D0*X(4)-2.D0*DABS(X(4))**(-0.71D0))/X(6)
     /                    -A*DABS(X(4))**(-1.3D0)*X(8)+1.D0    
      IF (INDEX1(5)) G(5)=BX-1.D0       
      IF (INDEX1(6)) G(6)=4.2D0-BX      
      RETURN    
5     IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,5)=-A*X(7)   
      GG(1,7)=-A*X(5)   
7     IF (.NOT.INDEX2(2)) GOTO 8
      GG(2,6)=-A*X(8)   
      GG(2,8)=-A*X(6)   
8     IF(.NOT.INDEX2(3)) GOTO 9 
      V1=X(5)**2
      GG(3,3)=(-4.D0+1.42D0*DABS(X(3))**(-1.71D0))/X(5)+1.3D0*
     /         A*DABS(X(3))**(-2.3D0)*X(7)      
      GG(3,5)=(4.D0*X(3)+2.D0*DABS(X(3))**(-0.71D0))/V1 
      GG(3,7)=-A*DABS(X(3))**(-1.3D0)   
9     IF (.NOT.INDEX2(4)) GOTO 10       
      V2=X(6)**2
      GG(4,4)=(-4.D0+1.42D0*DABS(X(4))**(-1.71D0))/X(6)+1.3D0*
     /        A*DABS(X(4))**(-2.3D0)*X(8)      
      GG(4,6)=(4.D0*X(4)+2.D0*DABS(X(4))**(-0.71D0))/V2 
      GG(4,8)=-A*DABS(X(4))**(-1.3D0)   
10    IF (.NOT.INDEX2(5).AND..NOT.INDEX2(6)) GOTO 12    
      GF(1)=0.268D0*DABS(X(1))**(-0.33D0)*DABS(X(7))**(-0.67D0)-1.D0  
      GF(2)=0.268D0*DABS(X(2))**(-0.33D0)*DABS(X(8))**(-0.67D0)-1.D0  
      GF(7)=-0.268D0*DABS(X(1))**0.67D0*DABS(X(7))**(-1.67D0)   
      GF(8)=-0.268D0*DABS(X(2))**0.67D0*DABS(X(8))**(-1.67D0)   
      IF (.NOT.INDEX2(5)) GOTO 11       
      DO 31 I=1,8       
31    GG(5,I)=GF(I)     
11    IF (.NOT.INDEX2(6)) GOTO 12       
      DO 32 I=1,8       
32    GG(6,I)=-GF(I)    
12    RETURN    
      END       
