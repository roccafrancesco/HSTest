      SUBROUTINE TP106(MODE)    
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
1     N=8       
      NILI=3    
      NINL=3    
      NELI=0    
      NENL=0    
      DO 23 I=1,3       
      X(I)=5.D+3
      XL(I)=1.D+3       
23    XU(I)=1.D+4       
      XL(1)=100.D0      
      DO 24 I=4,8       
      XL(I)=10.D0       
24    XU(I)=1.D+3       
      X(4)=200.D0       
      X(5)=350.D0       
      X(6)=150.D0       
      X(7)=225.D0       
      X(8)=425.D0       
      DO 6 I=1,8
      LXL(I)=.TRUE.     
6     LXU(I)=.TRUE.     
      DO 22 I=1,6       
      DO 22 J=1,8       
22    GG(I,J)=0.D0      
      GF(1)=1.D0
      GF(2)=1.D0
      GF(3)=1.D0
      DO 20 I=4,8       
20    GF(I)=0.D0
      GG(1,4)=-2.5D-3   
      GG(1,6)=-2.5D-3   
      GG(2,5)=-2.5D-3   
      GG(2,7)=-2.5D-3   
      GG(2,4)=2.5D-3    
      GG(3,5)=0.01D0    
      GG(3,8)=-0.01D0   
      GG(4,4)=-833.33252D0      
      GG(5,5)=-1.25D+3
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.57930668D+03     
      XEX(2)=0.13599707D+04     
      XEX(3)=0.51099707D+04      
      XEX(4)=0.18201770D+03     
      XEX(5)=0.29560117D+03     
      XEX(6)=0.21798230D+03      
      XEX(7)=0.28641653D+03      
      XEX(8)=0.39560117D+03     
C      FEX=0.704933092308D+04 
      FEX=0.70492480D+04
      RETURN    
2     FX=X(1)+X(2)+X(3) 
3     RETURN    
4     IF (INDEX1(1)) G(1)=-2.5D-3*(X(4)+X(6))+1.D0      
      IF (INDEX1(2)) G(2)=-2.5D-3*(X(5)+X(7)-X(4))+1.D0 
      IF (INDEX1(3)) G(3)=-0.01D0*(X(8)-X(5))+1.D0      
      IF (INDEX1(4)) G(4)=(-833.33252D0*X(4)-100.D0*X(1)+8.3333333D+4 
     -                         +X(1)*X(6))
      IF (INDEX1(5)) G(5)=(-1.25D+3*X(5)-X(2)*X(4)+1.25D+3       
     -                        *X(4)+X(2)*X(7))
      IF (INDEX1(6)) G(6)=(-1.25D+6-X(3)*X(5)+2.5D+3*X(5)+       
     -                         X(3)*X(8))
      RETURN    
5     IF (.NOT.INDEX2(4)) GOTO 10       
      GG(4,1)=(-100.D0+X(6))
      GG(4,6)=X(1)
10    IF (.NOT.INDEX2(5)) GOTO 11       
      GG(5,2)=(-X(4)+X(7))
      GG(5,4)=(-X(2)+1.25D+3)
      GG(5,7)=X(2)      
11    IF (.NOT.INDEX2(6)) GOTO 12      
      GG(6,3)=(-X(5)+X(8))
      GG(6,5)=(-X(3)+2.5D+3)
      GG(6,8)=X(3)
12    RETURN    
      END       
