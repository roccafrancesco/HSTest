      SUBROUTINE TP116(MODE)    
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
1     N=13      
      NILI=5    
      NINL=10   
      NELI=0    
      NENL=0    
      X(1)=0.5D0
      X(2)=0.8D0
      X(3)=0.9D0
      X(4)=0.1D0
      X(5)=0.14D0       
      X(6)=0.5D0
      X(7)=489.D0       
      X(8)=80.D0
      X(9)=650.D0       
      X(10)=450.D0      
      X(11)=150.D0      
      X(12)=150.D0      
      X(13)=150.D0      
      DO 6 I=1,10       
6     XL(I)=0.1D0       
      XL(4)=1.D-4       
      XL(9)=500.D0      
      XL(11)=1.D0       
      XL(12)=1.D-4      
      XL(13)=1.D-4      
      DO 7 I=1,3
      XU(I)=1.D0
      XU(I+6)=1.D+3     
7     XU(I+10)=150.D0   
      XU(4)=0.1D0       
      XU(5)=0.9D0       
      XU(6)=0.9D0       
      XU(10)=500.D0     
      DO 32 I=1,13      
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
      DO 32 J=1,15      
32    GG(J,I)=0.D0      
      DO 30 I=1,10      
30    GF(I)=0.D0
      DO 31 I=11,13     
31    GF(I)=1.D0
      GG(1,2)=-1.D0     
      GG(1,3)=1.D0      
      GG(2,1)=-1.D0     
      GG(2,2)=1.D0      
      GG(3,7)=-2.D-3    
      GG(3,8)=2.D-3     
      GG(4,11)=1.D0     
      GG(4,12)=1.D0     
      GG(4,13)=1.D0     
      GG(5,11)=-1.D0    
      GG(5,12)=-1.D0    
      GG(5,13)=-1.D0    
      GG(6,13)=1.D0     
      GG(14,11)=1.D0    
      GG(15,12)=1.D0    
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.803770278595D+00     
      XEX(2)=0.899986033670D+00     
      XEX(3)=0.970972419495D+00     
      XEX(4)=0.999995162129D-01     
      XEX(5)=0.190815447786D+00     
      XEX(6)=0.460571745738D+00     
      XEX(7)=0.574080310673D+03     
      XEX(8)=0.740804261398D+02     
      XEX(9)=0.500016155317D+03     
      XEX(10)=0.999999999985D-01     
      XEX(11)=0.202341325935D+02     
      XEX(12)=0.773475459898D+02     
      XEX(13)=0.673039736648D-02     
      FEX=0.975884089805D+02 
      RETURN    
2     FX=X(11)+X(12)+X(13)      
3     RETURN    
4     IF (INDEX1(1)) G(1)=X(3)-X(2)     
      IF (INDEX1(2)) G(2)=X(2)-X(1)     
      IF (INDEX1(3)) G(3)=1.D0-2.D-3*(X(7)-X(8))
      IF (INDEX1(4)) G(4)=X(11)+X(12)+X(13)-50.0D0      
      IF (INDEX1(5)) G(5)=250.D0-X(11)-X(12)-X(13)      
      IF (INDEX1(6)) G(6)=X(13)-1.262626D0*X(10)+1.231059D0     
     /     *X(3)*X(10)       
      IF (INDEX1(7)) G(7)=X(5)-0.03475D0*X(2)-0.975D0*X(2)*     
     /     X(5)+9.75D-3*X(2)**2       
      IF (INDEX1(8)) G(8)=X(6)-0.03475D0*X(3)-0.975D0*X(3)
     /     *X(6)+9.75D-3*X(3)**2       
      IF (INDEX1(9)) G(9)=X(5)*X(7)-X(1)*X(8)-X(4)*X(7) 
     /     +X(4)*X(8)
      IF (INDEX1(10)) G(10)=-2.D-3*(X(2)*X(9)+X(5)*X(8) 
     /     -X(1)*X(8)-X(6)*X(9))-X(6)-X(5)+1.D0      
      IF (INDEX1(11)) G(11)=X(2)*X(9)-X(3)*X(10)-X(6)*X(9)
     /     -500.D0*(X(2)-X(6))+X(2)*X(10)    
      IF (INDEX1(12)) G(12)=X(2)-0.9D0-2.D-3*(X(2)*X(10)-       
     /      X(3)*X(10))       
      IF (INDEX1(13)) G(13)=X(4)-0.03475D0*X(1)-0.975D0*X(1)*X(4)
     /    +9.75D-3*X(1)**2   
      IF(INDEX1(14)) G(14)=X(11)-1.262626D0*X(8)+1.231059D0*X(1)*X(8)
      IF (INDEX1(15)) G(15)=X(12)-1.262626D0*X(9)+1.231059D0*X(2)*X(9)
      RETURN    
5     IF (.NOT.INDEX2(6)) GOTO 12       
      GG(6,3)=1.231059D0*X(10)  
      GG(6,10)=-1.262626D0+1.231059D0*X(3)      
12    IF (.NOT.INDEX2(7)) GOTO 13       
      GG(7,2)=-0.03475D0-0.975D0*X(5)+1.95D-2*X(2)      
      GG(7,5)=1.D0-0.975D0*X(2) 
13    IF (.NOT.INDEX2(8)) GOTO 14       
      GG(8,3)=-0.03475D0-0.975D0*X(6)+1.95D-2*X(3)      
      GG(8,6)=1.D0-0.975D0*X(3) 
14    IF (.NOT.INDEX2(9)) GOTO 15       
      GG(9,1)=-X(8)     
      GG(9,4)=-X(7)+X(8)
      GG(9,5)=X(7)      
      GG(9,7)=X(5)-X(4) 
      GG(9,8)=-X(1)+X(4)
15    IF (.NOT.INDEX2(10)) GOTO 16      
      GG(10,1)=2.D-3*X(8)       
      GG(10,2)=-2.D-3*X(9)      
      GG(10,5)=-2.D-3*X(8)-1.D0 
      GG(10,6)=-1.D0+2.D-3*X(9) 
      GG(10,8)=-2.D-3*(X(5)-X(1))       
      GG(10,9)=-2.D-3*(X(2)-X(6))       
16    IF (.NOT.INDEX2(11)) GOTO 17      
      GG(11,2)=X(9)-500.D0+X(10)
      GG(11,3)=-X(10)   
      GG(11,6)=-X(9)+500.D0     
      GG(11,9)=X(2)-X(6)
      GG(11,10)=-X(3)+X(2)      
17    IF (.NOT.INDEX2(12)) GOTO 18      
      GG(12,2)=1.D0-2.D-3*X(10) 
      GG(12,3)=2.D-3*X(10)      
      GG(12,10)=2.D-3*(X(3)-X(2))       
18    IF (.NOT.INDEX2(13)) GOTO 19      
      GG(13,1)=-0.03475D0-0.975D0*X(4)+1.95D-2*X(1)     
      GG(13,4)=1.D0-0.975D0*X(1)
19    IF (.NOT.INDEX2(14)) GOTO 20      
      GG(14,1)=1.231059D0*X(8)  
      GG(14,8)=1.231059D0*X(1)-1.262626D0       
20    IF (.NOT.INDEX2(15)) GOTO 21      
      GG(15,2)=1.231059D0*X(9)  
      GG(15,9)=-1.262626D0+1.231059D0*X(2)      
21    RETURN    
      END       
