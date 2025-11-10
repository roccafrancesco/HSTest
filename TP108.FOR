      SUBROUTINE TP108(MODE)    
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
1     N=9       
      NILI=0    
      NINL=13   
      NELI=0    
      NENL=0    
      DO 21 J=1,9       
21    X(J)=1.D0 
      DO 22 I=1,9 
      XL(I)=0.0D0      
      LXL(I)=.TRUE.
C      LXL(I)=.FALSE.
      XU(I)=1.0D0    
22    LXU(I)=.TRUE.    
      LXL(9)=.TRUE.     
      XL(9)=0.D0
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.884129216724D+00     
      XEX(2)=0.467242472598D+00     
      XEX(3)=0.374207573677D-01     
      XEX(4)=0.999299598210D+00     
      XEX(5)=0.884129216724D+00     
      XEX(6)=0.467242472594D+00     
      XEX(7)=0.374207573618D-01     
      XEX(8)=0.999299598210D+00     
      XEX(9)=0.261984643608D-19     
      FEX=-0.866025403841D+00 
      RETURN    
2     FX=-0.5D0*(X(1)*X(4)-X(2)*X(3)+X(3)*X(9)-X(5)*X(9)+       
     -X(5)*X(8)-X(6)*   
     -X(7))     
      RETURN    
3     GF(1)=-0.5D0*X(4) 
      GF(2)=0.5D0*X(3)  
      GF(3)=0.5D0*(X(2)-X(9))   
      GF(4)=-0.5D0*X(1) 
      GF(5)=0.5D0*(X(9)-X(8))   
      GF(6)=0.5D0*X(7)  
      GF(7)=0.5D0*X(6)  
      GF(8)=-0.5D0*X(5) 
      GF(9)=-GF(8)-GF(2)
      RETURN    
4     IF (INDEX1(1)) G(1)=1.D0-X(3)**2-X(4)**2  
      IF (INDEX1(2)) G(2)=1.D0-X(9)**2  
      IF (INDEX1(3)) G(3)=1.D0-X(5)**2-X(6)**2  
      IF (INDEX1(4)) G(4)=1.D0-X(1)**2-(X(2)-X(9))**2   
      IF (INDEX1(5)) G(5)=1.D0-(X(1)-X(5))**2-(X(2)-X(6))**2       
      IF (INDEX1(6)) G(6)=1.D0-(X(1)-X(7))**2-(X(2)-X(8))**2       
      IF (INDEX1(7)) G(7)=1.D0-(X(3)-X(5))**2-(X(4)-X(6))**2       
      IF (INDEX1(8)) G(8)=1.D0-(X(3)-X(7))**2-(X(4)-X(8))**2       
      IF (INDEX1(9)) G(9)=1.D0-X(7)**2-(X(8)-X(9))**2   
      IF (INDEX1(10)) G(10)=X(1)*X(4)-X(2)*X(3) 
      IF (INDEX1(11))G(11)=X(3)*X(9)    
      IF (INDEX1(12)) G(12)=-X(5)*X(9)  
      IF (INDEX1(13)) G(13)=X(5)*X(8)-X(6)*X(7) 
      RETURN    
5     DO 23 I=1,13      
      DO 23 J=1,9       
23    GG(I,J)=0.D0      
      IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,3)=-2.D0*X(3)
      GG(1,4)=-2.D0*X(4)
7     IF (.NOT.INDEX2(2)) GOTO 8
      GG(2,9)=-2.D0*X(9)
8     IF(.NOT.INDEX2(3)) GOTO 9 
      GG(3,5)=-2.D0*X(5)
      GG(3,6)=-2.D0*X(6)
9     IF (.NOT.INDEX2(4)) GOTO 10       
      GG(4,1)=-2.D0*X(1)
      GG(4,2)=-2.D0*(X(2)-X(9)) 
      GG(4,9)=-GG(4,2)  
10    IF (.NOT.INDEX2(5)) GOTO 11       
      GG(5,1)=-2.D0*(X(1)-X(5)) 
      GG(5,2)=-2.D0*(X(2)-X(6)) 
      GG(5,5)=-GG(5,1)  
      GG(5,6)=-GG(5,2)  
11    IF (.NOT.INDEX2(6)) GOTO 12       
      GG(6,1)=-2.D0*(X(1)-X(7)) 
      GG(6,2)=-2.D0*(X(2)-X(8)) 
      GG(6,7)=-GG(6,1)  
      GG(6,8)=-GG(6,2)  
12    IF (.NOT.INDEX2(7)) GOTO 13       
      GG(7,3)=-2.D0*(X(3)-X(5)) 
      GG(7,4)=-2.D0*(X(4)-X(6)) 
      GG(7,5)=-GG(7,3)  
      GG(7,6)=-GG(7,4)  
13    IF (.NOT.INDEX2(8)) GOTO 14       
      GG(8,3)=-2.D0*(X(3)-X(7)) 
      GG(8,4)=-2.D0*(X(4)-X(8)) 
      GG(8,7)=-GG(8,3)  
      GG(8,8)=-GG(8,4)  
14    IF (.NOT.INDEX2(9)) GOTO 15       
      GG(9,7)=-2.D0*X(7)
      GG(9,8)=-2.D0*(X(8)-X(9)) 
      GG(9,9)=-GG(9,8)  
15    IF (.NOT.INDEX2(10)) GOTO 16      
      GG(10,1)=X(4)     
      GG(10,2)=-X(3)    
      GG(10,3)=-X(2)    
      GG(10,4)=X(1)     
16    IF (.NOT.INDEX2(11)) GOTO 17      
      GG(11,3)=X(9)     
      GG(11,9)=X(3)     
17    IF (.NOT.INDEX2(12)) GOTO 18      
      GG(12,5)=-X(9)    
      GG(12,9)=-X(5)    
18    IF (.NOT.INDEX2(13)) GOTO 19      
      GG(13,5)=X(8)     
      GG(13,6)=-X(7)    
      GG(13,7)=-X(6)    
      GG(13,8)=X(5)     
19    RETURN    
      END       
