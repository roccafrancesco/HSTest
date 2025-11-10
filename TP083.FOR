      SUBROUTINE TP83(MODE)     
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
      COMMON/DATA83/A,B,C,D,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,
     /              V1,V2,V3
      DOUBLEPRECISION A,B,C,D,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,
     /              V1,V2,V3    
      GOTO (1,2,3,4,5),MODE     
1     N=5       
      NILI=0    
      NINL=6    
      NELI=0    
      NENL=0    
      X(1)=78.0D0
      X(2)=33.0D0
      X(3)=27.0D0
      X(4)=27.0D0
      X(5)=27.0D0
      DO 6 I=1,5
      LXL(I)=.TRUE.     
6     LXU(I)=.TRUE.     
      XL(1)=78.0D0       
      XL(2)=33.0D0       
      XU(1)=102.0D0      
      XU(2)=45.D0       
      DO 31 I=3,5       
      XL(I)=27.0D0       
31    XU(I)=45.0D0       
      A=5.3578547D0     
      B=0.8356891D0     
      C=37.293239D0     
      D=4.0792141D+4    
      A1=85.334407D0    
      A2=5.6858D-3      
      A3=6.262D-4       
      A4=2.2053D-3      
      A5=80.51249D0     
      A6=7.1317D-3      
      A7=2.9955D-3      
      A8=2.1813D-3      
      A9=9.300961D0     
      A10=4.7026D-3     
      A11=1.2547D-3     
      A12=1.9085D-3     
      GF(2)=0.D0
      GF(4)=0.D0
      GG(2,4)=0.D0      
      GG(3,2)=0.D0      
      GG(5,4)=0.D0      
      GG(6,2)=0.D0      
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.780000000000D+02     
      XEX(2)=0.330000000000D+02     
      XEX(3)=0.299952560253D+02     
      XEX(4)=0.450000000000D+02     
      XEX(5)=0.367758129081D+02     
      FEX=-0.306655386717D+05
      RETURN    
2     FX=A*X(3)**2+B*X(1)*X(5)+C*X(1)-D 
      RETURN    
3     GF(1)=B*X(5)+C    
      GF(3)=2.D0*A*X(3) 
      GF(5)=B*X(1) 
      RETURN    
4     IF (.NOT.(INDEX1(1).OR.INDEX1(4))) GOTO 41
      V1=A1+A2*X(2)*X(5)+A3*X(1)*X(4)-A4*X(3)*X(5)      
      IF (INDEX1(1)) G(1)=V1    
      IF (INDEX1(4)) G(4)=92.D0-V1      
41    IF (.NOT.(INDEX1(2).OR.INDEX1(5))) GOTO 42
      V2=A5+A6*X(2)*X(5)+A7*X(1)*X(2)+A8*X(3)**2-90.D0  
      IF (INDEX1(2)) G(2)=V2    
      IF (INDEX1(5)) G(5)=20.D0-V2      
42    IF (.NOT.(INDEX1(3).OR.INDEX1(6))) GOTO 43
      V3=A9+A10*X(3)*X(5)+A11*X(1)*X(3)+A12*X(3)*X(4)-20.D0      
      IF (INDEX1(3)) G(3)=V3    
      IF (INDEX1(6)) G(6)=5.0D0-V3       
43    RETURN    
5     IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,1)=A3*X(4)   
      GG(1,2)=A2*X(5)   
      GG(1,3)=-A4*X(5)  
      GG(1,4)=A3*X(1)   
      GG(1,5)=A2*X(2)-A4*X(3)   
7     IF (.NOT.INDEX2(2)) GOTO 8
      GG(2,1)=A7*X(2)   
      GG(2,2)=A6*X(5)+A7*X(1)   
      GG(2,3)=2.D0*A8*X(3)      
      GG(2,5)=A6*X(2)   
8     IF(.NOT.INDEX2(3)) GOTO 9 
      GG(3,1)=A11*X(3)  
      GG(3,3)=A10*X(5)+A11*X(1)+A12*X(4)
      GG(3,4)=A12*X(3)  
      GG(3,5)=A10*X(3)  
9     IF (.NOT.INDEX2(4)) GOTO 10       
      GG(4,1)=-A3*X(4)  
      GG(4,2)=-A2*X(5)  
      GG(4,3)=A4*X(5)   
      GG(4,4)=-A3*X(1)  
      GG(4,5)=-A2*X(2)+A4*X(3)  
10    IF (.NOT.INDEX2(5)) GOTO 11       
      GG(5,1)=-A7*X(2)  
      GG(5,2)=-A6*X(5)-A7*X(1)  
      GG(5,3)=-2.D0*A8*X(3)     
      GG(5,5)=-A6*X(2)  
11    IF (.NOT.INDEX2(6)) GOTO 12       
      GG(6,1)=-A11*X(3) 
      GG(6,3)=-A10*X(5)-A11*X(1)-A12*X(4)       
      GG(6,4)=-A12*X(3) 
      GG(6,5)=-A10*X(3) 
12    RETURN    
      END       
