      SUBROUTINE TP87(MODE)     
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
      DOUBLEPRECISION A,B,C,D,E,F1,F2,V1,V2,V3,V4,V5   
      A=131.078D0       
      B=1.48477D0       
      C=0.90798D0       
      D=DCOS(1.47588D0) 
      E=DSIN(1.47588D0) 
      GOTO (1,2,3,4,5),MODE     
1     N=6       
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=4    
      X(1)=390.D0       
      X(2)=1.D+3
      X(3)=419.5D0      
      X(4)=340.5D0      
      X(5)=198.175D0    
      X(6)=0.5D0
      DO 6 I=1,6
      LXL(I)=.TRUE.     
6     LXU(I)=.TRUE.     
      XL(1)=0.D0
      XL(2)=0.D0
      XL(3)=340.D0      
      XL(4)=340.D0      
      XL(5)=-1.D+3      
      XL(6)=0.D0
      XU(1)=400.D0      
      XU(2)=1.D+3       
      XU(3)=420.D0      
      XU(4)=420.D0      
      XU(5)=1.D+3       
      XU(6)=0.5236D0    
      DO 70 I=3,6       
70    GF(I)=0.D0
      GG(1,1)=-1.D0     
      GG(1,2)=0.D0      
      GG(1,5)=0.D0      
      GG(2,1)=0.D0      
      GG(2,2)=-1.D0     
      GG(2,5)=0.D0      
      GG(3,1)=0.D0      
      GG(3,2)=0.D0      
      GG(3,5)=-1.D0     
      GG(4,1)=0.D0      
      GG(4,2)=0.D0      
      GG(4,5)=0.D0      
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.107811937779D+03     
      XEX(2)=0.196318606955D+03     
      XEX(3)=0.373830728516D+03     
      XEX(4)=0.420000000000D+03     
      XEX(5)=0.213071293896D+02     
      XEX(6)=0.153291953422D+00     
      FEX=0.892759773493D+04 
      RETURN    
2     IF (X(1)-300.D0) 31,32,32 
31    F1=30.D0*X(1)     
      GOTO 33   
32    F1=31.D0*X(1)     
33    IF (X(2)-100.D0) 34,35,35 
34    F2=28.D0*X(2)     
      GOTO 46   
35    IF (X(2)-200.D0) 36,37,37 
36    F2=29.D0*X(2)     
      GOTO 46   
37    F2=30.D0*X(2)     
46    FX=F1+F2  
      RETURN    
3     IF (X(1)-300.D0) 38,39,39 
38    GF(1)=30.D0       
      GOTO 40   
39    GF(1)=31.D0       
40    IF (X(2)-100.D0) 41,42,42 
41    GF(2)=28.D0       
      GOTO 45   
42    IF(X(2)-200.D0) 43,44,44  
43    GF(2)=29.D0       
      GOTO 45   
44    GF(2)=30.D0       
45    RETURN    
4     IF (INDEX1(1)) G(1)=-X(1)+300.D0-X(3)*X(4)/A*
     /     DCOS(B-X(6))+C*X(3)**2/A*D      
      IF (INDEX1(2)) G(2)=-X(2)-X(3)*X(4)/A*DCOS(B+X(6))
     /     +C*X(4)**2/A*D    
      IF (INDEX1(3)) G(3)=-X(5)-X(3)*X(4)/A*DSIN(B+X(6))
     /     +C*X(4)**2/A*E    
      IF (INDEX1(4)) G(4)=200.D0-X(3)*X(4)/A*DSIN(B-X(6))+      
     /     C*X(3)**2/A*E     
      RETURN    
5     V1=1.D0/A 
      IF (.NOT.(INDEX2(1).OR.INDEX2(4))) GOTO 8 
      V2=B-X(6) 
      V3=DCOS(V2)*V1    
      V4=DSIN(V2)*V1    
      V5=2.D0*C*X(3)*V1 
      IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,3)=-X(4)*V3+V5*D     
      GG(1,4)=-X(3)*V3  
      GG(1,6)=-X(3)*X(4)*V4     
7     IF (.NOT.INDEX2(4)) GOTO 8
      GG(4,3)=-X(4)*V4+V5*E     
      GG(4,4)=-X(3)*V4  
      GG(4,6)=X(3)*X(4)*V3      
8     IF (.NOT.(INDEX2(2).OR.INDEX2(3))) GOTO 10
      V2=B+X(6) 
      V3=DCOS(V2)*V1    
      V4=DSIN(V2)*V1    
      V5=2.D0*C*X(4)*V1 
      IF (.NOT.INDEX2(2)) GOTO 9
      GG(2,3)=-X(4)*V3  
      GG(2,4)=-X(3)*V3+V5*D     
      GG(2,6)=X(3)*X(4)*V4      
9     IF (.NOT.INDEX2(3)) GOTO 10       
      GG(3,3)=-X(4)*V4  
      GG(3,4)=-X(3)*V4+V5*E     
      GG(3,6)=-X(3)*X(4)*V3     
10    RETURN    
      END       
