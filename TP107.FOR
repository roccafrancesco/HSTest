      SUBROUTINE TP107(MODE)    
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
      COMMON     /D107/V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,
     /                 V15,A,B,C,D,Y1,Y2,Y3,Y4,Y5,Y6  
      DOUBLEPRECISION  V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,
     /                 V15,A,B,C,D,Y1,Y2,Y3,Y4,Y5,Y6  
      INTEGER I,J
      GOTO (1,2,3,4,4),MODE     
1     N=9       
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=6    
      X(1)=0.8D0
      X(2)=0.8D0
      X(3)=0.2D0
      X(4)=0.2D0
      X(5)=1.0454D0     
      X(6)=1.0454D0     
      X(7)=1.0454D0     
      X(8)=0.D0 
      X(9)=0.D0 
      V1=48.4D0/50.176D0
      C=V1*DSIN(0.25D0) 
      D=V1*DCOS(0.25D0) 
      DO 20 I=1,6       
      DO 20 J=1,9       
20    GG(I,J)=0.D0      
      DO 21 I=1,2       
      LXL(I)=.TRUE.     
      LXU(I)=.FALSE.    
21    XL(I)=0.D0
      DO 22 I=3,4       
      LXL(I)=.FALSE.    
22    LXU(I)=.FALSE.    
      DO 23 I=5,7       
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
      XL(I)=0.90909D0   
23    XU(I)=1.0909D0    
      DO 24 I=8,9       
      LXL(I)=.FALSE.    
24    LXU(I)=.FALSE.    
      DO 25 I=3,9       
25    GF(I)=0.D0
      GG(1,1)=-1.D0     
      GG(2,2)=-1.D0     
      GG(4,3)=-1.D0     
      GG(5,4)=-1.D0     
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.667009506909D+00     
      XEX(2)=0.102238816675D+01     
      XEX(3)=0.228287932605D+00     
      XEX(4)=0.184821729352D+00     
      XEX(5)=0.10909D+01     
      XEX(6)=0.10909D+01     
      XEX(7)=0.106903593236D+01     
      XEX(8)=0.106612642267D+00     
      XEX(9)=-0.338786658776D+00     
      FEX=0.505501180339D+04
      RETURN    
2     FX=3.D+3*X(1)+1.D+3*X(1)**3+2.D+3*X(2)+666.66666667D0*X(2)**3
      RETURN    
3     GF(1)=3.D+3+3.D+3*X(1)**2 
      GF(2)=2.D+3+2.000001D+3*X(2)**2   
      RETURN    
 4    Y1=DSIN(X(8))     
      Y2=DCOS(X(8))     
      Y3=DSIN(X(9))     
      Y4=DCOS(X(9))     
      Y5=DSIN(X(8)-X(9))
      Y6=DCOS(X(8)-X(9))
      IF (MODE.EQ.5) GOTO 5     
      IF (INDEX1(1)) G(1)=0.4D0-X(1)+2.D0*C*X(5)**2+X(5)*X(6)
     /    *(-D*Y1-C*Y2)+X(5)*X(7)*(-D*Y3-C*Y4)    
      IF (INDEX1(2)) G(2)=0.4D0-X(2)+2.D0*C*X(6)**2+X(5)*X(6)
     /    *(D*Y1-C*Y2)+X(6)*X(7)*(D*Y5-C*Y6)     
      IF (INDEX1(3)) G(3)=0.8D0+2.D0*C*X(7)**2+X(5)*X(7)
     /    *(D*Y3-C*Y4)+X(6)*X(7)*(-D*Y5-C*Y6)   
      IF (INDEX1(4)) G(4)=0.2D0-X(3)+2.D0*D*X(5)**2-X(5)
     /    *X(6)*(-C*Y1+D*Y2)-X(5)*X(7)*(-C*Y3+D*Y4)     
      IF (INDEX1(5)) G(5)=0.2D0-X(4)+2.D0*D*X(6)**2
     /    -X(5)*X(6)*(C*Y1+D*Y2)-X(6)*X(7)*(C*Y5+D*Y6)       
      IF (INDEX1(6)) G(6)=-0.337D0+2.D0*D*X(7)**2-X(5)*X(7)     
     /    *(C*Y3+D*Y4)-X(6)*X(7)*(-C*Y5+D*Y6)
      RETURN    
5     IF (.NOT.INDEX2(1)) GOTO 7
      V1=-D*Y1-C*Y2     
      V2=-D*Y3-C*Y4     
      GG(1,5)=4.D0*C*X(5)+X(6)*V1+X(7)*V2       
      GG(1,6)=X(5)*V1   
      GG(1,7)=X(5)*V2   
      GG(1,8)=X(5)*X(6)*(-D*Y2+C*Y1)    
      GG(1,9)=X(5)*X(7)*(-D*Y4+C*Y3)    
7     IF (.NOT.INDEX2(2)) GOTO 8
      V2=D*Y1-C*Y2      
      V3=D*Y6+C*Y5      
      V4=D*Y5-C*Y6      
      GG(2,5)=X(6)*V2   
      GG(2,6)=4.D0*C*X(6)+X(5)*V2+X(7)*V4       
      GG(2,7)=X(6)*V4   
      GG(2,8)=X(5)*X(6)*(D*Y2+C*Y1)+X(6)*X(7)*V3
      GG(2,9)=-X(6)*X(7)*V3     
8     IF(.NOT.INDEX2(3)) GOTO 9 
      V5=D*Y3-C*Y4      
      V6=-D*Y5-C*Y6     
      V7=-D*Y6+C*Y5     
      GG(3,5)=X(7)*V5   
      GG(3,6)=X(7)*V6   
      GG(3,7)=4.D0*C*X(7)+X(5)*V5+X(6)*V6       
      GG(3,8)=X(6)*X(7)*V7      
      GG(3,9)=X(5)*X(7)*(D*Y4+C*Y3)-X(6)*X(7)*V7
9     IF (.NOT.INDEX2(4)) GOTO 10       
      V8=-C*Y1+D*Y2     
      V9=-C*Y3+D*Y4     
      GG(4,5)=4.D0*D*X(5)-X(6)*V8-X(7)*V9       
      GG(4,6)=-X(5)*V8  
      GG(4,7)=-X(5)*V9  
      GG(4,8)=X(5)*X(6)*(C*Y2+D*Y1)     
      GG(4,9)=X(5)*X(7)*(C*Y4+D*Y3)     
10    IF (.NOT.INDEX2(5)) GOTO 11       
      V10=C*Y1+D*Y2     
      V11=C*Y5+D*Y6     
      V12=(C*Y6-D*Y5)*X(6)      
      GG(5,5)=-X(6)*V10 
      GG(5,6)=4.D0*D*X(6)-X(5)*V10-X(7)*V11     
      GG(5,7)=-X(6)*V11 
      GG(5,8)=-X(5)*X(6)*(C*Y2-D*Y1)-X(7)*V12   
      GG(5,9)=X(7)*V12  
11    IF (.NOT.INDEX2(6)) GOTO 12       
      V13=C*Y3+D*Y4     
      V14=-C*Y5+D*Y6    
      V15=(C*Y6+D*Y5)*X(6)*X(7) 
      GG(6,5)=-X(7)*V13 
      GG(6,6)=-X(7)*V14 
      GG(6,7)=4.D0*D*X(7)-X(5)*V13-X(6)*V14     
      GG(6,8)=V15       
      GG(6,9)=-X(5)*X(7)*(C*Y4-D*Y3)-V15
12    RETURN    
      END       
