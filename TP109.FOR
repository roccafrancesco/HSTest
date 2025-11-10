      SUBROUTINE TP109(MODE)    
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
      COMMON     /D109/A,RA,B,C,HV1,V1,V2,V3,V4,V5,V6,V7,V8,V9,
     /                V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20    
      DOUBLEPRECISION A,RA,B,C,HV1,V1,V2,V3,V4,V5,V6,V7,V8,
     /                V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20  
      INTEGER I,J
      GOTO (1,2,3,4,5),MODE     
1     N=9       
      NILI=2    
      NINL=2    
      NELI=0    
      NENL=6    
      A=50.176D0
      RA=1.D0/A 
      B=DSIN(0.25D0)    
      C=DCOS(0.25D0)    
      DO 20 I=1,2       
      XL(I)=0.D0
      LXL(I)=.TRUE.     
      LXU(I)=.FALSE.    
      XL(I+2)=-0.55D0   
      XU(2+I)=0.55D0    
      XL(7+I)=-400.D0   
      XU(7+I)=800.D0    
      XL(4+I)=196.D0    
      XU(4+I)=252.D0    
20    CONTINUE  
      XL(7)=196.D0      
      XU(7)=252.D0      
      DO 21 I=3,9       
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
21    CONTINUE  
      DO 22 J=1,9       
      X(J)=0.D0 
      DO 22 I=1,10      
      GG(I,J)=0.D0      
22    CONTINUE  
      DO 30 I=3,9       
30    GF(I)=0.D0
      X(5)=250.D0
      X(6)=250.D0
      X(7)=200.D0
      GG(1,3)=-1.D0     
      GG(1,4)=1.D0      
      GG(2,3)=1.D0      
      GG(2,4)=-1.D0     
      GG(5,1)=-1.D0     
      GG(6,2)=-1.D0     
      GG(8,8)=1.D0      
      GG(9,9)=1.D0      
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.674888100445D+03     
      XEX(2)=0.113417039470D+04     
      XEX(3)=0.133569060261D+00     
      XEX(4)=-0.371152592466D+00     
      XEX(5)=0.252D+03     
      XEX(6)=0.252D+03     
      XEX(7)=0.201464535316D+03     
      XEX(8)=0.426660777226D+03     
      XEX(9)=0.368494083867D+03     
      FEX=0.536206927538D+04
      RETURN    
2     FX=3.D0*X(1)+1.D-6*X(1)**3+0.522074D-6*X(2)**3+2.D0*X(2) 
      RETURN    
3     GF(1)=3.D-6*X(1)**2+3.D0  
      GF(2)=2.D0+1.566222D-6*X(2)**2   
      RETURN    
4     IF (INDEX1(1)) G(1)=X(4)-X(3)+0.55D0      
      IF (INDEX1(2)) G(2)=X(3)-X(4)+0.55D0      
      IF (INDEX1(3)) G(3)=(2.25D+6-X(1)**2-X(8)**2)
      IF (INDEX1(4)) G(4)=(2.25D+6-X(2)**2-X(9)**2)
      IF (INDEX1(5)) G(5)=(X(5)*X(6)*DSIN(-X(3)-0.25D0)
     /   +X(5)*X(7)*DSIN(-X(4)-0.25D0)+2.D0*X(5)**2*B)*RA+400.D0-X(1)  
      IF (INDEX1(6)) G(6)=(X(5)*X(6)*DSIN(X(3)-0.25D0)+X(6)*X(7)
     /    *DSIN(X(3)-X(4)-0.25D0)+2.D0*X(6)**2*B)*RA+400.D0-X(2)  
      IF (INDEX1(7)) G(7)=(X(5)*X(7)*DSIN(X(4)-0.25D0)+X(6)*X(7)
     /    *DSIN(X(4)-X(3)-0.25D0)+2.D0*X(7)**2*B)*RA+881.779D0     
      IF (INDEX1(8)) G(8)=X(8)+(X(5)*X(6)*DCOS(-X(3)-0.25D0)+X(5)*X(7)
     /    *DCOS(-X(4)-0.25D0)-2.D0*X(5)**2*C)*RA+0.7533D-3*X(5)**2
     /    -200.0D0  
      IF (INDEX1(9)) G(9)=X(9)+(X(5)*X(6)*DCOS(X(3)-0.25D0)
     /     +X(7)*X(6)*DCOS(X(3)-X(4)-0.25D0)-2.D0*X(6)**2*C)*RA
     /     +0.7533D-3*X(6)**2-200.0D0
      IF (INDEX1(10)) G(10)=(X(5)*X(7)*DCOS(X(4)-0.25D0)+X(6)*X(7)
     /     *DCOS(X(4)-X(3)-0.25D0)-2.D0*X(7)**2*C)*RA+0.7533D-3*X(7)**2
     /     -22.938D0
      RETURN    
5     CONTINUE  
8     IF (.NOT.INDEX2(3)) GOTO 9
      GG(3,1)=-2.D0*X(1)
      GG(3,8)=-2.D0*X(8)
9     IF (.NOT.INDEX2(4)) GOTO 10       
      GG(4,2)=-2.D0*X(2)
      GG(4,9)=-2.D0*X(9)
10    IF (.NOT.INDEX2(5)) GOTO 11       
      V1=DSIN(-X(3)-0.25D0)     
      V2=DSIN(-X(4)-0.25D0)     
      V3=X(5)*RA
      GG(5,3)=-X(6)*V3*DCOS(-X(3)-0.25D0)       
      GG(5,4)=-X(7)*V3*DCOS(-X(4)-0.25D0)       
      GG(5,5)=(X(6)*V1+X(7)*V2+4.D0*X(5)*B)*RA  
      GG(5,6)=V3*V1     
      GG(5,7)=V3*V2     
11    IF (.NOT.INDEX2(6)) GOTO 12       
      HV1=X(3)-X(4)-0.25D0      
      V3=DCOS(HV1)      
      V4=DSIN(X(3)-0.25D0)      
      V5=X(6)*RA
      V6=DSIN(HV1)      
      GG(6,3)=X(5)*V5*DCOS(X(3)-0.25D0)+X(7)*V5*V3      
      GG(6,4)=-X(7)*V5*V3       
      GG(6,5)=V5*V4     
      GG(6,6)=(X(5)*V4+X(7)*V6)*RA+4.D0*V5*B    
      GG(6,7)=V5*V6     
12    IF (.NOT.INDEX2(7)) GOTO 13       
      HV1=X(4)-X(3)-0.25D0      
      V7=X(7)*RA
      V8=DCOS(HV1)      
      V9=DSIN(X(4)-0.25D0)      
      V10=DSIN(HV1)     
      GG(7,3)=-X(6)*V7*V8       
      GG(7,4)=X(5)*V7*DCOS(X(4)-0.25D0)+X(6)*V7*V8      
      GG(7,5)=V7*V9     
      GG(7,6)=V7*V10    
      GG(7,7)=(X(5)*V9+X(6)*V10)*RA+4.D0*V7*B   
13    IF (.NOT.INDEX2(8)) GOTO 14       
      V11=X(5)*RA       
      V12=DCOS(-X(3)-0.25D0)*RA 
      V13=DCOS(-X(4)-0.25D0)*RA 
      GG(8,3)=X(6)*V11*DSIN(-X(3)-0.25D0)       
      GG(8,4)=X(7)*V11*DSIN(-X(4)-0.25D0)       
      GG(8,5)=X(6)*V12+X(7)*V13-4.D0*V11*C+1.5066D-3*X(5)       
      GG(8,6)=X(5)*V12  
      GG(8,7)=X(5)*V13  
14    IF (.NOT.INDEX2(9)) GOTO 15       
      HV1=X(3)-X(4)-0.25D0      
      V14=DSIN(HV1)*X(6)*RA     
      V15=DCOS(X(3)-0.25D0)*RA  
      V16=DCOS(HV1)*RA  
      GG(9,3)=-X(5)*X(6)*DSIN(X(3)-0.25D0)*RA-X(7)*V14  
      GG(9,4)=X(7)*V14  
      GG(9,5)=X(6)*V15  
      GG(9,6)=X(5)*V15+X(7)*V16-4.D0*X(6)*C*RA+1.5066D-3*X(6)      
      GG(9,7)=X(6)*V16  
15    IF (.NOT.INDEX2(10)) GOTO 16      
      HV1=X(4)-X(3)-0.25D0      
      V17=DSIN(HV1)*X(6)*RA     
      V18=DCOS(X(4)-0.25D0)*RA  
      V19=DCOS(HV1)*RA  
      V20=X(7)*RA       
      GG(10,3)=X(7)*V17 
      GG(10,4)=-X(5)*V20*DSIN(X(4)-0.25D0)-X(7)*V17     
      GG(10,5)=X(7)*V18 
      GG(10,6)=X(7)*V19 
      GG(10,7)=X(5)*V18+X(6)*V19-4.D0*V20*C+1.5066D-3*X(7) 
16    RETURN    
      END       
