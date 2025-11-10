      SUBROUTINE TP88(MODE)     
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
      DOUBLEPRECISION GLEICH,MUE,A,DCOSKO,RHO,DV,T,DZ,INTKO,PI,Z,  
     /           V1,V2,V3,U,EP1,A1,W    
      COMMON     /D88/ MUE(30),A(30),DCOSKO(30),RHO(30),DV(6),T(6),
     /                 DZ(6),INTKO,PI,Z,V1,V2,V3,U,EP1,A1,W     
      INTEGER I,J,KN1,N1
      KN1=1     
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.107431872940D+01     
      XEX(2)=-0.456613707247D+00     
      FEX=0.136265680997D+01 
      GOTO 7    
      ENTRY TP89(MODE)  
      KN1=2     
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.107431872754D+01     
      XEX(2)=-0.456613706239D+00     
      XEX(3)=0.300836097604D-10     
      FEX=0.136265680508D+01 
      GOTO 7    
      ENTRY TP90(MODE)  
      KN1=3     
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.708479399007D+00     
      XEX(2)=0.237919269592D-04     
      XEX(3)=0.807599939006D+00     
      XEX(4)=-0.456613723294D+00     
      FEX=0.136265681317D+01 
      GOTO 7    
      ENTRY TP91(MODE)  
      KN1=4     
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.701892928031D+00     
      XEX(2)=0.221084326516D-11     
      XEX(3)=0.813330836201D+00     
      XEX(4)=0.456613707134D+00     
      XEX(5)=0.899937588382D-11     
      FEX=0.136265680910D+01 
      GOTO 7    
      ENTRY TP92(MODE)  
      KN1=5     
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.494144465323D+00     
      XEX(2)=-0.103530473697D-04     
      XEX(3)=0.614950839550D+00     
      XEX(4)=-0.242186612731D-05     
      XEX(5)=0.729258528936D+00     
      XEX(6)=-0.456613099133D+00     
      FEX=0.136265681213D+01 
7     IF (MODE - 2) 1,17,17     
    1 N=KN1+1   
      NILI=0    
      NINL=1    
      NELI=0    
      NENL=0    
      DO 11 I=1,3       
      X(2*I-1)=0.5D0    
11    X(2*I)=-0.5D0     
      DO 6 I=1,6
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
      XL(I)=-10.D0      
6     XU(I)=10.D0
      XL(1)=0.1       
      PI=DATAN(1.D0)*4.D0       
      DO 10 I=1,30      
      Z=PI*DBLE(I-1)  
      MUE(I)=GLEICH(Z)  
      V1=DSIN(MUE(I))   
      V2=DCOS(MUE(I))   
      DCOSKO(I)=(V1/MUE(I)-V2)/MUE(I)**2
10    A(I)=2.D0*V1/(MUE(I)+V1*V2)       
      INTKO=2.D0/15.D0  
      RETURN    
17    IF(MODE - 4)  19,18,18    
18    N1=N-1    
      T(N)=X(N)**2      
      DO 8 I=1,N1       
8     T(N-I)=T(N-I+1)+X(N-I)**2 
      V1=0.D0   
      DO 13 J=1,30      
      V2=MUE(J) 
      V3=-V2**2 
      RHO(J)=DBLE((-1)**N) 
      DO 14 I=1,N1      
      EP1=0.D0  
      A1=V3*T(N+1-I)    
      IF(A1.GT.-100.D0) EP1=DEXP(A1)    
14    RHO(J)=RHO(J)+DBLE((-1)**(N-I))*2.D0*EP1     
      EP1=0.D0  
      A1=V3*T(1)
      IF(A1.GT.-100.D0) EP1=DEXP(A1)    
      RHO(J)=(RHO(J)+EP1)/V3    
13    V1=V1-V3*A(J)*RHO(J)*(V2*DSIN(V2)*RHO(J)-2.D0*DCOSKO(J))       
19    GOTO (1,2,3,4,5),MODE     
2     U=0.D0    
      DO 20 I=1,N       
20    U=U+X(I)**2       
      FX=U      
      RETURN    
3     DO 21 I=1,N       
21    GF(I)=2.D0*X(I)   
      RETURN    
4     G(1)=1.D-4-V1-INTKO       
      RETURN    
5     DO 22 I=1,N       
22    DV(I)=0.D0
      DO 25 J=1,30      
      W=MUE(J)  
      V1=W**2*A(J)*(W*DSIN(W)*RHO(J)-DCOSKO(J)) 
      EP1=0.D0  
      A1=-MUE(J)**2*T(1)
      IF(A1.GT.-100.D0) EP1=DEXP(A1)    
      DZ(1)=EP1 
      DV(1)=DV(1)+DZ(1)*V1      
      DO 23 I=2,N       
      EP1=0.D0  
      A1=-MUE(J)**2*T(I)
      IF(A1.GT.-100.D0) EP1=DEXP(A1)    
      DZ(I)=DZ(I-1)+DBLE((-1)**(I+1))*2.D0*EP1     
23    DV(I)=DV(I)+DZ(I)*V1      
25    CONTINUE  
      DO 24 I=1,N       
24    GG(1,I)=-4.D0*DV(I)*X(I)  
      RETURN    
      END       
