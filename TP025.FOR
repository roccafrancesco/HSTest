      SUBROUTINE TP25(MODE)
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
      COMMON     /D25/ A(99),B(99),U(99),DA(99,3),V1,T,S,V2,V11,V22,X13
      DOUBLEPRECISION A,B,U,DA,V1,T,S,V2,V11,V22,X13    
      INTEGER I,J
      GOTO (1,2,3,4,4),MODE     
1     N=3       
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=0    
      X(1)=100.D0       
      X(2)=12.5D0       
      X(3)=3.D0 
      DO 6 I=1,3
      LXL(I)=.TRUE.     
6     LXU(I)=.TRUE.     
      XL(1)=0.1D0       
      XL(2)=1.D-5       
      XL(3)=1.D-5       
      XU(1)=100.D0      
      XU(2)=25.6D0      
      XU(3)=5.0D0
      LEX=.TRUE.
      NEX=1     
      XEX(1)=50.D0      
      XEX(2)=25.D0      
      XEX(3)=1.5D0      
      FEX=0.D0  
      RETURN    
  2   CONTINUE  
      DO 30 I=1,99      
      V1=2.D0/3.D0      
      U(I)=25.D0+(-50.D0*DLOG(0.01D0*DBLE(I)))**V1    
      V1=U(I)-X(2) 
      IF (V1.LT.0) GOTO 7       
      V11=-(V1**X(3))/X(1)
      B(I)=DEXP(V11)    
30    A(I)=B(I)-0.01D0*DBLE(I)
      T=0.D0    
      DO 31 I=1,99      
31    T=T+A(I)**2       
      FX=T
      RETURN    
7     S=0.D0    
      DO 8 I=1,3
8     S=S+(X(I)-5.D0)**2
      FX=S
      RETURN    
 3    CONTINUE  
      DO 36 I=1,99      
      V1=2.D0/3.D0      
      U(I)=25.D0-(50.D0*DLOG(0.01D0*DBLE(I)))**V1    
      V2=U(I)-X(2)      
      IF (V2.LE.0) GOTO 9       
      V22=-V2**X(3)/X(1)
      IF(V22.GT. -150.D0) GOTO42
      B(I)=0.D0 
      GOTO43    
   42 B(I)=DEXP(V22)    
   43 CONTINUE  
      A(I)=B(I)-0.01D0*DBLE(I)
      DA(I,1)=V2**X(3)/X(1)**2*B(I)     
      DA(I,2)=X(3)*V2**(X(3)-1.D0)/X(1)*B(I)    
36    DA(I,3)=-V2**X(3)/X(1)*DLOG(V2)*B(I)      
      DO 34 I=1,3       
      T=0.D0    
      DO 33 J=1,99      
33    T=T+2.D0*A(J)*DA(J,I)     
34    GF(I)=T   
      RETURN    
9     DO 10 I=1,3       
10    GF(I)=2.D0*(X(I)-5.D0)    
4     RETURN    
      END       
