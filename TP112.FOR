      SUBROUTINE TP112(MODE)    
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
      COMMON     /D112/C(10)   
      DOUBLEPRECISION C,T,DLOGT,S   
      INTEGER I,J
      GOTO (1,2,2,4,5),MODE     
1     N=10      
      NILI=0    
      NINL=0    
      NELI=3    
      NENL=0    
      DO 6 I=1,10       
      XL(I)=1.D-4       
      LXL(I)=.TRUE.     
6     LXU(I)=.FALSE.    
      C(1)=-6.089D0     
      C(2)=-17.164D0    
      C(3)=-34.054D0    
      C(4)=-5.914D0     
      C(5)=-24.721D0    
      C(6)=-14.986D0    
      C(7)=-24.1D0      
      C(8)=-10.708D0    
      C(9)=-26.662D0    
      C(10)=-22.179D0   
      DO 20 J=1,10      
      X(J)=0.1D0
      DO 20 I=1,3       
20    GG(I,J)=0.D0      
      GG(1,1)=1.D0      
      GG(1,2)=2.D0      
      GG(1,3)=2.D0      
      GG(1,6)=1.D0      
      GG(1,10)=1.D0     
      GG(2,4)=1.D0      
      GG(2,5)=2.D0      
      GG(2,6)=1.D0      
      GG(2,7)=1.D0      
      GG(3,3)=1.D0      
      GG(3,7)=1.D0      
      GG(3,8)=1.D0      
      GG(3,9)=2.D0      
      GG(3,10)=1.D0     
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.177354776881D-01     
      XEX(2)=0.820018011109D-01     
      XEX(3)=0.882564558920D+00     
      XEX(4)=0.723325625629D-03     
      XEX(5)=0.490785079062D+00     
      XEX(6)=0.433546900325D-03     
      XEX(7)=0.172729773078D-01     
      XEX(8)=0.776563912291D-02     
      XEX(9)=0.198492864597D-01     
      XEX(10)=0.526982611793D-01     
      FEX=-0.47761086D+2  
      RETURN    
2     T=0.D0    
      DO 30 I=1,10      
30    T=T+X(I)  
      IF (MODE.EQ.3) GOTO 3     
      IF (T.LT.1.D-5) GOTO 34    
      DLOGT=DLOG(T)     
      S=0.D0    
      DO 31 I=1,10      
      IF (X(I).LT.0.D0) GOTO 34  
   31 S=S+X(I)*(C(I)+DLOG(X(I))-DLOGT)  
      FX=S
      RETURN    
   34 S=0.D0    
      DO 35 I=1,10      
   35 IF (X(I).LT.0.D0) S=S+(X(I)-5.D0)**2       
      FX=(S+1.D+3-47.8D0)
      RETURN    
    3 IF (T.LT.1.D-5) GOTO 36    
      DLOGT=DLOG(T)     
      DO 33 I=1,10      
      IF (X(I).LT.0.D0) GOTO 36  
   33 GF(I)=(C(I)+DLOG(X(I))-DLOGT)
      RETURN    
   36 DO 37 I=1,10      
      GF(I)=0.D0
   37 IF (X(I).LT.0.D0) GF(I)=2.D0*(X(I)-5.D0)
      RETURN    
    4 IF (INDEX1(1)) G(1)=X(1)+2.D0*X(2)+2.D0*X(3)+X(6)+X(10)-2.0   
      IF (INDEX1(2)) G(2)=X(4)+2.D0*X(5)+X(6)+X(7)-1.0 
      IF (INDEX1(3)) G(3)=X(3)+X(7)+X(8)+2.D0*X(9)+X(10)-1.0      
5     RETURN    
      END       
