      SUBROUTINE TP111(MODE)    
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
      COMMON     /D111/C(10)   
      DOUBLEPRECISION  C,T,S    
      INTEGER I,J
      GOTO (1,2,2,4,5),MODE     
1     N=10      
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=3    
      DO 20 J=1,10      
      X(J)=-2.3D0       
      DO 20 I=1,3       
20    GG(I,J)=0.D0      
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
      DO 6 I=1,10       
      XL(I)=-100.D0     
      XU(I)=100.D0      
      LXL(I)=.TRUE.     
6     LXU(I)=.TRUE.     
      LEX=.FALSE.       
      NEX=1
      XEX(1)=-0.320121253241D+01     
      XEX(2)=-0.191205959435D+01     
      XEX(3)=-0.244441308369D+00     
      XEX(4)=-0.653748856532D+01     
      XEX(5)=-0.723152425984D+00     
      XEX(6)=-0.726773826993D+01     
      XEX(7)=-0.359671064233D+01     
      XEX(8)=-0.401776873216D+01     
      XEX(9)=-0.328746169619D+01     
      XEX(10)=-0.233558183059D+01     
      FEX=-0.477610902637D+02 
      RETURN    
 2    T=0.D0    
      DO 30 I=1,10      
30    T=T+DEXP(X(I))    
      IF (MODE.EQ.3) GOTO 3     
      S=0.D0    
      DO 31 I=1,10      
31    S=S+DEXP(X(I))*(C(I)+X(I)-DLOG(T))
      FX=S      
      RETURN    
3     DO 33 I=1,10      
33    GF(I)=DEXP(X(I))*(C(I)+X(I)-DLOG(T))      
      RETURN    
4     IF (INDEX1(1)) G(1)=DEXP(X(1))+2.D0*DEXP(X(2))+2.D0*DEXP  
     -(X(3))+DEXP(X(6)  
     -)+DEXP(X(10))-2.D0
      IF (INDEX1(2)) G(2)=DEXP(X(4))+2.D0*DEXP(X(5))+DEXP(X(    
     -6))+DEXP(X(7))-1.D0       
      IF (INDEX1(3)) G(3)=DEXP(X(3))+DEXP(X(7))+DEXP(X(8))      
     -+2.D0*DEXP(X(9))+ 
     -DEXP(X(10))-1.D0  
      RETURN    
5     IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,1)=DEXP(X(1))
      GG(1,2)=2.D0*DEXP(X(2))   
      GG(1,3)=2.D0*DEXP(X(3))   
      GG(1,6)=DEXP(X(6))
      GG(1,10)=DEXP(X(10))      
7     IF (.NOT.INDEX2(2)) GOTO 8
      GG(2,4)=DEXP(X(4))
      GG(2,5)=2.D0*DEXP(X(5))   
      GG(2,6)=DEXP(X(6))
      GG(2,7)=DEXP(X(7))
8     IF (.NOT.INDEX2(3)) GOTO 9
      GG(3,3)=DEXP(X(3))
      GG(3,7)=DEXP(X(7))
      GG(3,8)=DEXP(X(8))
      GG(3,9)=2.D0*DEXP(X(9))   
      GG(3,10)=DEXP(X(10))      
9     RETURN    
      END       
