      SUBROUTINE TP54(MODE)     
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
      COMMON     /D54/DQ,V1,V2,V3,V4,V5,V6,V7,V8,V9,Q  
      DOUBLEPRECISION DQ,V1,V2,V3,V4,V5,V6,V7,V8,V9,Q       
      DIMENSION DQ(6)   
      GOTO (1,2,3,4,5),MODE     
1     N=6       
      NILI=0    
      NINL=0    
      NELI=1    
      NENL=0    
      X(1)=6.D+3
      X(2)=1.5D0
      X(3)=4.D+6
      X(4)=2.D0 
      X(5)=3.D-3
      X(6)=5.D+7
      DO 6 I=1,6
      LXL(I)=.TRUE.     
6     LXU(I)=.TRUE.     
      XL(1)=0.D0
      XL(2)=-10.D0      
      XL(3)=0.D0
      XL(4)=0.D0
      XL(5)=0.D0       
      XL(6)=0.D0
      XU(1)=2.D+4       
      XU(2)=10.D0       
      XU(3)=1.D+7       
      XU(4)=20.D0       
      XU(5)=1.D0
      XU(6)=2.D+8       
      GG(1,1)=1.D0    
      GG(1,2)=4.D+3
      DO 30 I=3,6       
30    GG(1,I)=0.0      
      LEX=.TRUE.
      NEX=1     
      XEX(1)=9.16D+4/7.D0       
      XEX(2)=79.D0/70.D0
      XEX(3)=2.D+6      
      XEX(4)=10.D0      
      XEX(5)=1.D-3      
      XEX(6)=1.D+8      
      FEX=-DEXP(-27.D0/280.D0)  
c      FEX=-0.90807476D0 
      RETURN    
2     V1=X(1)-1.D+4     
      V2=X(2)-1.D0      
      V3=X(3)-2.D+6     
      V4=X(4)-10.D0     
      V5=X(5)-1.D-3     
      V6=X(6)-1.D+8     
      V7=1.D0/0.96D0    
      V8=1.D0/4.9D+13   
      V9=1.D0/2.45D+13  
      Q = (1.5625D-8*V1**2 + 5.D-5*V1*V2 + V2**2)*V7 + V3**2*V8
     /   + 4.D-4*V4**2 + 4.D+2*V5**2 + 4.D-18*V6**2       
      FX = -DEXP(-0.5D0*Q)
      RETURN    
3     V1=X(1)-1.D+4     
      V2=X(2)-1.D0      
      V3=X(3)-2.D+6     
      V4=X(4)-10.D0     
      V5=X(5)-1.D-3     
      V6=X(6)-1.D+8     
      V7=1.D0/0.96D0    
      V8=1.D0/4.9D+13   
      V9=1.D0/2.45D+13  
      DQ(1)=(3.125D-8*V1+5.D-5*V2)*V7   
      DQ(2)=(5.D-5*V1+2.D0*V2)*V7       
      DQ(3)=V3*V9       
      DQ(4)=8.D-4*V4    
      DQ(5)=800.D0*V5   
      DQ(6)=8.D-18*V6   
      DO 31 I=1,6       
31    GF(I)=0.5D0*DEXP(-0.5D0*Q)*DQ(I)  
      RETURN    
4     IF (INDEX1(1)) G(1)=X(1)+4.D+3*X(2)-1.76D+4
5     RETURN    
      END       
