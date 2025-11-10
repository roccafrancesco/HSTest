      SUBROUTINE TP84(MODE)     
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
      INTEGER I,J,I1
      COMMON     /D84/ A(21),B(3)      
      DOUBLEPRECISION A,B,V1     
      A(1)=-2.4345D+4   
      A(2)=-8.720288849D+6      
      A(3)=1.505125253D+5       
      A(4)=-1.566950325D+2      
      A(5)=4.764703222D+5       
      A(6)=7.294828271D+5       
      A(7)=-1.45421402D+5       
      A(8)=2.9311506D+3 
      A(9)=-40.427932D0 
      A(10)=5.106192D+3 
      A(11)=1.571136D+4 
      A(12)=-1.550111084D+5     
      A(13)=4.36053352D+3       
      A(14)=12.9492344D0
      A(15)=1.0236884D+4
      A(16)=1.3176786D+4
      A(17)=-3.266695104D+5     
      A(18)=7.39068412D+3       
      A(19)=-27.8986976D0       
      A(20)=1.6643076D+4
      A(21)=3.0988146D+4
      B(1)=2.94D+5      
      B(2)=2.94D+5      
      B(3)=2.772D+5     
      GOTO (1,2,3,4,5),MODE     
1     N=5       
      NILI=0    
      NINL=6    
      NELI=0    
      NENL=0    
      X(1)=2.52D0       
      X(2)=2.D0 
      X(3)=37.5D0       
      X(4)=9.25D0       
      X(5)=6.8D0
      DO 6 I=1,5
      LXU(I)=.TRUE.     
6     LXL(I)=.TRUE.     
      XL(1)=0.D0
      XL(2)=1.2D0       
      XL(3)=20.D0       
      XL(4)=9.D0
      XL(5)=6.5D0       
      XU(1)=1.D+3       
      XU(2)=2.4D0       
      XU(3)=60.D0       
      XU(4)=9.3D0       
      XU(5)=7.D0
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.453743097466D+01     
      XEX(2)=0.240000000002D+01     
      XEX(3)=0.600000000000D+02     
      XEX(4)=0.929999999999D+01     
      XEX(5)=0.700000000000D+01     
      FEX=-0.528033513306D+07
      RETURN    
2     FX=-(A(1)+X(1)*(A(2)+A(3)*X(2)+A(4)*X(3)+A(5)*X(4)+A(6)*X(5)))  
      RETURN    
3     GF(1)=-(A(2)+A(3)*X(2)+A(4)*X(3)+A(5)*X(4)+A(6)*X(5))
      DO 30 I=2,5       
30    GF(I)=-A(1+I)*X(1)
      RETURN    
4     DO 80 I=1,3       
      IF (.NOT.(INDEX1(I).OR.INDEX1(I+3))) GOTO 80      
      I1=I*5    
      V1=X(1)*(A(I1+2)+A(I1+3)*X(2)+A(I1+4)*X(3)+A(I1+5)*X(4)    
     /              +A(I1+6)*X(5))    
      IF (INDEX1(I)) G(I)=V1
      IF (INDEX1(I+3)) G(I+3)=(B(I)-V1)
80    CONTINUE  
      RETURN    
5     DO 90 I=1,3       
      IF (.NOT.(INDEX2(I).OR.INDEX2(I+3))) GOTO 90      
      I1=5*I+1  
      IF (.NOT.INDEX2(I)) GOTO 95       
      GG(I,1)=(A(I1+1)+A(I1+2)*X(2)+A(I1+3)*X(3)+A(I1+4)*X(4)     
     /+A(I1+5)*X(5))
      DO 91 J=2,5       
91    GG(I,J)=A(I1+J)*X(1)
      IF (.NOT.INDEX2(I+3)) GOTO 90     
      DO 92 J=1,5       
92    GG(I+3,J)=-GG(I,J)
      GOTO 90   
95    GG(I+3,1)=-(A(I1+1)+A(I1+2)*X(2)+A(I1+3)*X(3)+A(I1+4)      
     /*X(4)+A(I1+5)*X(5))
      DO 96 J=2,5       
96    GG(I+3,J)=-A(I1+J)*X(1)
90    CONTINUE  
      RETURN    
      END       
