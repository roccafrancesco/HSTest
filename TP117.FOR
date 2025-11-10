      SUBROUTINE TP117(MODE)    
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
      DOUBLEPRECISION E,D,B,C,A,T1,T2,T3,T4,T5,T6
      COMMON     /D117/E(5),D(5),B(10),C(5,5),A(10,5),T4(5),T5(5),T6(5) 
      INTEGER I,J
      GOTO (1,2,3,2,5),MODE     
1     N=15      
      NILI=0    
      NINL=5    
      NELI=0    
      NENL=0    
      DO 20 I=1,15      
20    X(I)=1.D-3
      X(7)=60.D0
      DO 22 I=1,15      
      XL(I)=0.D0
      LXL(I)=.TRUE.     
      LXU(I)=.FALSE.    
      DO 22 J=1,5       
22    GG(J,I)=0.D0      
      E(1)=-15.D0       
      E(2)=-27.D0       
      E(3)=-36.D0       
      E(4)=-18.D0       
      E(5)=-12.D0       
      C(1,1)=30.D0      
      C(1,2)=-20.D0     
      C(1,3)=-10.D0     
      C(1,4)=32.D0      
      C(1,5)=-10.D0     
      C(2,2)=39.D0      
      C(2,3)=-6.D0      
      C(2,4)=-31.D0     
      C(2,5)=32.D0      
      C(3,3)=10.D0      
      C(3,4)=-6.D0      
      C(3,5)=-10.D0     
      C(4,4)=39.D0      
      C(4,5)=-20.D0     
      C(5,5)=30.D0      
      DO 70 I=1,5       
      DO 70 J=1,5       
70    C(J,I)=C(I,J)     
      D(1)=4.D0 
      D(2)=8.D0 
      D(3)=10.D0
      D(4)=6.D0 
      D(5)=2.D0 
      DO 72 I=1,6       
      DO 72 J=1,5       
72    A(I,J)=0.D0       
      A(1,1)=-16.D0     
      A(1,2)=2.D0       
      A(1,4)=1.D0       
      A(2,2)=-2.D0      
      A(2,4)=0.4D0      
      A(2,5)=2.D0       
      A(3,1)=-3.5D0     
      A(3,3)=2.D0       
      A(4,2)=-2.D0      
      A(4,4)=-4.D0      
      A(4,5)=-1.D0      
      A(5,2)=-9.D0      
      A(5,3)=-2.D0      
      A(5,4)=1.D0       
      A(5,5)=-2.8D0     
      A(6,1)=2.D0       
      A(6,3)=-4.D0      
      A(8,1)=-1.D0      
      A(8,2)=-2.D0      
      A(8,3)=-3.D0      
      A(8,4)=-2.D0      
      A(8,5)=-1.D0      
      DO 73 I=1,5       
      A(7,I)=-1.D0      
      A(9,I)=DBLE(I)  
73    A(10,I)=1.D0      
      B(1)=-40.D0       
      B(2)=-2.D0
      B(3)=-0.25D0      
      B(4)=-4.D0
      B(5)=-4.D0
      B(6)=-1.D0
      B(7)=-40.D0       
      B(8)=-60.D0       
      B(9)=5.D0 
      B(10)=1.D0
      DO 35 I=1,10      
35    GF(I)=-B(I)       
      DO 40 I=1,10      
      DO 40 J=1,5       
40    GG(J,I)=-A(I,J)   
      DO 41 I=1,5       
      DO 42 J=1,5       
      IF (I-J) 45,42,45 
45    GG(J,I+10)=2.D0*C(I,J)    
42    CONTINUE  
41    CONTINUE  
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.0D0  
      XEX(2)=0.0D0  
      XEX(3)=0.517413630680D+01     
      XEX(4)=0.0D0  
      XEX(5)=0.306109271525D+01     
      XEX(6)=0.118396760290D+02     
      XEX(7)=0.0D0  
      XEX(8)=0.0D0  
      XEX(9)=0.103907059194D+00     
      XEX(10)=0.0D0  
      XEX(11)=0.299992902601D+00     
      XEX(12)=0.333470928832D+00     
      XEX(13)=0.399990975915D+00     
      XEX(14)=0.428314541579D+00     
      XEX(15)=0.223960749729D+00     
      FEX=0.323486789791D+02 
      RETURN    
2     T1=0.D0   
      T2=0.D0   
      DO 30 J=1,5       
      T2=T2+D(J)*X(10+J)**3     
      DO 30 I=1,5       
      T1=T1+C(I,J)*X(10+I)*X(10+J)      
30    CONTINUE  
      T3=0.D0   
      DO 31 I=1,10      
31    T3=T3+B(I)*X(I)   
      DO 34 J=1,5       
      T4(J)=0.D0
      T5(J)=0.D0
      DO 32 I=1,5       
32    T4(J)=T4(J)+C(I,J)*X(10+I)
      DO 33 I=1,10      
33    T5(J)=T5(J)+A(I,J)*X(I)   
34    CONTINUE  
      IF (MODE.EQ.4) GOTO 4     
      FX=-(T3-T1-2.D0*T2)       
      RETURN    
3     DO 37 I=1,5       
      T6(I)=0.D0
      DO 37 J=1,5       
37    T6(I)=T6(I)+(C(I,J)+C(J,I))*X(10+J)       
      DO 36 I=1,5       
36    GF(10+I)=T6(I)+6.D0*D(I)*X(10+I)**2       
      RETURN    
4     DO 38 J=1,5       
      IF (INDEX1(J)) G(J)=2.D0*T4(J)+3.D0*D(J)*X(10+J)**2+E     
     -(J)-T5(J) 
38    CONTINUE  
      RETURN    
5     DO 39 J=1,5       
      IF (.NOT.INDEX2(J)) GOTO 39       
      GG(J,10+J)=2.D0*C(J,J)+6.D0*D(J)*X(10+J)  
39    CONTINUE  
      RETURN    
      END       
