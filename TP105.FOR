      SUBROUTINE TP105(MODE)    
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
     /           /D105/Y(235)
      DIMENSION A(235),B(235),C(235),DA(235,8),D 
     /          B(235,8),DC(235,8)
      DOUBLEPRECISION Y,A,B,C,DA,DB,DC,S,V,V0,V1,V2,V3,V4,V5,V6,
     /          V7,V8,V9,V10,V11,T1,SUM      
      INTEGER I,J
      IF (MODE-1) 1,1,20      
1     N=8       
      NILI=1    
      NINL=0    
      NELI=0    
      NENL=0    
      DO 59 I=1,8       
      LXL(I)=.TRUE.     
59    LXU(I)=.TRUE.     
      XL(1)=1.D-3       
      XL(2)=1.D-3       
      XL(3)=100.D0      
      XL(4)=130.D0      
      XL(5)=170.D0      
      XU(1)=0.499D0     
      XU(2)=0.499D0     
      XU(3)=180.D0      
      XU(4)=210.D0      
      XU(5)=240.D0      
      DO 62 I=6,8       
      XL(I)=5.D0
62    XU(I)=25.D0       
      X(1)=0.1D0
      X(2)=0.2D0
      X(3)=100.D0       
      X(4)=125.D0       
      X(5)=175.D0       
      X(6)=11.2D0       
      X(7)=13.2D0       
      X(8)=15.8D0       
      Y(1)=95.D0
      Y(2)=105.D0       
      DO 30 I=3,6       
30    Y(I)=110.D0       
      DO 31 I=7,10      
31    Y(I)=115.D0       
      DO 32 I=11,25     
32    Y(I)=120.D0       
      DO 33 I=26,40     
33    Y(I)=125.D0       
      DO 34 I=41,55     
34    Y(I)=130.D0       
      DO 35 I=56,68     
35    Y(I)=135.D0       
      DO 36 I=69,89     
36    Y(I)=140.D0       
      DO 37 I=90,101    
37    Y(I)=145.D0       
      DO 38 I=102,118   
38    Y(I)=150.D0       
      DO 39 I=119,122   
39    Y(I)=155.D0       
      DO 40 I=123,142   
40    Y(I)=160.D0       
      DO 41 I=143,150   
41    Y(I)=165.D0       
      DO 42 I=151,167   
42    Y(I)=170.D0       
      DO 43 I=168,175   
43    Y(I)=175.D0       
      DO 44 I=176,181   
44    Y(I)=180.D0       
      DO 45 I=182,187   
45    Y(I)=185.D0       
      DO 46 I=188,194   
46    Y(I)=190.D0       
      DO 47 I=195,198   
47    Y(I)=195.D0       
      DO 48 I=199,201   
48    Y(I)=200.D0       
      DO 49 I=202,204   
49    Y(I)=205.D0       
      DO 50 I=205,212   
50    Y(I)=210.D0       
      Y(213)=215.D0     
      DO 51 I=214,219   
51    Y(I)=220.D0       
      DO 52 I=220,224   
52    Y(I)=230.D0       
      Y(225)=235.D0     
      DO 53 I=226,232   
53    Y(I)=240.D0       
      Y(233)=245.D0     
      Y(234)=260.D0     
      Y(235)=260.D0     
      GG(1,1)=-1.D0     
      GG(1,2)=-1.D0     
      DO 58 I=3,8       
58    GG(1,I)=0.D0      
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.412892753597D+00     
      XEX(2)=0.403352658261D+00     
      XEX(3)=0.131261311486D+03     
      XEX(4)=0.164313514476D+03     
      XEX(5)=0.217422221771D+03     
      XEX(6)=0.122801780396D+02     
      XEX(7)=0.157716989473D+02     
      XEX(8)=0.207468249193D+02     
      FEX=0.113841623960D+04 
      RETURN    
20    IF (MODE - 4) 21,4,5      
21    S=0.D0    
      V=1.D0/DSQRT(8.D0*DATAN(1.D0))    
      V1=X(1)/X(6)      
      V2=X(2)/X(7)      
      V3=(1.D0-X(1)-X(2))/X(8)  
      V4=1.D0/(2.D0*X(6)**2)    
      V5=1.D0/(2.D0*X(7)**2)    
      V6=1.D0/(2.D0*X(8)**2)    
      DO 54 I=1,235    
      A(I)=V1*DEXP(DMAX1(-(Y(I)-X(3))**2*V4,-1.0D1))  
      B(I)=V2*DEXP(DMAX1(-(Y(I)-X(4))**2*V5,-1.0D1))  
      C(I)=V3*DEXP(DMAX1(-(Y(I)-X(5))**2*V6,-1.0D1))  
      V11=(A(I)+B(I)+C(I))*V    
      IF (V11.LE.1.0D-5) GOTO 70  
54    S=S+DLOG(V11)     
      IF (MODE.EQ.3) GOTO 3     
2     FX=-S     
      RETURN    
3     DO 60 I=1,235     
      DO 60 J=1,8       
      DA(I,J)=0.D0      
      DB(I,J)=0.D0      
60    DC(I,J)=0.D0      
      DO 55 I=1,235     
      V0=X(6)**2
      V2=Y(I)-X(3)      
      V1=DEXP(-V2**2/(2.D0*V0)) 
      DA(I,1)=V1/X(6)   
      DA(I,3)=X(1)*V2/X(6)**3*V1
      DA(I,6)=X(1)/V0*(V2**2/V0-1.D0)*V1
      V3=X(7)**2
      V4=Y(I)-X(4)      
      V5=DEXP(-V4**2/(2.D0*V3)) 
      DB(I,2)=V5/X(7)   
      DB(I,4)=X(2)*V4/X(7)**3*V5
      DB(I,7)=X(2)/V3*(V4**2/V3-1.D0)*V5
      V7=X(8)**2
      V9=Y(I)-X(5)      
      V8=DEXP(-V9**2/(2.D0*V7)) 
      V10=1.D0-X(1)-X(2)
      DC(I,1)=-V8/X(8)  
      DC(I,2)=DC(I,1)   
      DC(I,5)=V10*V9/X(8)**3*V8 
      DC(I,8)=V10/V7*(V9**2/V7-1.D0)*V8 
55    CONTINUE  
      DO 57 J=1,8       
      T1=0.D0   
      DO 56 I=1,235     
56    T1=T1+(DA(I,J)+DB(I,J)+DC(I,J))/(A(I)+B(I)+C(I))  
      GF(J)=-T1 
57    CONTINUE  
      RETURN    
70    DO 71 I=1,8       
      SUM=0.D0  
71    SUM=SUM+(X(I)-5.D0)**2    
      FX=SUM+2.09D+3    
      RETURN    
4     IF (INDEX1(1)) G(1)=1.D0-X(1)-X(2)
5     RETURN    
      END       
