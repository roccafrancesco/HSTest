      SUBROUTINE TP70(MODE)     
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
      COMMON     /D70/ DF(19,4),V4(19),U1(19),U2(19),YC(19),C(19),
     /                 V3(19),V8(19),V9(19),F(19),YO(19)       
      DOUBLEPRECISION DF,V4,U1,U2,YC,C,V3,V8,V9,F,YO,B,H1,H2,H3,  
     /     H4,H5,H6,H7,H30,H40,V10,V11,V12,Z1,Z2,Z5,Z6,Z7,
     /     T,H8,H9,H10,H11,H12,H13,H14,H15,H16,H17,H18,H19,
     /     H20,H21,H22,H23,S,SUM   
      INTEGER I,J
      LOGICAL LOG
      LOG=.FALSE.       
      IF (MODE - 2) 1,17,17     
1     N=4       
      NILI=0    
      NINL=1    
      NELI=0    
      NENL=0    
      X(1)=2.D0 
      X(2)=4.D0 
      X(3)=0.04D0       
      X(4)=2.D0 
      DO 6 I=1,4
      LXU(I)=.TRUE.     
      LXL(I)=.TRUE.     
      XL(I)=1.D-5       
6     XU(I)=100.D0      
      XU(3)=1.D0
      GG(1,1)=0.D0      
      GG(1,2)=0.D0      
      XEX(1)=0.122769537557D+02     
      XEX(2)=0.463178796886D+01     
      XEX(3)=0.312862470961D+00     
      XEX(4)=0.202929031570D+01     
      FEX=0.749846356143D-02 
      LEX=.FALSE.       
      NEX=1
      RETURN    
   17 CONTINUE
      C(1)=0.1D0
      DO 20 I=2,19      
20    C(I)=DBLE(I-1)  
      YO(1)=1.89D-3     
      YO(2)=0.1038D0    
      YO(3)=0.268D0     
      YO(4)=0.506D0     
      YO(5)=0.577D0     
      YO(6)=0.604D0     
      YO(7)=0.725D0     
      YO(8)=0.898D0     
      YO(9)=0.947D0     
      YO(10)=0.845D0    
      YO(11)=0.702D0    
      YO(12)=0.528D0    
      YO(13)=0.385D0    
      YO(14)=0.257D0    
      YO(15)=0.159D0    
      YO(16)=0.0869D0   
      YO(17)=0.0453D0   
      YO(18)=0.01509D0  
      YO(19)=1.89D-3    
      IF (MODE - 4) 18,4,5      
18    B=X(3)+(1.D0-X(3))*X(4)   
      H1=X(1)-1.D0      
      H2=X(2)-1.D0      
      H3=1.D0/7.658D0   
      H5=B*H3   
      H4=H5/X(4)
      H6=12.D0*X(1)/(12.D0*X(1)+1.D0)   
      H7=12.D0*X(2)/(12.D0*X(2)+1.D0)   
      H30=0.D0  
      H40=0.D0  
      V10=X(2)/6.2832D0 
      V11=B/X(4)
      V12=X(1)/6.2832D0 
      IF(B.LT.0.D0.OR.V10.LT.0.D0.OR.V11.LT.0.D0.OR.V12.LT.0.D0)
     1     LOG=.TRUE.   
      IF(LOG .AND. MODE.EQ.2) GOTO 8    
      IF (LOG .AND. MODE.EQ.3) GOTO 9   
      Z1=X(3)*B**X(2)   
      Z2=V10**0.5D0     
      Z5=1.D0-X(3)      
      Z6=V11**X(1)      
      Z7=V12**0.5D0     
      DO 30 I=1,19      
      V3(I)=(C(I)*H3)**H2       
      V4(I)=DEXP(X(2)*(1.D0-C(I)*H5))   
      V8(I)=(C(I)*H3)**H1       
      V9(I)=DEXP(X(1)*(1.D0-C(I)*H4))   
      U1(I)=Z1*Z2*V3(I)*V4(I)*H7
      U2(I)=Z5*Z6*Z7*V8(I)*V9(I)*H6     
      YC(I)=U1(I)+U2(I) 
30    F(I)=YC(I)-YO(I)  
      GOTO (1,2,3),MODE 
2     T=0.D0    
      DO 31 I=1,19      
31    T=T+(F(I))**2     
      FX=T      
      RETURN    
3     H8=X(4)-1.D0      
      H9=X(3)-1.D0      
      H10=1.D0/B
      H11=1.D0/X(4)     
      H12=(0.5D0+1.D0/(12.D0*X(1)+1.D0))/X(1)+1.D0      
      H13=(0.5D0+1.D0/(12.D0*X(2)+1.D0))/X(2)+1.D0      
      H16=X(2)*H8       
      H17=X(1)*H8       
      H18=1.D0/X(3)-H16*H10     
      H19=1.D0/H9-H17*H10       
      H16=H16*H3
      H17=H17*H11*H3    
      H20=X(2)*H9       
      H21=X(1)*X(3)*H11**2      
      H22=H20*H3
      H23=H21*H3
      H20=H20*H10       
      H21=H21*H10*X(4)  
      DO 33 J=1,19      
      H14=H4*C(J)       
      H15=H5*C(J)       
      IF (H14.GT.0.D0) GOTO 10  
      GF(1)=2.D0*(X(1)-5.D0)    
      H30=1.D0  
      GOTO 11   
10    DF(J,1)=U2(J)*(H12-H14+DLOG(H14)) 
11    IF (H15.GT.0.D0) GOTO 12  
      GF(2)=2.D0*(X(2)-5.D0)    
      H40=1.D0  
      GOTO 13   
12    DF(J,2)=U1(J)*(H13-H15+DLOG(H15)) 
13    DF(J,3)=U1(J)*(H18+C(J)*H16)+U2(J)*(H19+C(J)*H17) 
33    DF(J,4)=U1(J)*(H22*C(J)-H20)+U2(J)*(H23*C(J)-H21) 
      IF (H30.EQ.1.D0.OR.H40.EQ.1.D0) GOTO 14   
      DO 37 I=1,4       
      S=0.D0    
      DO 36 J=1,19      
      S=S+2.D0*F(J)*DF(J,I)     
36    CONTINUE  
      GF(I)=S   
37    CONTINUE  
      RETURN    
14    DO 38 I=1,4       
      IF (I.EQ.1.AND.H30.EQ.1.D0) GOTO 38       
      IF (I.EQ.2.AND.H40.EQ..1) GOTO 38 
      S=0.D0    
      DO 39 J=1,19      
39    S=S+2.D0*F(J)*DF(J,I)     
      GF(I)=S   
38    CONTINUE  
      H30=0.D0  
      H40=0.D0  
      RETURN    
8     LOG=.FALSE.       
      SUM=0.D0  
      DO 40 I=1,4       
40    SUM=SUM+(X(I)-5.D0)**2    
      FX=SUM
      RETURN    
9     LOG=.FALSE.       
      DO 41 I=1,4       
41    GF(I)=2.D0*(X(I)-5.D0)
      RETURN    
4     IF (INDEX1(1)) G(1)=X(3)+(1.D0-X(3))*X(4) 
      RETURN    
5     IF(.NOT.INDEX2(1)) GOTO 7 
      GG(1,3)=1.D0-X(4) 
      GG(1,4)=-X(3)+1.D0
7     RETURN    
      END       
