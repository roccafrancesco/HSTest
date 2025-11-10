      SUBROUTINE TP67(MODE)     
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
      COMMON     /D67/ A(3),Y(8),DY2C(3),DY4C(3),DY(8,3),
     /                 RX,V1,V2,Y2C,V3,Y4C  
      DOUBLEPRECISION A,Y,DY2C,DY4C,DY,RX,V1,V2,Y2C,V3,Y4C  
      INTEGER I,J,IREP
      IF (MODE - 2) 1,17,17     
1     N=3       
      NILI=0    
      NINL=14   
      NELI=0    
      NENL=0    
      X(1)=1.745D+3     
      X(2)=1.2D+4       
      X(3)=110.D0       
      DO 6 I=1,3
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
6     XL(I)=1.D-5       
      XU(1)=2.D+3       
      XU(2)=1.6D+4      
      XU(3)=120.D0      
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.172837128614D+04     
      XEX(2)=0.160000000000D+05     
      XEX(3)=0.981415140238D+02     
      FEX=-0.116203650728D+04 
      RETURN    
17    RX=1.D0/X(1)      
      Y(2)=1.6D0*X(1)   
      DY(2,1)=1.6D0     
      DY(2,2)=0.D0      
      DY(2,3)=0.D0  
      IREP=0    
100   Y(3)=1.22D0*Y(2)-X(1)     
      DY(3,1)=1.22D0*DY(2,1)-1.D0       
      DY(3,2)=1.22D0*DY(2,2)    
      DY(3,3)=1.22D0*DY(2,3)    
      Y(6)=(X(2)+Y(3))*RX       
      DY(6,1)=(X(1)*DY(3,1)-X(2)-Y(3))*RX**2    
      DY(6,2)=(1.D0+DY(3,2))*RX 
      DY(6,3)=DY(3,1)*RX
      V1=0.01D0*X(1)*(13.167D0-2.D0*0.6667D0*Y(6))      
      V2=(112.D0+(13.167D0-0.6667D0*Y(6))*Y(6))*0.01D0  
      Y2C=X(1)*V2       
      DY2C(1)=V2+V1*DY(6,1)     
      DY2C(2)=V1*DY(6,2)
      DY2C(3)=V1*DY(6,3)
      IF (DABS(Y2C-Y(2))-1.D-3) 102,102,101     
101   DO 103 I=1,3      
103   DY(2,I)=DY2C(I)   
      Y(2)=Y2C
      IREP=IREP+1
      IF (IREP.GT.100) GOTO 102  
      GOTO 100  
102   Y(4)=93.D0
      DO 104 I=1,3      
104   DY(4,I)=0.D0  
      IREP=0    
105   Y(5)=86.35D0+1.098D0*Y(6)-0.038D0*Y(6)**2+0.325D0*(Y(4)-89.D0)    
      Y(8)=-133.D0+3.D0*Y(5)    
      Y(7)=35.82D0-0.222D0*Y(8) 
      DO 106 I=1,3      
      DY(5,I)=1.098D0*DY(6,I)-0.076D0*Y(6)*DY(6,I)+0.325D0*DY(4,I)      
      DY(8,I)=3.D0*DY(5,I)      
106   DY(7,I)=-0.222D0*DY(8,I)  
      V3=1.D0/(Y(2)*Y(7)+1.D+3*X(3))    
      Y4C=9.8D+4*X(3)*V3
      DO 107 I=1,2      
107   DY4C(I)=-9.8D+4*X(3)*(Y(2)*DY(7,I)+Y(7)*DY(2,I))/(Y(2)*Y(7)+1.D+3*
     -X(3))**2  
      DY4C(3)=9.8D+4*(Y(2)*Y(7)-X(3)*(Y(2)*DY(7,3)+Y(7)*DY(2,3)))*V3**2 
      IF (DABS(Y4C-Y(4))-1.D-4) 109,109,108     
108   Y(4)=Y4C  
      DO 110 I=1,3      
110   DY(4,I)=DY4C(I)
      IREP=IREP+1
      IF (IREP.GT.100) GOTO 109   
      GOTO 105  
109   GOTO (1,2,3,4,5),MODE     
2     FX=-(0.063D0*Y(2)*Y(5)-5.04D0*X(1)-3.36D0*Y(3)-0.035D0*X(2)-10.D0 
     1     *X(3))       
      RETURN    
3     DO 120 I=1,3      
120   A(I)=-0.063D0*(DY(2,I)*Y(5)+DY(5,I)*Y(2))+3.36D0*DY(3,I)  
      GF(1)=A(1)+5.04D0 
      GF(2)=A(2)+0.035D0
      GF(3)=A(3)+10.D0  
      RETURN    
4     IF (INDEX1(1)) G(1)=Y(2)  
      IF (INDEX1(2)) G(2)=Y(3)  
      IF (INDEX1(3)) G(3)=Y(4)-85.D0    
      IF (INDEX1(4)) G(4)=Y(5)-90.D0    
      IF (INDEX1(5)) G(5)=Y(6)-3.D0     
      IF (INDEX1(6)) G(6)=Y(7)-0.01D0   
      IF (INDEX1(7)) G(7)=Y(8)-145.D0   
      IF (INDEX1(8)) G(8)=5.D+3-Y(2)    
      IF (INDEX1(9)) G(9)=2.D+3-Y(3)    
      IF (INDEX1(10)) G(10)=93.D0-Y(4)  
      IF (INDEX1(11)) G(11)=95.D0-Y(5)  
      IF (INDEX1(12)) G(12)=12.D0-Y(6)  
      IF (INDEX1(13)) G(13)=4.D0-Y(7)   
      IF (INDEX1(14)) G(14)=162.D0-Y(8) 
      RETURN    
5     DO 133 J=1,7      
      IF (.NOT.INDEX2(J)) GOTO 131      
      DO 130 I=1,3      
130   GG(J,I)=DY(J+1,I) 
131   IF (.NOT.INDEX2(J+7)) GOTO 133    
      DO 132 I=1,3      
132   GG(J+7,I)=-DY(J+1,I)      
133   CONTINUE  
      RETURN    
      END       
