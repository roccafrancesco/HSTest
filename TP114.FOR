      SUBROUTINE TP114(MODE)    
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
      INTEGER I,J
      DOUBLEPRECISION V1,V2,V3   
      GOTO (1,2,3,4,5),MODE     
1     N=10      
      NILI=4    
      NINL=4    
      NELI=1    
      NENL=2    
      X(1)=1.745D+3     
      X(2)=1.2D+4       
      X(3)=110.D0       
      X(4)=3.048D+3     
      X(5)=1.974D+3     
      X(6)=89.2D0       
      X(7)=92.8D0       
      X(8)=8.D0 
      X(9)=3.6D0
      X(10)=145.D0      
      DO 25 I=1,5       
25    XL(I)=1.D-5       
      XL(6)=85.D0       
      XL(7)=90.D0       
      XL(8)=3.D0
      XL(9)=1.2D0       
      XL(10)=145.D0     
      XU(1)=2.D+3       
      XU(2)=1.6D+4      
      XU(3)=120.D0      
      XU(4)=5.D+3       
      XU(5)=2.D+3       
      XU(6)=93.D0       
      XU(7)=95.D0       
      XU(8)=12.D0       
      XU(9)=4.D0
      XU(10)=162.D0     
      DO 6 I=1,10       
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
      DO 6 J=1,11       
6     GG(J,I)=0.D0      
      GF(1)=5.04D0*1.0D-4      
      GF(2)=0.035D0*1.0D-4     
      GF(3)=10.D0*1.0D-4    
      GF(4)=0.0D0   
      GF(5)=3.36D0*1.0D-4      
      GF(6)=0.D0
      GF(7)=0.0D0   
      GF(8)=0.D0
      GF(9)=0.D0
      GF(10)=0.D0       
      GG(1,9)=-0.9D0    
      GG(1,10)=-0.222D0 
      GG(2,7)=3.D0      
      GG(2,10)=-0.99D0  
      GG(3,9)=10.D0/9.D0
      GG(3,10)=0.222D0  
      GG(4,7)=-3.D0     
      GG(4,10)=100.D0/99.D0     
      GG(5,4)=-0.99D0   
      GG(6,6)=0.325D0   
      GG(6,7)=-0.99D0   
      GG(7,4)=100.D0/99.D0      
      GG(8,6)=-0.325D0  
      GG(8,7)=100.D0/99.D0      
      GG(9,1)=-1.D0     
      GG(9,4)=1.22D0    
      GG(9,5)=-1.D0     
      GG(10,6)=-1.D0    
      GG(11,8)=-1.D0    
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.169809564792D+04     
      XEX(2)=0.158187256985D+05     
      XEX(3)=0.541022827849D+02     
      XEX(4)=0.303122594099D+04     
      XEX(5)=0.200000000000D+04     
      XEX(6)=0.901153669236D+02     
      XEX(7)=0.950000000000D+02     
      XEX(8)=0.104933580864D+02     
      XEX(9)=0.156163636380D+01     
      XEX(10)=0.153535353535D+03     
      FEX=-0.176880696344D+04
      RETURN    
2     FX=5.04D0*X(1)+0.035D0*X(2)+10.D0*X(3)+3.36D0*X(5)
     /       -0.063D0*X(4)*X(7) 
3     GF(4)=-0.063D0*X(7)      
      GF(7)=-0.063D0*X(4)       
      RETURN    
4     IF (INDEX1(1)) G(1)=35.82D0-0.222D0*X(10)-0.9D0*X(9)      
      IF (INDEX1(2)) G(2)=-133.D0+3.D0*X(7)-0.99D0*X(10)
      IF (INDEX1(3)) G(3)=-35.82D0+0.222D0*X(10)+10.D0/9.D0*X(9) 
      IF (INDEX1(4)) G(4)=133.D0-3.D0*X(7)+X(10)/0.99D0 
      IF (INDEX1(5)) G(5)=1.12D0*X(1)+0.13167D0*X(1)*X(8)
     /       -6.67D-3*X(1)*X(8)**2-0.99D0*X(4)    
      IF (INDEX1(6)) G(6)=57.425D0+1.098D0*X(8)-0.038D0*X(8)**2
     /       +0.325D0*X(6)-0.99D0*X(7)      
      IF (INDEX1(7)) G(7)=-1.12D0*X(1)-0.13167D0*X(1)*X(8)+     
     /        6.67D-3*X(1)*X(8)**2+X(4)/0.99D0   
      IF (INDEX1(8)) G(8)=-57.425D0-1.098D0*X(8)+0.038D0*X(8)**2
     /        -0.325D0*X(6)+X(7)/0.99D0       
      IF (INDEX1(9)) G(9)=1.22D0*X(4)-X(1)-X(5) 
      IF (INDEX1(10)) G(10)=9.8D+4*X(3)/(X(4)*X(9)+1.D+3*X(3))-X(6)
      IF (INDEX1(11)) G(11)=(X(2)+X(5))/X(1)-X(8)       
      RETURN    
5     IF (.NOT.INDEX2(5)) GOTO 11       
      GG(5,1)=1.12D0+0.13167D0*X(8)-6.67D-3*X(8)**2     
      GG(5,8)=0.13167D0*X(1)-2.D0*6.67D-3*X(1)*X(8)     
11    IF (INDEX2(6)) GG(6,8)=1.098D0-0.076D0*X(8)       
      IF (.NOT.INDEX2(7)) GOTO 13       
      GG(7,8)=-0.13167D0*X(1)+2.D0*6.67D-3*X(1)*X(8)    
      GG(7,1)=-0.13167D0*X(8)+6.67D-3*X(8)**2-1.12D0    
13    IF (INDEX2(8)) GG(8,8)=-1.098D0+0.076D0*X(8)      
      IF (.NOT.INDEX2(10)) GOTO 16      
      V1=(X(4)*X(9)+1.D+3*X(3))**2      
      V2=9.8D+4*X(9)    
      V3=V2/V1  
      GG(10,3)=X(4)*V3  
      GG(10,4)=-X(3)*V3 
      GG(10,9)=-9.8D+4*X(3)*X(4)/V1     
16    IF (.NOT.INDEX2(11)) GOTO 17      
      GG(11,1)=-(X(2)+X(5))/X(1)**2     
      GG(11,2)=1.D0/X(1)
      GG(11,5)=GG(11,2) 
17    RETURN    
      END       
