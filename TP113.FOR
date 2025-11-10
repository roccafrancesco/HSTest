      SUBROUTINE TP113(MODE)    
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
      GOTO (1,2,3,4,5),MODE     
1     N=10      
      NILI=3    
      NINL=5    
      NELI=0    
      NENL=0    
      DO 20 I=1,8       
      DO 20 J=1,10      
20    GG(I,J)=0.D0      
      X(1)=2.D0 
      X(2)=3.D0 
      X(3)=5.D0 
      X(4)=5.D0 
      X(5)=1.D0 
      X(6)=2.D0 
      X(7)=7.D0 
      X(8)=3.D0 
      X(9)=6.D0 
      X(10)=10.D0       
      DO 6 I=1,10       
      LXL(I)=.FALSE.    
6     LXU(I)=.FALSE.    
      GG(1,1)=-4.D0     
      GG(1,2)=-5.D0     
      GG(1,7)=3.D0      
      GG(1,8)=-9.D0     
      GG(2,1)=-10.D0    
      GG(2,2)=8.D0      
      GG(2,7)=17.D0     
      GG(2,8)=-2.D0     
      GG(3,1)=8.D0      
      GG(3,2)=-2.D0     
      GG(3,9)=-5.D0     
      GG(3,10)=2.D0     
      GG(4,4)=7.D0      
      GG(5,2)=-8.D0     
      GG(5,4)=2.D0      
      GG(6,6)=1.D0      
      GG(7,5)=-14.D0    
      GG(7,6)=6.D0      
      GG(8,1)=3.D0      
      GG(8,2)=-6.D0     
      GG(8,10)=7.D0     
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.217199637118D+01     
      XEX(2)=0.236368297378D+01     
      XEX(3)=0.877392573847D+01     
      XEX(4)=0.509598448797D+01     
      XEX(5)=0.990654764992D+00     
      XEX(6)=0.143057397893D+01     
      XEX(7)=0.132164420805D+01     
      XEX(8)=0.982872580801D+01     
      XEX(9)=0.828009167017D+01     
      XEX(10)=0.837592666387D+01     
      FEX=0.243062090641D+02 
      RETURN    
2     FX=X(1)**2+X(2)**2+X(1)*X(2)-14.D0*X(1)-16.D0*X(2)
     /     +(X(3)-10.D0)**2+4.D0*(X(4)-5.D0)**2+(X(5)-3.D0)**2
     /     +2.D0*(X(6)-1.D0)**2+5.D0*X(7)**2+7.D0*(X(8)-11.D0)**2
     /     +2.D0*(X(9)-10.D0)**2+(X(10)-7.D0)**2+45.D0      
      RETURN    
3     GF(1)=2.D0*X(1)+X(2)-14.D0
      GF(2)=2.D0*X(2)+X(1)-16.D0
      GF(3)=2.D0*(X(3)-10.D0)   
      GF(4)=8.D0*(X(4)-5.D0)    
      GF(5)=2.D0*(X(5)-3.D0)    
      GF(6)=4.D0*(X(6)-1.D0)    
      GF(7)=10.D0*X(7)  
      GF(8)=14.D0*(X(8)-11.D0)  
      GF(9)=4.D0*(X(9)-10.D0)   
      GF(10)=2.D0*(X(10)-7.D0)  
      RETURN    
4     IF (INDEX1(1)) G(1)=-4.D0*X(1)-5.D0*X(2)+3.D0*X(7)-9.D0*X(8)
     /     +105.D0
      IF (INDEX1(2)) G(2)=-10.D0*X(1)+8.D0*X(2)+17.D0*X(7)
     /    -2.D0*X(8)     
      IF (INDEX1(3)) G(3)=8.D0*X(1)-2.D0*X(2)-5.D0*X(9)
     /     +2.D0*X(10)+12.D0 
      IF (INDEX1(4)) G(4)=-3.D0*(X(1)-2.D0)**2-4.D0*(X(2)-3.D0)**2
     /     -2.D0*X(3)**2+7.D0*X(4)+120.D0      
      IF (INDEX1(5)) G(5)=-5.D0*X(1)**2-8.D0*X(2)-(X(3)-6.D0)**2
     /     +2.D0*X(4)+40.D0       
      IF (INDEX1(6)) G(6)=-0.5D0*(X(1)-8.D0)**2-2.D0*(X(2)
     /     -4.D0)**2-3.D0*X(5)**2+X(6)+30.D0 
      IF (INDEX1(7)) G(7)=-X(1)**2-2.D0*(X(2)-2.D0)**2
     /     +2.D0*X(1)*X(2)-14.D0*X(5)+6.D0*X(6)       
      IF (INDEX1(8)) G(8)=3.D0*X(1)-6.D0*X(2)-12.D0*(X(9)-8.D0)**2
     /     +7.D0*X(10)    
      RETURN    
5     IF (.NOT.INDEX2(4)) GOTO 10       
      GG(4,1)=-6.D0*(X(1)-2.D0) 
      GG(4,2)=-8.D0*(X(2)-3.D0) 
      GG(4,3)=-4.D0*X(3)
10    IF (.NOT.INDEX2(5)) GOTO 11       
      GG(5,1)=-10.D0*X(1)       
      GG(5,3)=-2.D0*(X(3)-6.D0) 
11    IF (.NOT.INDEX2(6)) GOTO 12       
      GG(6,1)=8.D0-X(1) 
      GG(6,2)=-4.D0*(X(2)-4.D0) 
      GG(6,5)=-6.D0*X(5)
12    IF (.NOT.INDEX2(7)) GOTO 13       
      GG(7,1)=-2.D0*X(1)+2.D0*X(2)      
      GG(7,2)=-4.D0*(X(2)-2.D0)+2.D0*X(1)       
13    IF (INDEX2(8)) GG(8,9)=-24.D0*(X(9)-8.D0) 
      RETURN    
      END       
