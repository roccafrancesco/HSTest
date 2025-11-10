      SUBROUTINE TP100(MODE)    
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
      DOUBLEPRECISION V1,V2      
      GOTO (1,2,3,4,5),MODE     
1     N=7       
      NILI=0    
      NINL=4    
      NELI=0    
      NENL=0    
      DO 6 I=1,7
      LXL(I)=.FALSE.    
6     LXU(I)=.FALSE.    
      X(1)=1.D0 
      X(2)=2.D0 
      X(3)=0.D0 
      X(4)=4.D0 
      X(5)=0.D0 
      X(6)=1.D0 
      X(7)=1.D0 
      GG(1,3)=-1.D0     
      GG(1,5)=-5.D0     
      GG(1,6)=0.D0      
      GG(1,7)=0.D0      
      GG(2,1)=-7.D0     
      GG(2,2)=-3.D0     
      GG(2,4)=-1.D0     
      GG(2,5)=1.D0      
      GG(2,6)=0.D0      
      GG(2,7)=0.D0      
      GG(3,1)=-23.D0    
      GG(3,3)=0.D0      
      GG(3,4)=0.D0      
      GG(3,5)=0.D0      
      GG(3,7)=8.D0      
      GG(4,4)=0.D0      
      GG(4,5)=0.D0      
      GG(4,6)=-5.D0     
      GG(4,7)=11.D0     
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.233049937431D+01     
      XEX(2)=0.195137237315D+01     
      XEX(3)=-0.477541392625D+00     
      XEX(4)=0.436572623462D+01     
      XEX(5)=-0.624486970475D+00     
      XEX(6)=0.103813101881D+01     
      XEX(7)=0.159422671137D+01     
      FEX=0.680630057275D+03
      RETURN    
2     FX=(X(1)-10.D0)**2+5.D0*(X(2)-12.D0)**2+X(3)**4
     /    +3.D0*(X(4)-11.D0)**2+10.D0*X(5)**6+7.D0*X(6)**2
     /    +X(7)**4-4.D0*X(6)*X(7)-10.D0*X(6)-8.D0*X(7)      
      RETURN    
3     GF(1)=2.D0*(X(1)-10.D0)   
      GF(2)=10.D0*(X(2)-12.D0)  
      GF(3)=4.D0*X(3)**3
      GF(4)=6.D0*(X(4)-11.D0)   
      GF(5)=60.D0*X(5)**5       
      GF(6)=14.D0*X(6)-4.D0*X(7)-10.D0  
      GF(7)=4.D0*X(7)**3-4.D0*X(6)-8.D0 
      RETURN    
4     V1=2.D0*X(1)**2   
      V2=X(2)**2
      IF (INDEX1(1)) G(1)=-V1-3.D0*V2**2-X(3)-4.D0*X(4)**2-     
     /                    5.D0*X(5)+127.D0  
      IF (INDEX1(2)) G(2)=-7.D0*X(1)-3.D0*X(2)-10.D0*X(3)**2-   
     /                    X(4)+X(5)+282.D0  
      IF (INDEX1(3)) G(3)=-23.D0*X(1)-V2-6.D0*X(6)**2
     /                    +8.D0*X(7)+196.D0 
      IF (INDEX1(4)) G(4)=-2.D0*V1-V2+3.D0*X(1)*X(2)
     /                     -2.D0*X(3)**2-5.D0*X(6)+11.D0*X(7)  
      RETURN    
5     IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,1)=-4.D0*X(1)
      GG(1,2)=-12.D0*X(2)**3    
      GG(1,4)=-8.D0*X(4)
7     IF (INDEX2(2)) GG(2,3)=-20.D0*X(3)
      IF(.NOT.INDEX2(3)) GOTO 9 
      GG(3,2)=-2.D0*X(2)
      GG(3,6)=-12.D0*X(6)       
9     IF (.NOT.INDEX2(4)) GOTO 10       
      GG(4,1)=-8.D0*X(1)+3.D0*X(2)      
      GG(4,2)=-2.D0*X(2)+3.D0*X(1)      
      GG(4,3)=-4.D0*X(3)
10    RETURN    
      END       
