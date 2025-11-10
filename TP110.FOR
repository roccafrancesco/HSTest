      SUBROUTINE TP110(MODE)    
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
      DOUBLEPRECISION T,S,U,SUM  
      INTEGER I
      GOTO (1,2,2,4,4),MODE     
1     N=10      
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=0    
      DO 20 I=1,10      
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
      XL(I)=2.001D0     
      XU(I)=9.999D0     
      X(I)=9.D0 
20    CONTINUE  
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.935025654733D+01     
      XEX(2)=0.935025654733D+01     
      XEX(3)=0.935025654733D+01     
      XEX(4)=0.935025654733D+01     
      XEX(5)=0.935025654733D+01     
      XEX(6)=0.935025654733D+01     
      XEX(7)=0.935025654733D+01     
      XEX(8)=0.935025654733D+01     
      XEX(9)=0.935025654733D+01     
      XEX(10)=0.935025654733D+01     
      FEX=-0.457784697153D+02 
      RETURN    
 2    T=1.D0    
      DO 30 I=1,10      
30    T=T*X(I)  
      S=(DABS(T))**0.2D0
      S=DSIGN(S,T)      
      IF (MODE.EQ.3) GOTO 3     
      U=0.D0    
      DO 31 I=1,10      
      IF ((X(I)-2.D0).LE.0.D0.OR.(10.D0-X(I)).LE.0.D0) GOTO 33  
31    U=U+(DLOG(X(I)-2.D0))**2+(DLOG(10.D0-X(I)))**2    
      FX=U-S    
      RETURN    
33    SUM=0.D0  
      DO 34 I=1,10      
34    SUM=SUM+(X(I)-5.D0)**2    
      FX=SUM+1.D+3-45.8D0       
      RETURN    
3     DO 32 I=1,10      
      IF ((X(I)-2.D0).LE.0.D0.OR.(10.D0-X(I)).LE.0.D0) GOTO 35  
      GF(I)=2.D0*(DLOG(X(I)-2.D0)/(X(I)-2.D0)-DLOG(10.D0-X(I))/ 
     /     (10.D0-X(I)))-S/X(I)*0.2D0   
      GOTO 32   
35    GF(I)=2.D0*(X(I)-5.D0)    
32    CONTINUE  
4     RETURN    
      END       
