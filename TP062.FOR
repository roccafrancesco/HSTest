      SUBROUTINE TP62(MODE)     
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
      DOUBLEPRECISION B1,B2,B3,C1,C2,C3,V1,V2,V3,V4,V5,V6,V7,S,RB1,
     /     RB2,RB3,RC1,RC2,RC3  
      IF (MODE - 2) 1,17,17     
1     N=3       
      NILI=0    
      NINL=0    
      NELI=1    
      NENL=0    
      X(1)=0.7D0
      X(2)=0.2D0
      X(3)=0.1D0
      DO 6 I=1,3
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
      XL(I)=0.D0
      XU(I)=1.D0
6     GG(1,I)=1.D0      
      LEX=.FALSE.       
      NEX=1
      XEX(1)=0.617813298210D+00     
      XEX(2)=0.328202155786D+00     
      XEX(3)=0.539845460119D-01     
      FEX=-0.262725144873D+05 
      RETURN    
17    IF (MODE - 4) 18,4,5      
18    B3=X(3)+0.03D0    
      C3=0.13D0*X(3)+0.03D0     
      B2=B3+X(2)
      C2=B3+0.07D0*X(2) 
      B1=B2+X(1)
      C1=B2+0.09D0*X(1) 
      GOTO (1,2,3),MODE 
2     V5=B1/C1  
      V6=B2/C2  
      V7=B3/C3  
      IF (V5.LE.0.D0.OR.V6.LE.0.D0.OR.V7.LE.0.D0) GOTO 7
      FX=-32.174D0*(255.D0*DLOG(V5)+280.D0*DLOG(V6)+290.D0*DLOG(V7))    
      RETURN    
7     S=0.D0    
      DO 8 I=1,3
8     S=S+(X(I)-5.D0)**2
      FX=S+1.D+3-2.67D+4
      RETURN    
3     RB1=1.D0/B1       
      RB2=1.D0/B2       
      RB3=1.D0/B3       
      RC1=1.D0/C1       
      RC2=1.D0/C2       
      RC3=1.D0/C3       
      V1=-32.174D0*255.D0       
      V2=-32.174D0*280.D0       
      V3=-32.174D0*290.D0       
      V4=V1*(RB1-RC1)   
      GF(1)=V1*(RB1-0.09D0*RC1) 
      GF(2)=V4+V2*(RB2-0.07D0*RC2)      
      GF(3)=V4+V2*(RB2-RC2)+V3*(RB3-0.13D0*RC3) 
      RETURN    
4     IF (INDEX1(1)) G(1)=X(1)+X(2)+X(3)-1.D0   
5     RETURN    
      END       
