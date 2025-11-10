      SUBROUTINE TP68(MODE)     
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
      COMMON     /D68/ A(2),B(2),Z(2),D(2)     
      DOUBLEPRECISION A,B,Z,D,V1,V2,V3,V4,V5,H1,H2,H3,X1
      INTEGER I,KN1
      NEX=1
      LEX=.FALSE.
      KN1=1     
      XEX(1)=0.678587452312D-01     
      XEX(2)=0.364617174165D+01     
      XEX(3)=0.266175189694D-03     
      XEX(4)=0.894862212037D+00     
      FEX=-0.920425020704D+00 
C      GOTO 9    
C      ENTRY TP69(MODE)  
C      KN1=2     
C      XEX(1)=0.293714180830D-01     
C      XEX(2)=0.119025343488D+01     
C      XEX(3)=0.233946796758D+00     
C      XEX(4)=0.791667815694D+00     
C      FEX=-0.956712887064D+03 
C9     CONTINUE
      GOTO (1,2,3,4,5),MODE     
1     N=4       
      NILI=0    
      NINL=0    
      NELI=0    
      NENL=2    
      DO 6 I=1,2
      X(I)=1.D0 
      X(I+2)=1.D0       
      LXL(I)=.TRUE.     
      LXL(I+2)=.TRUE.   
      LXU(I)=.TRUE.     
      LXU(I+2)=.TRUE.   
      XL(I+2)=0.D0      
      XU(I)=100.D0      
6     XU(I+2)=2.D0      
      XL(1)=1.D-4 
      XU(1)=1.0D0
      XL(2)=0.D0
      XL(4)=1.0D-4
      A(1)=1.D-4
      A(2)=0.1D0
      B(1)=1.D0 
      B(2)=1.D+3
      D(1)=1.D0 
      D(2)=1.D0 
      Z(1)=24.D0
      Z(2)=4.D0 
      GG(1,1)=0.D0      
      GG(1,4)=0.D0      
      GG(1,3)=1.D0      
      GG(2,1)=0.D0      
      GG(2,3)=0.D0      
      GG(2,4)=1.D0      
      GF(2)=0.D0
      LEX=.FALSE.       
      NEX=0
      RETURN    
2     CONTINUE
      X1=DMIN1(DMAX1(1.0D-8,X(1)),1.0D1)
      V1=DEXP(X1)-1.D0
      FX=(A(KN1)*Z(KN1)-X(4)*(B(KN1)*V1-X(3))/(V1+X(4)))/X1   
      RETURN    
3     V1=DEXP(X(1))     
      V2=V1-1.D0
      V3=1.D0/(V2+X(4)) 
      V4=1.D0/X(1)      
      V5=(B(KN1)*V2-X(3))*V4    
      GF(1)=-((V1*(X(4)*B(KN1)+X(3))*V3-V5)*X(4)*V3+A(KN1)*Z(KN1)*V4)*V4
      GF(3)=X(4)*V4*V3  
      GF(4)=-V5*V2*V3**2
      RETURN          
4     IF (.NOT.INDEX1(1)) GOTO 30       
      CALL MDNORD(-X(2),H1)
      G(1)=X(3)-2.D0*H1 
  30  IF (.NOT.INDEX1(2)) RETURN
      CALL MDNORD(-X(2)+DSQRT(Z(KN1)),H1)
      CALL MDNORD(-X(2)-DSQRT(Z(KN1)),H2)
      G(2)=X(4)-H1-H2
      RETURN
5     IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,2)=2.D0*DEXP(-0.5D0*X(2)**2)/DSQRT(8.D0*DATAN(1.D0)) 
7     IF (.NOT.INDEX2(2)) GOTO 8
      H1=-X(2)-D(KN1)*DSQRT(Z(KN1))     
      H2=-X(2)+D(KN1)*DSQRT(Z(KN1))     
      H3=1.D0/DSQRT(8.D0*DATAN(1.D0))   
      GG(2,2)=(DEXP(-0.5D0*H1**2)+DEXP(-0.5D0*H2**2))*H3
8     RETURN    
      END       
