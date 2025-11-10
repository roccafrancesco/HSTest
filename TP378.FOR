      SUBROUTINE TP378(MODE)
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
      DOUBLEPRECISION A(10),CON
      DATA A/-0.6089D+1,-0.17164D+2,-0.34054D+2,-0.5914D+1,
     1 -0.24721D+2,-0.14986D+2,-0.24100D+2,-0.10708D+2,
     1 -0.26662D+2,-0.22179D+2/	
      GOTO(1,2,3,4,5),MODE
    1 N=10
      NILI=0	
      NINL=0
      NELI=0
      NENL=3
      DO 6 I=1,10
      X(I)=-0.23D+1
      XU(I)=-0.1
      LXU(I)=.TRUE.
      XL(I)=-16.0
    6 LXL(I)=.TRUE.
      LEX=.TRUE.
      NEX=1
C      FEX=-0.47760D+2
      FEX=-0.47761091D+2 
      XEX(1)=-0.32024D+1
      XEX(2)=-0.19123D+1
      XEX(3)=-0.2444D+0
      XEX(4)=-0.15670D+2
      XEX(5)=-0.7217D+0
      XEX(6)=-0.72736D+1
      XEX(7)=-0.35965D+1
      XEX(8)=-0.40206D+1
      XEX(9)=-0.32885D+1
      XEX(10)=-0.23344D+1
      GG(1,4)=0.0D+0
      GG(1,5)=0.0D+0
      GG(1,7)=0.0D+0
      GG(1,8)=0.0D+0
      GG(1,9)=0.0D+0
      GG(2,1)=0.0D+0
      GG(2,2)=0.0D+0
      GG(2,3)=0.0D+0
      GG(2,8)=0.0D+0
      GG(2,9)=0.0D+0
      GG(2,10)=0.0D+0
      GG(3,1)=0.0D+0
      GG(3,2)=0.0D+0
      GG(3,4)=0.0D+0
      GG(3,5)=0.0D+0
      GG(3,6)=0.0D+0
      RETURN
    2 FX=0.0D+0
      CON=0.0D+0
      DO 7 J=1,10
    7 CON=CON+DEXP(X(J))
      CON=DLOG(CON)
      DO 8 I=1,10
    8 FX=FX+DEXP(X(I))*(A(I)+X(I)-CON)
    3 RETURN
      CON=0.0D+0
      DO 45 J=1,10
   45 CON=CON+DEXP(X(J))
      CON=DLOG(CON)
      DO 46 I=1,10
      GF(I)=DEXP(X(I))*(A(I)+X(I)-CON)
   46 CONTINUE 
      RETURN    
    4 IF(INDEX1(1)) G(1)=DEXP(X(1))+0.2D+1*DEXP(X(2))+0.2D+1*DEXP(X(3))+
     +DEXP(X(6))+DEXP(X(10))-0.2D+1
      IF(INDEX1(2)) G(2)=DEXP(X(4))+0.2D+1*DEXP(X(5))+DEXP(X(6))+
     +DEXP(X(7))-0.1D+1
      IF(INDEX1(3)) G(3)=DEXP(X(3))+DEXP(X(7))+DEXP(X(8))+0.2D+1*
     *DEXP(X(9))+DEXP(X(10))-0.1D+1
      RETURN
    5 IF(.NOT.INDEX2(1)) GOTO 47
      GG(1,1)=DEXP(X(1))
      GG(1,2)=DEXP(X(2))*0.2D+1
      GG(1,3)=DEXP(X(3))*0.2D+1
      GG(1,6)=DEXP(X(6))
      GG(1,10)=DEXP(X(10))
   47 IF(.NOT.INDEX2(2)) GOTO 48
      GG(2,4)=DEXP(X(4))
      GG(2,5)=DEXP(X(5))*0.2D+1
      GG(2,6)=DEXP(X(6))
      GG(2,7)=DEXP(X(7))
   48 IF(.NOT.INDEX2(3)) GOTO 49
      GG(3,3)=DEXP(X(3))
      GG(3,7)=DEXP(X(7))
      GG(3,8)=DEXP(X(8))
      GG(3,9)=DEXP(X(9))*0.2D+1
      GG(3,10)=DEXP(X(10))
   49 RETURN
      END
