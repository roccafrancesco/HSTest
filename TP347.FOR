      SUBROUTINE TP347(MODE)
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
      COMMON     /D347/H
      DOUBLEPRECISION H(8),A(3)
      DATA(A(I),I=1,3)/0.820437D+4,0.900872D+4,0.933046D+4/
      INTEGER I
      GOTO(1,2,2,4,5),MODE
 1    N=3
      NILI=0
      NINL=0
      NELI=1
      NENL=0
      X(1)=0.7D+0
      X(2)=0.2D+0
      X(3)=0.1D+0
      DO 7  I=1,3
      LXL(I)=.TRUE.
      LXU(I)=.TRUE.
      XL(I)=0.0D+0
      XU(I)=0.1D+1
 7    GG(1,I)=0.1D+1
      LEX=.TRUE.
      NEX=1
      XEX(1)=.00000000D+00
      XEX(2)=.00000000D+00
      XEX(3)=.10000000D+01
      FEX=.17374625D+05
      RETURN
 2    H(1)=X(1)+X(2)+X(3)+0.3D-1
      H(2)=0.9D-1*X(1)+X(2)+X(3)+0.3D-1
      H(3)=H(1)*H(2)
      H(4)=X(2)+X(3)+0.3D-1
      H(5)=0.7D-1*X(2)+X(3)+0.3D-1
      H(6)=H(4)*H(5)
      H(7)=X(3)+0.3D-1
      H(8)=0.13D+0*X(3)+0.3D-1
      IF (MODE .EQ. 3) GOTO 3
      FX = A(1)*DLOG(DMAX1(H(1)/H(2),1.0D-4))
     /   + A(2)*DLOG(DMAX1(H(4)/H(5),1.0D-4))
     /   + A(3)*DLOG(DMAX1(H(7)/H(8),1.0D-4))
      RETURN
 3    GF(1)=A(1)*(H(2)-0.9D-1*H(1))/H(3)
      GF(2)=A(1)*(H(2)-H(1))/H(3)+A(2)*(H(5)-0.7D-1*H(4))/H(6)
      GF(3)=A(1)*(H(2)-H(1))/H(3)+A(2)*(H(5)-H(4))/H(6)
     F        +A(3)*(H(8)-.13D+0*H(7))/(H(7)*H(8))
      RETURN
 4    IF (INDEX1(1)) G(1)=X(1)+X(2)+X(3)-0.1D+1
 5    RETURN
      END            
