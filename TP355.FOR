      SUBROUTINE TP355(MODE)
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
      DOUBLEPRECISION R(4),H0,H1,H2,H3,H4,H5,H6
      INTEGER I
      H0=X(2)*X(4)
C      R(1)=0.11D+2-X(1)*X(4)-H0+X(3)*X(4)
C      R(2)=X(1)+0.1D+2*X(2)-X(3)+X(4)+H0*(X(3)-X(1))
C      R(3)=0.11D+2-0.4D+1*X(1)*X(4)-0.4D+1*H0+X(3)*X(4)
C      R(4)=0.2D+1*X(1)+0.2D+2*X(2)-0.5D+0*X(3)+0.2D+1*X(4)+0.2D+1*H0*
C     F(X(3)-0.4D+1*X(1))
      R(1)=11.0D0-(X(1)+X(2)-X(3))*X(4)
      R(2)=X(1)+10.0D0*X(2)-X(3)+X(4)+H0*(X(3)-X(1))
      R(3)=11.0D0-(4.0D0*X(1)+4.0D0*X(2)- X(3))*X(4)
      R(4)=2.0D0*X(1)+20.0D0*X(2)-0.5D0*X(3)+2.0D0*X(4) 
     /     + 2.0D0*H0*(X(3)-4.0D0*X(1))
      H1=0.2D+1*R(1) 
      H2=0.2D+1*R(2)
      H3=0.2D+1*R(3)
      H4=0.2D+1*R(4)
      H5=H1*X(4)
      H6=H2*(0.1D+1-H0)
      GOTO(1,2,3,4,5),MODE
    1 N=4
      NILI=0
      NINL=0
      NELI=0
      NENL=1
      DO 6 I=1,4
      LXU(I)=.FALSE.
      LXL(I)=.TRUE.
    6 X(I)=0.0D+0
      X(1)=0.1D0
      X(2)=0.1D0
      XL(1)=0.1D+0
      XL(2)=0.1D+0
      XL(3)=0.0D+0
      XL(4)=0.0D+0
      LEX=.FALSE.
      NEX=1
      FEX=0.69675463D+2
      XEX(1)=0.19166330D+1
      XEX(2)=0.10000000D+0
      XEX(3)=0.00000000D+0
      XEX(4)=0.19718118D+1
      RETURN
    2 FX=R(1)*R(1)+R(2)*R(2)
      RETURN
    3 GF(1)=-H5+H6
      GF(2)=-H5+H2*(0.1D+2+X(4)*(X(3)-X(1)))
      GF(3)=H5-H6
      GF(4)=H1*(-X(1)-X(2)+X(3))+H2*(0.1D+1+X(2)*(X(3)-X(1)))
      RETURN
    4 IF (INDEX1(1)) G(1)=R(1)**2+R(2)**2-R(3)**2-R(4)**2
      RETURN   
    5 IF (.NOT.INDEX2(1)) GOTO 7
      GG(1,1)=-H5+H6-H3*(-0.4D+1)*X(4)-H4*(0.2D+1-0.8D+1*H0)
      GG(1,2)=-H5+H2*(0.1D+2+X(4)*(X(3)-X(1)))-H3*(-0.4D+1)*X(4)-
     F   H4*(0.2D+2+0.2D+1*X(4)*(X(3)-0.4D+1*X(1)))
      GG(1,3)=H5-H6-H3*X(4)-H4*(-0.5D+0+0.2D+1*H0)
      GG(1,4)=H1*(-X(1)-X(2)+X(3))+H2*(0.1D+1+X(2)*(X(3)-X(1)))-H3*
     F   (-0.4D+1*X(1)-0.4D+1*X(2)+X(3))-H4*(0.2D+1+0.2D+1*X(2)*(X(3)
     F   -0.4D+1*X(1)))
    7 RETURN
      END
