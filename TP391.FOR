      SUBROUTINE TP391(MODE)
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
      DOUBLEPRECISION SUM,WURZ
      GOTO (1,2,3,3,3),MODE
    1 N=30
      NILI=0
      NINL=0
      NELI=0
      NENL=0
      DO 6 I=1,30
      SUM=0.D+0
      DO 60 J=1,30
      IF (J .EQ. I) GOTO 60
      WURZ=DSQRT(DBLE(I)/DBLE(J))
      SUM=SUM+WURZ*((DSIN(DLOG(WURZ)))**5+(DCOS(DLOG(WURZ)))**5)
   60 CONTINUE
      X(I)=-2.8742711D+0*(DBLE((I-15)**3)+SUM)
      LXU(I)=.FALSE.
    6 LXL(I)=.FALSE.
      LEX=.FALSE.
      NEX=1
      FEX=0.D+0
      XEX(1)=0.64449021D+01 
      XEX(2)=0.51379199D+01
      XEX(3)=0.40135613D+01
      XEX(4)=0.30596619D+01
      XEX(5)=0.22646038D+01
      XEX(6)=0.16151497D+01
      XEX(7)=0.10956969D+01
      XEX(8)=0.69059536D+00
      XEX(9)=0.38552462D+00
      XEX(10)=0.16651219D+00
      XEX(11)=0.19318427D-01
      XEX(12)=-0.70414236D-01
      XEX(13)=-0.11703457D+00
      XEX(14)=-0.13486644D+00
      XEX(15)=-0.13822151D+00
      XEX(16)=-0.14140568D+00
      XEX(17)=-0.15872230D+00 
      XEX(18)=-0.20446919D+00
      XEX(19)=-0.29293764D+00
      XEX(20)=-0.43841769D+00
      XEX(21)=-0.65524697D+00
      XEX(22)=-0.95789935D+00
      XEX(23)=-0.13608416D+01
      XEX(24)=-0.18778557D+01
      XEX(25)=-0.25219169D+01
      XEX(26)=-0.33067694D+01
      XEX(27)=-0.42480867D+01
      XEX(28)=-0.53623746D+01
      XEX(29)=-0.66652372D+01
      XEX(30)=-0.81710133D+01
      RETURN
    2 FX=0.D+0
      DO 7 I=1,30
      SUM=0.D+0
      DO 70 J=1,30
      IF (J .EQ. I) GOTO 70
      WURZ=DSQRT(X(J)**2+DBLE(I)/DBLE(J))
      SUM=SUM+WURZ*((DSIN(DLOG(WURZ)))**5+(DCOS(DLOG(WURZ)))**5)
   70 CONTINUE
    7 FX=FX+(4.2D+2*X(I)+DBLE((I-15)**3)+SUM)**2
    3 RETURN
      END      
