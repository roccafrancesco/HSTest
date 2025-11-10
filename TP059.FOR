      SUBROUTINE TP59(MODE)     
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
      COMMON     /D59/ X11,X12,X13,X14,X21,X22,X23,X24,XX11,XX12,XX13,  
     /     XX21,XX31   
      DOUBLEPRECISION X11,X12,X13,X14,X21,X22,X23,X24,XX11,XX12,XX13, 
     /     XX21,XX31    
      INTEGER I
      GOTO (1,2,3,4,5),MODE     
1     N=2       
      NILI=0    
      NINL=3    
      NELI=0    
      NENL=0    
      X(1)=90.D0
      X(2)=10.D0
      DO 6 I=1,2
      LXL(I)=.TRUE.     
      LXU(I)=.TRUE.     
6     XL(I)=0.D0
      XU(1)=75.D0       
      XU(2)=65.D0       
      GG(2,2)=1.D0      
      GG(3,1)=-5.D0     
      NEX=2
      LEX=.FALSE.       
      XEX(1)=13.5501042366D0  
      XEX(2)=51.6601812877D0  
      FEX=-7.80422632408D0    
c      XEX(3)=0.463995762710D+2
c      XEX(4)=0.522196899513D+2
C     FEX=-0.675456604292D+1  
      RETURN    
2     X11=X(1)  
      X12=X11*X11       
      X13=X12*X11       
      X14=X13*X11       
      X21=X(2)  
      X22=X21*X21       
      X23=X22*X21       
      X24=X23*X21       
      FX=-75.196D0+3.8112D0*X11-0.12694D0*X12+2.0567D-3*X13
     /    -1.0345D-5*X14      
     /    +6.8306D0*X21-3.0234D-2*X11*X21+1.28134D-3*X12*X21
     /    -3.5256D-5*X13*X21      
     /    +2.266D-7*X14*X21-0.25645D0*X22+3.4604D-3*X23
     /    -1.3514D-5*X24+28.106D0 
     /         /(X21+1.D0)+5.2375D-6*X12*X22
     /    +6.3D-8*X13*X22-7.D-10*X13*X23       
     /    -3.4054D-4*X11*X22+1.6638D-6*X11*X23
     /    +2.8673D0*DEXP(5.D-4*X11*X21) 
      RETURN    
3     X11=X(1)  
      X12=X11*X11       
      X13=X12*X11       
      X14=X13*X11       
      X21=X(2)  
      X22=X21*X21       
      X23=X22*X21       
      XX11=X11*X21      
      XX12=X11*X22      
      XX21=X12*X21      
      XX31=X13*X21      
      GF(1)=3.8112D0-0.25388D0*X11+6.1701D-3*X12-4.138D-5*X13-3.0234D-2 
     -*X21      
     -+2.56268D-3*XX11-1.05768D-4*XX21+9.064D-7*XX31+1.0475D-5*XX12     
     -+1.89D-7*X12*X22-2.1D-9*X12*X23-3.4054D-4*X22+1.6638D-6*X23       
     -+1.43365D-3*X21*DEXP(5.D-4*XX11)  
      GF(2)=6.8306D0-3.0234D-2*X11+1.28134D-3*X12-3.5256D-5*X13+2.266D-7
     -*X14-0.5129D0*X21+1.03812D-2*X22-5.4056D-5*X23-28.106D0/(X21      
     -+1.D0)**2 
     -+1.0475D-5*XX21+1.26D-7*XX31-2.1D-9*X13*X22-6.8108D-4*XX11
     -+4.9914D-6*XX12+1.43365D-3*X11*DEXP(5.D-4*XX11)   
      RETURN    
4     IF (INDEX1(1)) G(1)=X(1)*X(2)-700.D0      
      IF (INDEX1(2)) G(2)=X(2)-8.D-3*X(1)**2    
      IF (INDEX1(3)) G(3)=(X(2)-50.D0)**2-5.D0*(X(1)-55.D0)     
      RETURN    
5     IF(.NOT.INDEX2(1)) GOTO 7 
      GG(1,1)=X(2)      
      GG(1,2)=X(1)      
7     IF (INDEX2(2)) GG(2,1)=-1.6D-2*X(1)       
      IF (INDEX2(3)) GG(3,2)=2.D0*(X(2)-50.D0)  
      RETURN    
      END       
