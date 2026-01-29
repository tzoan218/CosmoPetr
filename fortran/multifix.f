!  multi.f90 
!
!  FUNCTIONS:
!  multi - Entry point of console application.
!

!*******************************************************************************
!
! PURPOSE: CALCULATION OF GRAVITATIONAL WAVES-REVISED EDITION-TWO FIELD INCLUDED
!
! STATUS :IN PROGRESS
!
!*******************************************************************************
      module global
      implicit none
      integer, parameter :: nf=1!, nf2=4
!      real(8) ll(nf,nf),G(nf,nf,nf),x(nf),xd(nf)
      real*8 parameters(20)!,ccc!,b2!phi3,phi2!
      real*8 metric_matrix(nf,nf)!,ccc!,b2!phi3,phi2!
      end module global
      
      
      PROGRAM MULTIFIELD_GRAVITATIONAL_WAVES_ABUNDANCEPBH 
      use global
      implicit none
      integer i,j,timestep,nf2,timestep2,m,k,jj,iii,k_step2,kfinal
      integer jjj,kkk,jj2,FLAG
      integer j_bardeen,j_dbardeen,it,k_step,i_norm,timestep_back
     +,g_lenght
      parameter(timestep=10000,nf2=2*nf,m=4*nf+2)
      real*8 t0,dt,vareps,Hubble,cq
      real*8 q(nf2),dqdt(nf2),x(nf),xd(nf),p(m),dpdt(m),prslow(timestep)
      real*8 t(timestep),xx(nf,timestep),xxd(nf,timestep)
      real*8 ks(timestep),ks_n(timestep),kstar,r(m),drdt(m)
      real*8  varepsilon(timestep),H(timestep),logosH(timestep)
      real*8 yf_r(nf),yl_r(nf),yb_r(1),hta(timestep)
      real*8 dyf_r(nf),dyl_r(nf),dyb_r(1)
      real*8 yf_i(nf),yl_i(nf),yb_i(1)
      real*8 dyf_i(nf),dyl_i(nf),dyb_i(1)
      real*8 yl0_r(nf,timestep),yl0_i(nf,timestep)
      real*8 dyl0_r(nf,timestep),dyl0_i(nf,timestep)
      real*8 ps0_r(timestep),dps0_r(timestep)
      real*8 ps0_i(timestep),dps0_i(timestep)
      real*8 kmode,norma,prk(timestep),prk_n(timestep),pr_n(timestep)
      real*8 Q_ms,Q_r,Q_i,Q_abs,zeta,zeta_r,zeta_i
      real*8 isocurv_r, isocurv_i, isocurv, p_isocurv(timestep)
      real*8 prk_isocurv(timestep)
      real*8 n_pbh(timestep), pr_pbh(timestep),kk(timestep)
      real*8 pr(timestep),dpr,temp_pr,dc
      real*8 VV,Vx(nf),V(nf),epsi,norma_back,n_back, ks_norm,Vxx(nf,nf)
      real*8 kminus(timestep),kplus(timestep),s,d
      real*8 yl0_r_min(nf,timestep),yl0_i_min(nf,timestep),
     +dyl0_r_min(nf,timestep),dyl0_i_min(nf,timestep)
      real*8 yl0_r_plus(nf,timestep),yl0_i_plus(nf,timestep),
     +dyl0_r_plus(nf,timestep),dyl0_i_plus(nf,timestep)
      real*8 ps0_r_min(timestep),dps0_r_min(timestep)
      real*8 ps0_i_min(timestep),dps0_i_min(timestep)
      real*8 ps0_r_plus(timestep),dps0_r_plus(timestep)
      real*8 ps0_i_plus(timestep),dps0_i_plus(timestep)
      real*8 prk_plus(timestep), ks_norm_plus
      real*8 prk_min(timestep), ks_norm_min,norma_p,norma_m
      parameter(t0=0,dt=0.05D0,kstar=0.05d0,
     +dpr=0.0001,cq=100.d0,dc=0.45d0
     +,k_step=2,epsi=1.d-5)
      !real*8 pot(200),yy
      real*8 pi,opbh_ps,mpeak_ps,ppeak
      real*8 opbh_pt,mpeak_pt,ppeak_pt
      real*8 mpbh_ps(timestep), vita_ps(timestep)
     +, s2m_ps(timestep), om_ps(timestep)
      real*8 mpbh_pt(timestep), vita_pt(timestep)
     +, s2m_pt(timestep), om_pt(timestep)
      real*8 omr,h_2!,flag1
      real*8 kmode_mon,result_mon
      PARAMETER(FLAG=1)!,flag1=2
      !FLAG =1 : Solving linear perturbation
      !FLAG =2 : Solving linear perturbation + Computing gravitational waves
      !FLAG =3 : Solving linear perturbation + Computing PBH abundance
      parameter(g_lenght=20)
      real*8 IC(g_lenght,g_lenght,timestep),
     +IS(g_lenght,g_lenght,timestep),
     +theta,fun(g_lenght,g_lenght,timestep),cg,inte3
      real*8 ran2,ss(g_lenght),dd(g_lenght),s_s,
     +w_d(g_lenght),w_s(g_lenght)
       real*8 d_min,d_max,s_min,s_max,smin_new,smax_new
       real*8 prmin(g_lenght,g_lenght,timestep)
     +,prmax(g_lenght,g_lenght,timestep)
      parameter(cg=0.4d0)
      parameter(omr=5.4d-5)
      character*100 char1,fmt1!,filename
      
      pi=dacos(-1.d0)
      
      open(unit=99,file="information.txt",status="unknown")
      open(unit=98,file="fields.txt",status="unknown")
      open(unit=97,file="n_epsilon_hubble.txt",status="unknown")
      open(unit=96,file="kmode.txt",status="unknown")
      open(unit=95,file="bardeen_initial.txt",status="unknown")
      !open(unit=94,file="n_qabs.txt",status="unknown")
      open(unit=93,file="n_prz_kmode.txt",status="unknown")
      open (unit=92,file="m_o_k_ps.txt",status="unknown")
      open (unit=91,file="m_o_k_pt.txt",status="unknown")
      open(unit=90, file="prslow.txt",status="unknown")
      open(unit=89,file="gw2.txt",status="unknown") 
      open(unit=88,file="epsilon.txt",status="unknown")
      open(unit=87,file="n_ps_kmode.txt",status="unknown")


       open (unit=70,file="vxy.txt",status="unknown")
       open (unit=69,file="v2xy.txt",status="unknown")

       
      open (unit=60,file="testkpm.txt",status="unknown")
      open(unit=59,file="bardeen_intial_pm.txt",status="unknown")
      open(unit=58, file="prplus.txt",status="unknown")
      open(unit=57, file="prmin.txt",status="unknown")
      open(unit=56, file="fun.txt",status="unknown")

      open(unit=55, file="testt.txt",status="unknown")
  !    open(unit=54, file="testa.txt",status="unknown")
   !   open(unit=53, file="testb.txt",status="unknown")
      
      !open(unit=79,file="prz.txt",status="unknown")
!     Input Files
      !open(unit=11,file="field_initial_conditions.txt",status="old")
!     Give the intial conditions for the fields      
      
      do j=1,timestep
      t(j)=t0+dt*(j)
      pr(j)=0.d0
      enddo
       
      do i=1,20
      parameters(i)=0.d0
      enddo
      
!      Inform user about potential and initial conditions input
      write(*,*) '========================================'
      write(*,*) 'POTENTIAL EXPRESSION'
      write(*,*) '========================================'
      write(*,*) 'The potential is read from: potential.inc'
      write(*,*) 'To change it, edit potential.inc before compiling.'
      write(*,*) 'The expression should assign to VV variable.'
      write(*,*) 'Example: VV = (0.1*Tanh(x(1)/Sqrt(6.d0)))**2'
      write(*,*) '========================================'
      write(*,*) 'INITIAL CONDITIONS'
      write(*,*) '========================================'
      write(*,*) 'Initial conditions from initial_conditions.inc'
      write(*,*) 'Edit file before compiling'
      write(*,*) 'Set the initial values for x(1), x(2), etc.'
      write(*,*) 'Example: x(1) = 6.33'
      write(*,*) '========================================'
      write(*,*) 'METRIC'
      write(*,*) '========================================'
      write(*,*) 'Metric matrix from metric.inc'
      write(*,*) 'Edit file before compiling'
      write(*,*) 'Set metric_matrix(i,j) for each i,j'
      write(*,*) 'Example: metric_matrix(1,1) = 1.0d0'
      write(*,*) '========================================'
      
!     Initialize metric matrix from metric.inc
      include 'metric.inc'
      
!      do iii=1,10000
!       yy=( 0.1d0+(iii-100)*0.1d0)
!       Pot(iii)= VV(yy)!/(2.36871*1.d-10)
!       write(48,*) yy, pot(iii)
!       enddo 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    BACKGROUND EQUATION                     !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!initial condition for the background equation
    !  phi0=0.2.d0!55d0
 !     DO jj=1,1
      !phi2=0.1d0+(jj-1)*0.1
      !phi3=2*phi2!0.6d0!2.d0*phi2!0.4d0!+(jj-1)*0.1
      !ccc=100.d0+(j-1)*100!40.d0+(jj-1)*40.d0
!      write (filename, '("pr", I2.2, ".txt")' )  JJ 
 !     open (file=filename,unit=16,status="unknown")
       
       
!     Read the initial conditions for the fields    
      do i=1,nf
   
!     Include user-provided initial conditions
      include 'initial_conditions.inc'
       call xic(x,xd)
      print*, xd(i)
      !read(*,*)
      enddo
      !xd(1)=-0.00333333d0
      !xd(2)=-0.166168162202735d0
      
 
!     Set up initial conditions      
      do i=1,nf
      xx(i,1)=x(i)
      xxd(i,1)=xd(i)
      q(i)= xx(i,1)
      q(i+nf)=xxd(i,1)
      enddo
      
      
     
!     Solution of background equations to find the end of inflation
      
      !call derivs(nf2,t0,q,dqdt)
      !call rk4(q,dqdt,nf2,t(1),dt)
       call odeintone(q,nf2,t0,t(1),epsi,dt)
       !print*, t(1),t(2),epsi,dt
      do i=1,nf
      xx(i,1)=q(i)
      xxd(i,1)=q(i+nf)
      x(i)=q(i)
      xd(i)=q(i+nf)
      enddo         
      
      varepsilon(1)=vareps(xd)
      H(1)=Hubble(x,xd)
      logosH(1)=1.d0
      hta=(Abs(varepsilon(1)-dqdt(2)/q(2)))
 
      
      do j=2,timestep
      !call derivs(nf2,t(j-1),q,dqdt)
      !call rk4(q,dqdt,nf2,t(j),dt)
      call odeintone(q,nf2,t(j-1),t(j),epsi,dt)

      do i=1,nf
      xx(i,j)=q(i)
      xxd(i,j)=q(i+nf)
      x(i)=q(i)
      xd(i)=q(i+nf)
      enddo 
 
!      call Vx1(Vx,x)
!      write(70,*) t(j) ,Vx
!       do jj=1,nf
!950    format (5(1x,f10.6))        
!       call Vxx1(Vxx,x)
!        write(69,950) t(j), vxx
!       enddo
      
      varepsilon(j)=vareps(xd)
      H(j)=Hubble(x,xd)
      logosH(j)=H(j)/H(1)
      hta(j)=(Abs(varepsilon(j)-dqdt(2)/q(2)))

       !write(46,*) t(j),
      !+(3.d0+ 2.d0*dqdt(2)/dqdt(1)-((dqdt(1))**2.d0)/2.d0)/10.d0
       !write(*,*) t(j), 3.d0+2.d0*dqdt(2)/dqdt(1) -(dqdt(1))**2.d0/2.d0
!     Write the field values in a file (unit 98 fields.txt) 
     
!      write(98,100) t(j), (x(i), i=1,nf)
      !100   format(f20.15,<nf>(f20.15))
      write(98,*) t(j), (x(i), i=1,nf)!, (xd(i), i=1,nf)
       write(97,*) t(j), varepsilon(j),H(j)   
      prslow(j)=(VV(q(1))/(8*pi**2.d0*(3-varepsilon(j))*varepsilon(j)))
      write(90,*) t(j),
     +  prslow(j)!,(VV(q(1))/(8*pi**2.d0*(3-varepsilon(j))*varepsilon(j)))
!      write(*,*) t(j), x(1)!,x(2) 
       write(88,*) t(j), varepsilon(j),hta(j)   

       if (varepsilon(j).gt.1.0) exit
      enddo
      timestep2=j-1
      print*, "N_End of Inflation=",timestep2*dt
       !read(*,*)
      
      fmt1='(1x,a20,1x,f20.15)'
      char1="N_End of Inflation:"
      write(99,fmt1) TRIM(ADJUSTL(char1)), t(timestep2)
      !print*, timestep2
      !read(*,*)
!     Construction of kmode, connection to efolds, perturbation initial conditions
!     Unit "kmode.txt" 


 !      if (t(i).le.5.d0) then
       
 !       cq=1
 !     call kic(cq,kstar,t,timestep,timestep2,H,logosH,ks,xx,
 !    +xxd,yl0_r,yl0_i,dyl0_r,dyl0_i)
  !    endif

  !    do i=1,timestep2
      ! if (t(i).gt.0.d0) then
      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
       n_back=cq*kstar

      ! exei bgei h back - na ftiakso normalization   -273
       !call back(timestep,t,n_back,dt,xx,xd,pr_n,prk_n,k_step2,
      !+varepsilon,H,ks,norma_back,timestep2)
      norma_back=1.8d-9
       
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!           
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                  
      
      
      call kic(cq,kstar,t,timestep,timestep2,H,logosH,ks,xx,
     +xxd,yl0_r,yl0_i,dyl0_r,dyl0_i)
       
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    PERTURBATIONS  FIELDS                   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! if(flag1.eq.1) then
      char1="C_q used:"
      write(99,'(1x,a20,1x,f20.1)') TRIM(ADJUSTL(char1)), cq
      
      
      do j =1 ,timestep2
      write(96,*) t(j),ks(j) !,ks_n(j)
      enddo 
       !kfinal=ks(timestep2)
      
      !write(93,*) 0.d0,2.1d-9,5.d-2 !242 grammi

      
!     Construction of Bardeen initial conditions
!     Unit "bardeen_initial.txt"        
      
      call bardeednic(t,timestep,timestep2,varepsilon,H,
     + xx,xxd,yl0_r,dyl0_r,ks,ps0_r,dps0_r)    
      
      call bardeednic(t,timestep,timestep2,varepsilon,H,
     + xx,xxd,yl0_i,dyl0_i,ks,ps0_i,dps0_i) 
      
      !do j=1,timestep2
       !write(95,"(5(1x,f20.15))") 
       !+ t(j),ps0_r(j),ps0_i(j),dps0_r(j),dps0_i(j)
      !enddo
      
!     Solution of linear perturbations+fields+Bardeen
      j_bardeen=1+2*nf
      j_dbardeen=2+4*nf  
      
      DO k=2,timestep2,k_step!200,200!

      kmode=ks(k)
      parameters(1)=kmode
      
      it=k ! Initial time
      !if ((kmode/cq).le.kstar) i_norm=it
             !,it
              
              !print*, it*dt
      
      do i=1,nf
!     Real part:
      p(i)=xx(i,it)
      p(i+nf)=yl0_r(i,it)
      p(j_bardeen+i)=xxd(i,it)
      p(j_bardeen+i+nf)=dyl0_r(i,it)
!     Imafinary part:
      r(i)=xx(i,it)
      r(i+nf)=yl0_i(i,it)
      r(j_bardeen+i)=xxd(i,it)
      r(j_bardeen+i+nf)=dyl0_i(i,it)
      enddo


      do i=1,nf
!     Real part:
      yf_r(i) =p(i)
      yl_r(i) =p(i+nf)
      dyf_r(i)=p(j_bardeen+i)
      dyl_r(i)=p(j_bardeen+i+nf)
!     Imafinary part:
      yf_i(i) =r(i)
      yl_i(i) =r(i+nf)
      dyf_i(i)=r(j_bardeen+i)
      dyl_i(i)=r(j_bardeen+i+nf)
      enddo

!     Bardeen Initial Conditions
!     Real part:
      p(j_bardeen) =ps0_r(it)
      p(j_dbardeen)=dps0_r(it)
      yb_r(1) =p(j_bardeen)
      dyb_r(1)=p(j_dbardeen)
!     Imafinary part:
      r(j_bardeen) =ps0_i(it)
      r(j_dbardeen)=dps0_i(it)
      yb_i(1) =r(j_bardeen)
      dyb_i(1)=r(j_dbardeen)
 !     print*,"kmode=",kmode
 !     print*, "pedia_r_ic:",p(1),p(2)
 !     print*, "deltaf_ic:",p(3), p(4)
 !     print*, "bardeen_ic:", p(5)
 !     print*, "d delta_ic:",p(8), p(9),p(10)
 !     print*, t(it)
      !call derivstwo(m,t(it),p,dpdt)
      !call rk4two(p,dpdt,m,t(it+1),dt)
 !      call odeint(p,m,t(it),t(it+1),epsi,dt)
      
      !call derivstwo(m,t(it),r,drdt)
      !call rk4two(r,drdt,m,t(it+1),dt)
 !     call odeint(r,m,t(it),t(it+1),epsi,dt)
      !print*, "d pedia_r_ic:",p(6),p(7)
      
      !print*, "d bardeen_ic:", p(10)
!      read(*,*)
c      do i=1,nf
c!     Real part:
c      yf_r(i) =p(i)
c      yl_r(i) =p(i+nf)
c      dyf_r(i)=p(j_bardeen+i)
c      dyl_r(i)=p(j_bardeen+i+nf)
c!     Imafinary part:
c      yf_i(i) =r(i)
c      yl_i(i) =r(i+nf)
c      dyf_i(i)=r(j_bardeen+i)
c      dyl_i(i)=r(j_bardeen+i+nf)
c      enddo

!     Bardeen
      yb_r(1) =p(j_bardeen)
      dyb_r(1)=p(j_dbardeen)
!     Imafinary part:
      yb_i(1) =r(j_bardeen)
      dyb_i(1)=r(j_dbardeen)
      
      zeta_r=zeta(yf_r,dyf_r,yl_r,yb_r(1))
      zeta_i=zeta(yf_i,dyf_i,yl_i,yb_i(1))
      pr(it)=kmode**3.d0*(zeta_r**2.d0+zeta_i**2.d0)/(2.d0*pi**2.d0)  
      
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11
      isocurv_r=isocurv(yf_r,dyf_r,yl_r,yb_r(1))
      isocurv_i=isocurv(yf_i,dyf_i,yl_i,yb_i(1))
      p_isocurv(it) =kmode**3.d0*(isocurv_r**2.d0+isocurv_i**2.d0)
     +/(2.d0*pi**2.d0)!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      do j=it,timestep2-1
          
      !call derivstwo(m,t(j-1),p,dpdt)
      !call rk4two(p,dpdt,m,t(j),dt)
       call odeint(p,m,t(j),t(j+1),epsi,dt)


      !call derivstwo(m,t(j-1),r,drdt)
      !call rk4two(r,drdt,m,t(j),dt)
       call odeint(r,m,t(j),t(j+1),epsi,dt)



      do i=1,nf
!     Real part:
      yf_r(i) =p(i)
      yl_r(i) =p(i+nf)
      dyf_r(i)=p(j_bardeen+i)
      dyl_r(i)=p(j_bardeen+i+nf)
!     Imafinary part:
      yf_i(i) =r(i)
      yl_i(i) =r(i+nf)
      dyf_i(i)=r(j_bardeen+i)
      dyl_i(i)=r(j_bardeen+i+nf)
      enddo

!     Bardeen
      yb_r(1) =p(j_bardeen)
      dyb_r(1)=p(j_dbardeen)
!     Imafinary part:
      yb_i(1) =r(j_bardeen)
      dyb_i(1)=r(j_dbardeen)
      
      zeta_r=zeta(yf_r,dyf_r,yl_r,yb_r(1))
      zeta_i=zeta(yf_i,dyf_i,yl_i,yb_i(1))

      pr(j)=kmode**3.d0*(zeta_r**2.d0+zeta_i**2.d0)/(2.d0*pi**2.d0)
      
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11
      isocurv_r=isocurv(yf_r,dyf_r,yl_r,yb_r(1))
      isocurv_i=isocurv(yf_i,dyf_i,yl_i,yb_i(1))
      p_isocurv(j) =kmode**3.d0*(isocurv_r**2.d0+isocurv_i**2.d0)
     +/(2.d0*pi**2.d0)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !   +/17.d0
      ! write(*,*) t(j), p(1)
!950    format (5(1x,f10.6))  
       !write(55,*) t(j),p(1),p(2)!,p(4),p(5) 
       !write(54,*) t(j),p(5),p(10)!,p(4),p(5) 
      ! write(53,*) t(j),p(1),p(2)!,p(4),p(5)
      
      temp_pr=100.d0*abs((pr(j)-pr(j-1))/pr(j-1))
       !write(44,*) t(j-1), dsqrt(pr(j-1)/17.d0**2) ,kmode
      ! if (t(i).le.n_star) i_norm=it !n_star=5
!      call Vx1(Vx,p(1))
!      write(70,*) t(j) ,Vx
!       do jj=1,nf
!950    format (5(1x,f10.6)) !=> 5 stiles me ena keno kai 10 psifia ek ton opoion 6 dekadika  
!       call Vxx1(Vxx,p(1))
!        write(69,950) t(j), vxx
!       enddo

      
       if (temp_pr.lt.dpr) exit
      
      
       !if (k.eq.10)
      !write(54,*) t(j), dsqrt(pr(j-1))
!       if(k.eq.801) then!821
!       write(47,*) t(j-1), dsqrt(pr(j-1)/900.d0) ,kmode
!      ! write(47,*) t(j-1), dsqrt(pr(j-1)/900.d0) ,kmode
!      write(45,*)  t(j-1),
!     +1- !1.d0/(4.d0*kmode**2.d0*((zeta_r**2.d0+zeta_i**2.d0))**2.d0)
!     + 1.d0/ (4.d0*kmode**2*p(4)**4.d0*
!     + ((zeta_r**2.d0+zeta_i**2.d0))**2.d0 
!     + *exp(4.d0*t(j-1)))!*17.d0**2.d0
!       !print*,  dsqrt(pr(j-1))
!       endif
!      print*, j
      enddo
      
      !Q_r=Q_ms(yf_r,dyf_r,yl_r,yb_r(1))
      !Q_i=Q_ms(yf_i,dyf_i,yl_i,yb_i(1))
      !Q_abs=dsqrt(Q_r**2.d0+Q_i**2.d0)
!      write(94,*) t(k), Q_abs
     

       
!      print*, t(it),pr(j-1),kmode
      prk(k)=pr(j-1)
      prk_isocurv(k)=p_isocurv(j-1)
       !if (j.gt.2000) then
       print*, j-it ,prk(k)  
       !endif
   
        norma=prk(2)
      
      ENDDO
      
       do k=2,timestep2-100,k_step
       ! if(norma_back.gt.0.d0) then
       prk(k)=prk(k)*norma_back/norma
       !else
        !prk(k)=prk(k)!*1.8d-9/norma
                
       !endif
      !!ks(k)=kstar/(cq*H(1)*dexp(t(i_norm)))
       ks_norm=kstar*cq*ks(k)/(ks(1))
       write(93,*) t(k) +n_back,prk(k),ks_norm
       prk_isocurv(k)=prk_isocurv(k)*norma_back/norma
       write(87,*) t(k) +n_back,prk_isocurv(k),ks_norm
      !kk(k)=kstar*cq*ks(k)/(ks(1))
       enddo
       !kfinal=kfinal*kstar*cq/(ks(1))
        kfinal=int(k/k_step)
       !print*, kfinal
       print*, "Iterations=",kfinal
 !      close (16)
      
      char1="P_R (peak):"
      write(99,fmt1)TRIM(ADJUSTL(char1)), MAXVAL(prk)
      print*, "P_R(peak)=", MAXVAL(prk) ,
     +"Enchancment=", MAXVAL(prk)/prk(2)
       ! read(*,*)
      
      ! endif
      !###########################################################################
      !###########################################################################
      !GRAVITATIONAL WAVES: 
       IF (FLAG.EQ.2) THEN
            
      call GW(kmode_mon,result_mon)

      
      !write(89,*)  kmode_mon,result_mon
     
      
      
       !###########################################################################
       !###########################################################################   
      ENDIF
      

! ABUNDANCE OF PRIMORDIAL BLACK HOLES- PRESS SCHECTER- PEAK THEORY 
      IF (FLAG.EQ.3) THEN

      write(99,'(1x,a20,1x,f20.3)') "_________________________"

      open(unit=12,file="n_prz_kmode.txt",status="old")
      write(99,'(1x,a20,1x,f20.5)') "<<PBH ABUNDANCE>>"
        char1="dc:"
      write(99,'(1x,a20,1x,f20.3)')TRIM(ADJUSTL(char1)), dc
       
      do i=1,kfinal
       read(12,*) n_pbh(i), pr_pbh(i), kk(i)
       !print*,  n_pbh(i), pr_pbh(i), kk(i)
      enddo
       
      call ps(kfinal,kk,pr_pbh,dc,opbh_ps,mpbh_ps,mpeak_ps,ppeak,
     +vita_ps,s2m_ps,om_ps)
     
      char1="Abundance (PS):"
      !write(99,*) opbh
      write(99,'(1x,a20,1x,f20.5)') TRIM(ADJUSTL(char1)), opbh_ps
      

      ! write(99,*) "M_pbh(peak):"
      ! write(99,*) mpeak
      char1="Mass_pbh(peak):"
       write(99,*) TRIM(ADJUSTL(char1)), mpeak_ps
      ! Print abundance, k , vita
       do j=1,kfinal
       !write(...,*) kk(j),vita(j),s2m(j)
       write(92,*) mpbh_ps(j),om_ps(j),kk(j)
       enddo
         
   
      
      call pt(kfinal,kk,pr_pbh,dc,opbh_pt,mpbh_pt,mpeak_pt,
     +ppeak_pt,vita_pt,s2m_pt,om_pt)
      
      char1="Abundance (PT):"
      !write(99,*) opbh
      write(99,'(1x,a20,1x,f20.3)') TRIM(ADJUSTL(char1)), opbh_pt
      

!!       !print*, MAXVAL(prk)
      !  write(99,*) "Pr_k:"
      !write(99,*) MAXVAL(prk)
      !

      char1="Mass_pbh(peak):"
       write(99,*) TRIM(ADJUSTL(char1)), mpeak_pt
    
      ! Print abundance, k , vita
       do j=1,kfinal
       !write(47,*) kk(j),vita_pt(j),s2m_pt(j)
       write(91,*) mpbh_pt(j),om_pt(j),kk(j)
       enddo   
      ENDIF
      END
      
!##############################################################

 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      FUNCTION AND SUBROUTINES                !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
      
 
      function VV(x)
      use global 
      implicit none
      
      real*8 VV,x(nf)
      REAL*8 a,V0
      real*8 pi
      real*8 f1,f2,f0
      real*8 c0, c1, c2, c3,c4,c5,c6,c7,c8,f8,c9
      a=1.d0
      v0=1.d0
      
      pi=dacos(-1.d0)

      
!     Include user-provided potential expression
      include 'potential.inc'
      

      end
 
 
 

      subroutine Vx1(Vx,x)
      use global
      implicit none
      integer i
      real*8 Vxii,x(nf),Vx(nf)
      do i=1,nf
      Vx(i)=Vxii(x,i)
      enddo
      return
      end

      subroutine Vxx1(Vxx,x)
      use global
      implicit none
      integer i,j
      real*8 Vxxij,x(nf),Vxx(nf,nf)
      do i=1,nf
      do j=1,nf
      Vxx(i,j)=Vxxij(x,i,j)
      enddo
      enddo
      return
      end


      FUNCTION Vxii(x,ii)
      use global
      IMPLICIT NONE
      INTEGER NTAB,ii
      REAL*8 err,h,CON,CON2,BIG,SAFE,x(nf),xp(nf),xm(nf),Vxii,vv
      PARAMETER (CON=1.4d0,CON2=CON*CON,BIG=1.d30,NTAB=10,SAFE=2.d0)
      INTEGER i,j,k
      REAL*8 errt,fac,hh,a(NTAB,NTAB)
      h=0.0001d0
      if(h.eq.0.d0) pause 'h must be nonzero in dfridr'
      hh=h

      do k=1,nf
      xp(k)=x(k)
      xm(k)=x(k)
      enddo
      xp(ii)=x(ii)+hh
      xm(ii)=x(ii)-hh
      a(1,1)=(VV(xp)-VV(xm))/(2.d0*hh)
      err=BIG
      do 12 i=2,NTAB
        hh=hh/CON
          do k=1,nf
          xp(k)=x(k)
          xm(k)=x(k)
          enddo
          xp(ii)=x(ii)+hh
          xm(ii)=x(ii)-hh
        a(1,i)=(VV(xp)-VV(xm))/(2.d0*hh)
        fac=CON2
        do 11 j=2,i
          a(j,i)=(a(j-1,i)*fac-a(j-1,i-1))/(fac-1.d0)
          fac=CON2*fac
          errt=max(abs(a(j,i)-a(j-1,i)),abs(a(j,i)-a(j-1,i-1)))
          if (errt.le.err) then
            err=errt
            Vxii=a(j,i)
          endif
11      enddo
        if(abs(a(i,i)-a(i-1,i-1)).ge.SAFE*err)return
12    enddo
      return
      END
      
      
      FUNCTION Vxxij(x,ii,jj)
      use global
      IMPLICIT NONE
      INTEGER NTAB,ii,jj
      REAL*8 Vxxij,err,h,CON,CON2,BIG,SAFE,x(nf),xp(nf),xm(nf),Vxii
      PARAMETER (CON=1.4d0,CON2=CON*CON,BIG=1.d30,NTAB=10,SAFE=2.d0)
      INTEGER i,j,k
      REAL*8 errt,fac,hh,a(NTAB,NTAB)
      h=0.0001d0
      if(h.eq.0.d0) pause 'h must be nonzero in dfridr'
      hh=h

      do k=1,nf
      xp(k)=x(k)
      xm(k)=x(k)
      enddo
      xp(ii)=x(ii)+hh
      xm(ii)=x(ii)-hh
      a(1,1)=(Vxii(xp,jj)-Vxii(xm,jj))/(2.d0*hh)
      err=BIG
      do 12 i=2,NTAB
        hh=hh/CON
          do k=1,nf
          xp(k)=x(k)
          xm(k)=x(k)
          enddo
          xp(ii)=x(ii)+hh
          xm(ii)=x(ii)-hh
        a(1,i)=(Vxii(xp,jj)-Vxii(xm,jj))/(2.d0*hh)
        fac=CON2
        do 11 j=2,i
          a(j,i)=(a(j-1,i)*fac-a(j-1,i-1))/(fac-1.d0)
          fac=CON2*fac
          errt=max(abs(a(j,i)-a(j-1,i)),abs(a(j,i)-a(j-1,i-1)))
          if (errt.le.err) then
            err=errt
            Vxxij=a(j,i)
          endif
11      enddo
        if(abs(a(i,i)-a(i-1,i-1)).ge.SAFE*err)return
12    enddo
      return
      END
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!    cosmological functions      !!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         function Hubble(x,xd)
      use global
      implicit none
      real*8 VV,Hubble!,vareps,x(nf),xd(nf)
      integer j,k
      real*8 sd2,epsil,x(nf),ll(nf,nf),xd(nf),ll_inv(nf,nf)
         call metric(ll,ll_inv,x)
           sd2=0.d0
      do j=1,nf
      do k=1,nf
      sd2=sd2+ll(j,k)*xd(j)*xd(k)
      enddo
      enddo
      epsil=0.5d0*sd2

      Hubble=VV(x)/(3.d0-epsil )
      Hubble=dsqrt(Hubble)

      end 
      
      
      function vareps(xd)
      use global
      implicit none
      integer j,k
      real*8 sd2,vareps,x(nf),ll(nf,nf),ll_inv(nf,nf),xd(nf)
         call metric(ll,ll_inv,x)
           sd2=0.d0
      do j=1,nf
      do k=1,nf
      sd2=sd2+ll(j,k)*xd(j)*xd(k)
      enddo
      enddo
      vareps=0.5d0*sd2
      
      end    
      
      
      SUBROUTINE xic(x,xd)
      use global
      implicit none
      integer b,i
      real*8 x(nf),xd(nf),Vx(nf),dv,V(nf),ll(nf,nf)
      real*8 ll_inv(nf,nf),VV,kappa
      
      do i=1,nf
     
      V(i)=VV(x)
      call Vx1(Vx,x)
      call metric(ll,ll_inv,x)
        dv=0.d0
       do b=1,nf
         dv= dv+ll_inv(i,b)*Vx(b)
       enddo
       
      xd(i)=-dv/V(i)
  
      
      !read(*,*)
      enddo
      
      
      return
      end subroutine xic
      
      subroutine kic(cq,kstar,t,timestep,timestep2,H,logosH,ks,xx,
     +xxd,lx_r,lx_i,dlx_r,dlx_i)
      use global
      implicit none
      integer i,timestep,timestep2,j,a,k
      real*8 temp,cq,Hubble,t(timestep),H(timestep)
      real*8 ks(timestep),logosH(timestep),ks_n(timestep)
      real*8 kstar,lx_r(nf,timestep),lx_i(nf,timestep)
      real*8 dlx_r(nf,timestep),dlx_i(nf,timestep)
      real*8 ll(nf,nf),ll_inv(nf,nf),lld(nf,nf,nf)
      real*8 xx(nf,timestep),xxd(nf,timestep)
      real*8 x(nf),xd(nf),laa,laad,laa1,laad1
      real*8 llaa(nf),llaad(nf)
    
      do i=1,timestep2
!      ks(i)=kstar*(dexp(t(i)-t(1))*logosH(i))
      ks(i)=cq*dexp(t(i))*H(i)
      !ks_n(i)=dexp(t(i))*H(i)
      enddo
      
      do a=1,nf
          llaad(a)=abs(ll(a,a))
      enddo
      
        do a=1,nf
      llaad(a)=abs(lld(a,a,a))*xd(a)
      enddo
    
      do j=1,nf
      do i=1,timestep2
      
      do k=1,nf
      x(k)=xx(k,i)
      xd(k)=xxd(k,i)
      enddo
      
      call metric(ll,ll_inv,x)
      call dmetric(lld,x)
      
      laa=0.d0
      do a=1,1!1,nf
      laa=laa+abs(ll(a,a))
      enddo      
     
      laad=0.d0
      do a=1,1!1,nf
      laad=laad+abs(lld(a,a,a))*xd(a)
      enddo
      
c      laa1=0.d0
c      do a=2,2!1,nf
c      laa1=laa1+abs(ll(a,a))
c      enddo 
c       laad1=0.d0
c      do a=2,2!1,nf
c      laad1=laad1+abs(lld(a,a,a))*xd(a)
c      enddo
      
      
      temp=dsqrt(1.d0/2.d0)/(dsqrt(laa*ks(i)) *dexp(t(i))) 
      lx_r(1,i)=temp
      lx_i(1,i)=0.d0
      dlx_r(1,i)=-temp*(1.d0+laad/(2.d0*laa))
      dlx_i(1,i)=-temp*(ks(i)/(dexp(t(i))*H(i)))
      
c            temp=dsqrt(1.d0/2.d0)/(dsqrt(laa1*ks(i)) *dexp(t(i))) 
c      lx_r(2,i)=temp
c      lx_i(2,i)=0.d0
c      dlx_r(2,i)=-temp*(1.d0+laad1/(2.d0*laa1))
c      dlx_i(2,i)=-temp*(ks(i)/(dexp(t(i))*H(i)))
      enddo
       enddo


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c      do j=1,nf
c      do i=1,timestep2
c      lx_r(j,i)=1.d0/(dsqrt(2*ks(i)) *dexp(t(i)) )
c      lx_i(j,i)=0.d0
c      dlx_r(j,i)=-1.d0/(dsqrt(2*ks(i))*dexp(t(i)))
c      dlx_i(j,i)=-(1.d0/(dsqrt(2*ks(i))*dexp(t(i))))*
c     +(ks(i)/ (dexp(t(i))*H(i)))
c      enddo
c      enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!1
      return
      end subroutine kic
      
      
      
      subroutine k_plus(cq,kstar,t,timestep,timestep2,H,logosH,
     +kplus,s,d,xx,xxd,lx_r,lx_i,dlx_r,dlx_i)
      use global
      implicit none
      integer i,timestep,timestep2,j,a,k
      real*8 temp,cq,Hubble,t(timestep),H(timestep)
      real*8 ks(timestep),logosH(timestep),kplus(timestep)
      real*8 kstar,lx_r(nf,timestep),lx_i(nf,timestep)
      real*8 dlx_r(nf,timestep),dlx_i(nf,timestep)
      real*8 ll(nf,nf),lld(nf,nf,nf),ll_inv(nf,nf)
      real*8 xx(nf,timestep),xxd(nf,timestep)
      real*8 x(nf),xd(nf),laa,laad,s,d
    
      do i=1,timestep2
       !ks(i)=cq*dexp(t(i))*H(i)
      kplus(i)=cq*dexp(t(i))*H(i)*sqrt(3.d0/4.d0)*(s+d)
      enddo
 
      do j=1,nf
      do i=1,timestep2
      
      do k=1,nf
      x(k)=xx(k,i)
      xd(k)=xxd(k,i)
      enddo
      
      call metric(ll,ll_inv,x)
      call dmetric(lld,x)
      
      laa=0.d0
      do a=1,nf
      laa=laa+abs(ll(a,a))
      enddo      
      
      laad=0.d0
      do a=1,nf
      laad=laad+abs(lld(a,a,a))*xd(a)
      enddo
      
      temp=dsqrt(1.d0/2.d0)/(dsqrt(laa*kplus(i)) *dexp(t(i))) 
      lx_r(j,i)=temp
      lx_i(j,i)=0.d0
      dlx_r(j,i)=-temp*(1.d0+laad/(2.d0*laa))
      dlx_i(j,i)=-temp*(kplus(i)/(dexp(t(i))*H(i)))
      enddo
       enddo

      return
      end subroutine k_plus
      
      
      subroutine k_minus(cq,kstar,t,timestep,timestep2,H,logosH,
     +kminus,s,d,xx,xxd,lx_r,lx_i,dlx_r,dlx_i)
      use global
      implicit none
      integer i,timestep,timestep2,j,a,k
      real*8 temp,cq,Hubble,t(timestep),H(timestep)
      real*8 ks(timestep),logosH(timestep),kminus(timestep)
      real*8 kstar,lx_r(nf,timestep),lx_i(nf,timestep)
      real*8 dlx_r(nf,timestep),dlx_i(nf,timestep)
      real*8 ll(nf,nf),lld(nf,nf,nf),ll_inv(nf,nf)
      real*8 xx(nf,timestep),xxd(nf,timestep)
      real*8 x(nf),xd(nf),laa,laad,s,d
    
      do i=1,timestep2
       !ks(i)=cq*dexp(t(i))*H(i)
      kminus(i)=cq*dexp(t(i))*H(i)*sqrt(3.d0/4.d0)*(s-d)
      enddo
 
      do j=1,nf
      do i=1,timestep2
      
      do k=1,nf
      x(k)=xx(k,i)
      xd(k)=xxd(k,i)
      enddo
      
      call metric(ll,ll_inv,x)
      call dmetric(lld,x)
      
      laa=0.d0
      do a=1,nf
      laa=laa+abs(ll(a,a))
      enddo      
      
      laad=0.d0
      do a=1,nf
      laad=laad+abs(lld(a,a,a))*xd(a)
      enddo
      
      temp=dsqrt(1.d0/2.d0)/(dsqrt(laa*kminus(i)) *dexp(t(i))) 
      lx_r(j,i)=temp
      lx_i(j,i)=0.d0
      dlx_r(j,i)=-temp*(1.d0+laad/(2.d0*laa))
      dlx_i(j,i)=-temp*(kminus(i)/(dexp(t(i))*H(i)))
      enddo
       enddo

      return
      end subroutine k_minus
      
      
 !################################################     
      subroutine back(timestep,t,n_back,dt,xx,xd,prk,pr,k_step2,
     +varepsilon,H,ks,norma_back,timestep2)
      use global
      implicit none
   
      integer timestep, timestep_back,j_bardeen,j_dbardeen,nf2,m,i,j,it
      integer k_step,k,k_step2,time_int,iii,timestep2!,i_norm
      parameter(nf2=2*nf,m=4*nf+2)
      real*8 cq,dt
       real*8 q(nf2),dqdt(nf2),x(nf),xd(nf),p(m),dpdt(m)
      real*8 t(timestep),xx(nf,timestep),xxd(nf,timestep)
      real*8 ks(timestep),r(m),drdt(m)
      real*8  varepsilon(timestep),H(timestep),logosH(timestep)
      real*8 yf_r(nf),yl_r(nf),yb_r(1)
      real*8 dyf_r(nf),dyl_r(nf),dyb_r(1)
      real*8 yf_i(nf),yl_i(nf),yb_i(1)
      real*8 dyf_i(nf),dyl_i(nf),dyb_i(1)
      real*8 yl0_r(nf,timestep),yl0_i(nf,timestep)
      real*8 dyl0_r(nf,timestep),dyl0_i(nf,timestep)
      real*8 ps0_r(timestep),dps0_r(timestep)
      real*8 ps0_i(timestep),dps0_i(timestep)
      real*8 kmode,norma,prk(timestep),kstar,pi
      real*8 Q_ms,Q_r,Q_i,Q_abs,zeta,zeta_r,zeta_i
      real*8 pr(timestep),dpr,temp_pr,n_back
      real*8 epsi,norma_back
      
      parameter(kstar=0.05d0,k_step=50,epsi=1.d-5,dpr=1.d-6)
      
      pi=dacos(-1.d0)
       do j=1,timestep
      t(j)=0.d0+dt*(j-1)
      pr(j)=0.d0
      enddo
      
       timestep_back=int(2*n_back/dt ) 
       time_int=int(n_back/dt)!-k_step
      cq=1
      call kic(cq,kstar,t,timestep,timestep_back,H,logosH,ks,xx,
     +xxd,yl0_r,yl0_i,dyl0_r,dyl0_i)
      
      !do j =1 ,6000
      !write(*,*) t(j),ks(j) !,ks_n(j)
      !enddo 
      
     !!!!!!!!!!!!!!!!!
      
      do i=1,nf
      xx(i,1)=q(i)
      xxd(i,1)=q(i+nf)
      x(i)=q(i)
      xd(i)=q(i+nf)
      enddo  
!     Construction of Bardeen initial conditions
!     Unit "bardeen_initial.txt"        
      
           call bardeednic(t,timestep,timestep2,varepsilon,H,
     + xx,xxd,yl0_r,dyl0_r,ks,ps0_r,dps0_r)    
      
      call bardeednic(t,timestep,timestep2,varepsilon,H,
     + xx,xxd,yl0_i,dyl0_i,ks,ps0_i,dps0_i) 
      
      do j=1,timestep2
       !write(95,"(5(1x,f20.15))") 
       !+ t(j),ps0_r(j),ps0_i(j),dps0_r(j),dps0_i(j)
      enddo
      
!     Solution of linear perturbations+fields+Bardeen
      j_bardeen=1+2*nf
      j_dbardeen=2+4*nf  
      
      DO k=2,int(timestep2/10),k_step
        

      kmode=ks(k)
      parameters(1)=kmode
      
      it=k ! Initial time
      !if ((kmode/cq).le.kstar) i_norm=it
      
      do i=1,nf
!     Real part:
      p(i)=xx(i,it)
      p(i+nf)=yl0_r(i,it)
      p(j_bardeen+i)=xxd(i,it)
      p(j_bardeen+i+nf)=dyl0_r(i,it)
!     Imafinary part:
      r(i)=xx(i,it)
      r(i+nf)=yl0_i(i,it)
      r(j_bardeen+i)=xxd(i,it)
      r(j_bardeen+i+nf)=dyl0_i(i,it)
      enddo


      do i=1,nf
!     Real part:
      yf_r(i) =p(i)
      yl_r(i) =p(i+nf)
      dyf_r(i)=p(j_bardeen+i)
      dyl_r(i)=p(j_bardeen+i+nf)
!     Imafinary part:
      yf_i(i) =r(i)
      yl_i(i) =r(i+nf)
      dyf_i(i)=r(j_bardeen+i)
      dyl_i(i)=r(j_bardeen+i+nf)
      enddo

!     Bardeen Initial Conditions
!     Real part:
      p(j_bardeen) =ps0_r(it)
      p(j_dbardeen)=dps0_r(it)
      yb_r(1) =p(j_bardeen)
      dyb_r(1)=p(j_dbardeen)
!     Imafinary part:
      r(j_bardeen) =ps0_i(it)
      r(j_dbardeen)=dps0_i(it)
      yb_i(1) =r(j_bardeen)
      dyb_i(1)=r(j_dbardeen)

      !call derivstwo(m,t(it),p,dpdt)
      !call rk4two(p,dpdt,m,t(it+1),dt)
       call odeint(p,m,t(it),t(it+1),epsi,dt)
      
      !call derivstwo(m,t(it),r,drdt)
      !call rk4two(r,drdt,m,t(it+1),dt)
        call odeint(r,m,t(it),t(it+1),epsi,dt)


      do i=1,nf
!     Real part:
      yf_r(i) =p(i)
      yl_r(i) =p(i+nf)
      dyf_r(i)=p(j_bardeen+i)
      dyl_r(i)=p(j_bardeen+i+nf)
!     Imafinary part:
      yf_i(i) =r(i)
      yl_i(i) =r(i+nf)
      dyf_i(i)=r(j_bardeen+i)
      dyl_i(i)=r(j_bardeen+i+nf)
      enddo

!     Bardeen
      yb_r(1) =p(j_bardeen)
      dyb_r(1)=p(j_dbardeen)
!     Imafinary part:
      yb_i(1) =r(j_bardeen)
      dyb_i(1)=r(j_dbardeen)
      
      zeta_r=zeta(yf_r,dyf_r,yl_r,yb_r(1))
      zeta_i=zeta(yf_i,dyf_i,yl_i,yb_i(1))
      pr(it)=kmode**3.d0*(zeta_r**2.d0+zeta_i**2.d0)/(2.d0*pi**2.d0)  
      
      do j=it+1,timestep2
          
      !call derivstwo(m,t(j-1),p,dpdt)
      !call rk4two(p,dpdt,m,t(j),dt)
       call odeint(p,m,t(j-1),t(j),epsi,dt)


      !call derivstwo(m,t(j-1),r,drdt)
      !call rk4two(r,drdt,m,t(j),dt)
       call odeint(r,m,t(j-1),t(j),epsi,dt)



      do i=1,nf
!     Real part:
      yf_r(i) =p(i)
      yl_r(i) =p(i+nf)
      dyf_r(i)=p(j_bardeen+i)
      dyl_r(i)=p(j_bardeen+i+nf)
!     Imafinary part:
      yf_i(i) =r(i)
      yl_i(i) =r(i+nf)
      dyf_i(i)=r(j_bardeen+i)
      dyl_i(i)=r(j_bardeen+i+nf)
      enddo

!     Bardeen
      yb_r(1) =p(j_bardeen)
      dyb_r(1)=p(j_dbardeen)
!     Imafinary part:
      yb_i(1) =r(j_bardeen)
      dyb_i(1)=r(j_dbardeen)
      
      zeta_r=zeta(yf_r,dyf_r,yl_r,yb_r(1))
      zeta_i=zeta(yf_i,dyf_i,yl_i,yb_i(1))
      pr(j)=kmode**3.d0*(zeta_r**2.d0+zeta_i**2.d0)/(2.d0*pi**2.d0)
  !   +/17.d0
      ! write(*,*) t(j), p(1)
       ! write(55,*) t(j), p(3),p(4)
      
      temp_pr=100.d0*abs((pr(j)-pr(j-1))/pr(j-1))
       !write(44,*) t(j-1), dsqrt(pr(j-1)/17.d0**2) ,kmode
      ! if (t(i).le.n_star) i_norm=it !n_star=5


      if (temp_pr.lt.dpr) exit

      enddo
      
      Q_r=Q_ms(yf_r,dyf_r,yl_r,yb_r(1))
      Q_i=Q_ms(yf_i,dyf_i,yl_i,yb_i(1))
      Q_abs=dsqrt(Q_r**2.d0+Q_i**2.d0)
!      write(94,*) t(k), Q_abs


       
!      print*, t(it),pr(j-1),kmode
      prk(k)=pr(j-1)
       !if (j.gt.2000) then
       !print*, j-it ,prk(k)  
       !endif
   
        norma=prk(2)
     
      ENDDO
      !!!!!!!!!!!!!!!!!!!!
             do k=2,time_int-2*k_step,k_step
        prk(k)=(prk(k)/norma)*2.1d-9
            

      !write(93,*) t(k),prk(k),5.d-2*ks(k)/(ks(1))

       enddo
       do k=2,time_int,k_step
        prk(k)=(prk(k)/norma)*2.1d-9
        
       enddo
      norma_back=(prk(k)/norma)*2.1d-9
      
      return
      end subroutine back
      

      
      function Q_ms(x,xd,xl,psi)
      use global
      implicit none
      integer a,b
      real*8 x(nf),xd(nf),xl(nf),psi,Q_ms
      real*8 sd,ll(nf,nf),ll_inv(nf,nf),ds,vareps
      
      call metric(ll,ll_inv,x)

      sd=0.d0
      do a=1,nf
       do b=1,nf
        sd=sd+dsqrt(ll(a,b)*xd(a)*xd(b))
        
       enddo
      enddo
      
      ds=0.d0
      do a=1,nf
       ds=ds+xd(a)*xl(a)/(sd)
      enddo
       Q_ms=ds+sd*psi
      
      end function Q_ms
      
      
      function zeta(x,xd,xl,psi)
      use global
      implicit none
      integer a,b
      real*8 sd,ds,ll(nf,nf),ll_inv(nf,nf),x(nf),xd(nf),xl(nf),psi,zeta
      
      call metric(ll,ll_inv,x)
      !!!!!
      sd=0.d0
      do a=1,nf
       do b=1,nf
        sd=sd+(ll(a,b)*xd(a)*xd(b))
       enddo
      enddo
      sd=dsqrt(sd)
     
      ds=0.d0
      do a=1,nf
          do b=1,nf 
       ds=ds+ll(a,b)*xd(a)*xl(b)/(sd)
          enddo
      enddo
      !print*, psi,ds,sd
      !read(*,*)
      zeta=psi+ds/sd
      
      end function zeta
      
      
      function isocurv(x,xd,xl,psi)
      use global
      implicit none
      integer a,b
      real*8 sd,l_xiasti,ll(nf,nf),ll_inv(nf,nf),x(nf),xd(nf),xl(nf)
      real*8 isocurv,psi
      
      if(nf.eq.2) then
       call metric(ll,ll_inv,x) 
      sd=0.d0
      do a=1,nf
      do b=1,nf
        sd=sd+dsqrt(ll(a,b)*xd(a)*xd(b))   
      enddo
      enddo
      
      l_xiasti=1.d0
      do a=1,nf
          l_xiasti=l_xiasti*ll(a,a)
      enddo
      isocurv=xd(2)*l_xiasti*(xl(1)+xd(1)*psi)/sd 
     + + xd(1)*l_xiasti*(xl(2)+xd(2)*psi)/sd
      isocurv=isocurv/sd    
      else
          isocurv=0.d0
      endif
      
      end function isocurv
      
      
      
      
      
      subroutine derivs(n,t,q,dqdt)
      use global
      implicit none
      integer n,i,j,k,l,ii
      real*8 t,Vx,Vy,V,dV(nf),ddV(nf,nf),VV,sd2,d
      real*8 q(n),dqdt(n),x(nf),xd(nf)
      real*8 ll(nf,nf),ll_inv(nf,nf),chris(nf,nf,nf),a
      real*8 pi
      
      pi = dacos(-1.d0)
            do i=1,nf
      x(i)=q(i)
      xd(i)=q(i+nf)
      enddo 
      call metric(ll,ll_inv,x)
      call christoffel(chris,x)
           
      sd2=0.d0
      do j=1,nf
      do k=1,nf
      sd2=sd2+ll(j,k)*xd(j)*xd(k)
      enddo
      enddo
      
      
 !     call Vdot(dV)
 !     call Vddot(ddV)      
      
       call Vx1(dV,x)
      
      do j=1,nf
      dqdt(j)=q(j+nf)
      enddo      
    
      a=0.d0
      d=0
        do j=1,nf
        a=0.d0
      do k=1,nf
      do l=1,nf
       a=a+chris(k,l,j)*xd(k)*xd(l)
      enddo
      enddo
      d=0.d0
      do ii=1,nf 
      !if(j.eq.ii) then
      !d=d+(1.d0/ll(j,ii))*dv(ii)
      d=d+ll_inv(j,ii)*dV(ii)
      !endif
       !print*, ll(j,ii)
      enddo
       !dqdt(1)=1.d0
       !dqdt(2)=1.d0
       dqdt(j+nf)=-a-xd(j)*(3-0.5d0*sd2)
     + -(3-0.5d0*sd2)*(d/VV(x))      
      
      enddo

!      dqdt(3)=-(3.d0-0.5*(q(3)**2+q(4)**2))*q(3)-
!     +(3.d0- 0.5*(q(3)**2+q(4)**2.d0) )*(1.395*1d-6)**2*q(1)
!     +/(0.5d0*(1.395*1d-6)**2*q(1)**2.d0+ 
!     +0.5d0*(7*1.395*1d-6)**2*q(2)**2.d0)
!      dqdt(4)=-(3.d0-0.5*(q(3)**2+q(4)**2))*q(4)-
!     +(3.d0- 0.5*(q(3)**2+q(4)**2.d0) )*(7*1.395*1d-6)**2*q(2)
!     +/(0.5d0*(1.395*1d-6)**2*q(1)**2.d0+
!     +0.5d0*(7*1.395*1d-6)**2*q(2)**2.d0)
      
      return
      end
      
      
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!  Perturbation+ Background      !!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      subroutine derivstwo(m,t,p,dpdt)
      use global
      implicit none
      integer a,b,c,d,j_bardeen,j_dbardeen,e
      integer m,i,jj,jjj,k,j,l,ii
      real*8 ll(nf,nf),chris(nf,nf,nf)
      real*8 t,sd2,ch,ch1,par,temp,d2
      real*8 p(m),dpdt(m),x(nf),xd(nf)
      real*8 xl(nf),dxl(nf),xb(1),dxb(1)
      real*8 dChris(nf,nf,nf,nf),dV(nf),ddV(nf,nf),VV
      real*8 vareps,Hubble,lld(nf,nf,nf),kmode
      real*8 pi,ddvs,ll_inv(nf,nf)
      real*8 first,second,third,bardeen_temp
       !open(unit=2,file="bardeenterm.txt",status="unknown")
      kmode=parameters(1)
      pi = dacos(-1.d0)
     
      j_bardeen=1+2*nf
      j_dbardeen=2+4*nf 
      jj=j_bardeen
      jjj=j_dbardeen
      
      
        do i=1,nf
      x(i)=p(i)
      xl(i)=p(i+nf)      
      xd(i)=p(j_bardeen+i)
      dxl(i)=p(j_bardeen+nf+i)
!       print*, jj+nf+i,jjj!,jj+i
       enddo
      
      
      xb(1)=p(j_bardeen)
      dxb(1)=p(j_dbardeen)

      do i=1,j_bardeen
       dpdt(i)=p(j_bardeen+i)
      enddo
      
      call Vx1(dV,x)
      call Vxx1(ddV,x)
      
      
      call metric(ll,ll_inv,x)
      call christoffel(chris,x)
      call dmetric(lld,x)
      call dchristoffel(dchris,x)
      

       
      sd2=0.d0
      do a=1,nf
      do b=1,nf
      sd2=sd2+ll(a,b)*xd(a)*xd(b)
      enddo
      enddo
      
      !initializing 
      bardeen_temp=0.d0
      ch=0.d0
      ch1=0.d0
      par=0.d0
      d2=0.d0
      first=0
      second=0
      third=0
      do c=1,nf
           bardeen_temp=bardeen_temp+dV(c)*xl(c)
      enddo
       
       DO c=1,nf
       ch=0.d0
       ch1=0.d0
       
       do a=1,nf
       do b=1,nf
       ch=ch+chris(a,b,c)*xd(a)*xd(b)
       ch1=ch1+chris(a,b,c)*xd(a)*dxl(b)
       enddo
       enddo
       
   
      par=0.d0
   
      d2=0.d0
      first=0
      second=0
      third=0

      
      do a=1,nf
      do b=1,nf
      do d=1,nf
         first=first+dchris(a,b,c,d)*xd(a)*xd(b)*xl(d)
      enddo
      enddo
      enddo
      
      do d=1,nf
      do a=1,nf
         second=second+ll_inv(c,a)*ddV(a,d)*xl(d)/(VV(x)/(3-0.5d0*sd2))
      enddo
      enddo
      
      do a=1,nf
      do b=1,nf
      do d=1,nf
      do e=1,nf
         third=third+ll_inv(c,a)*lld(a,b,d)*dV(e)*ll_inv(e,b)*xl(d)/
     +(VV(x)/(3-0.5d0*sd2))
      enddo
      enddo
      enddo
      enddo  
      
      par=first+second-third
       !ANTE GAMISOU 
       
      !d2=0.d0
      do ii=1,nf
      !if(c.eq.ii) then
      d2=d2+(ll_inv(c,ii))*dv(ii)
      !endif
      enddo
      
      
      !!do a=1,nf
      !bardeen_temp=bardeen_temp+dV(c)*xl(c)
      !!enddo
      
      
       dpdt(jj+c)=-ch-xd(c)*(3-0.5d0*sd2)
     + -(3-0.5d0*sd2)*(d2/VV(x))   
   !!!!!!!!!!!!     

      
       dpdt(jj+nf+c)=-(3.d0-0.5d0*sd2)* dxl(c)
     + -2.d0*ch1-par
     + - (kmode**2.d0/( (dexp(t) **2.d0*(VV(x)/(3-0.5d0*sd2)) ) ))*xl(c)
     + + 4.d0*dxb(1)*xd(c)- 2.d0*xb(1)*(d2/(VV(x)/(3-0.5d0*sd2)))
     
    
      ENDDO   
      
       dpdt(jjj)=-(7.d0-0.5d0*sd2)*dxb(1)
     + -(2*VV(x)/((VV(x)/(3-0.5d0*sd2))) )*xb(1)
     + -((kmode**2.d0/( (dexp(t)**2.d0*(VV(x)/(3-0.5d0*sd2)) ))) )*xb(1)
     + -(bardeen_temp/(VV(x)/(3-0.5d0*sd2)))
      !print*, kmode
      return
      end 




! call metric(ll,ll_inv,x),,ll_inv(nf,nf)

      subroutine bardeednic(t,timestep,timestep2,varepsilon,H,
     + xx,xxd,xl,dxl,ks,ps0,dps0)
      use global
      implicit none
      INTEGER a,b,c,timestep,timestep2,i,j
      real*8 x(nf),xd(nf)
      real*8 xx(nf,timestep),xxd(nf,timestep)
      real*8 xl(nf,timestep),dxl(nf,timestep)
      real*8 ps0(timestep),dps0(timestep),t(timestep)
      real*8 varepsilon(timestep),H(timestep)
      real*8 ll(nf,nf),ll_inv(nf,nf)
      real*8 dV(nf)
      real*8 vareps,Hubble,lld(nf,nf,nf),ks(timestep)
      real*8 sd0,sd1,sd2,sd3,temp,ds0


      DO i=1,timestep2

      do j=1,nf
      x(j)=xx(j,i)
      xd(j)=xxd(j,i)
      enddo

      call Vx1(dV,x)
      call metric(ll,ll_inv,x)
      ! call christoffel(chris,x)
      call dmetric(lld,x)
      ! call dchristoffel(dchris,x)
!
      sd0=0.d0
      do a=1,nf
      do b=1,nf
      sd0=sd0+ll(a,b)*xxd(a,i)*dxl(b,i)
      enddo
      enddo

      ds0=0.d0
      do a=1,nf
      do b=1,nf
      ds0=ds0+ll(a,b)*xxd(a,i)*xl(b,i)
      enddo
      enddo

      sd1=0.d0
      do a=1,nf
      do b=1,nf
      do c=1,nf
       sd1=sd1+0.5d0*lld(a,b,c)*xxd(a,i)*xxd(b,i)*xl(c,i)
      enddo
      enddo
      enddo

      sd2=0
      do a=1,nf
      do c=1,nf
      sd2=sd2+3.d0*ll(a,c)*xxd(a,i)*xl(c,i)
      enddo
      enddo


      sd3=0.d0
      do c=1,nf
      sd3=sd3+dV(c)*xl(c,i)/(H(i)**2.d0)
      enddo

      temp=1.d0/(2.d0*(varepsilon(i)-(ks(i)/(dexp(t(i))*H(i)))**2.d0))

      ps0(i)=temp*(sd0+sd1+sd2+sd3)
      dps0(i)=ds0/2.d0-ps0(i)
      enddo

      return
      end subroutine bardeednic

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!     Metric ,Christoffel Encounter       !!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine metric(ll,ll_inv,x)
      use global     
      implicit none
      integer i,j,nrot
      real*8 x(nf),ll(nf,nf),ll_inv(nf,nf),lll,d(nf),v(nf,nf)
      do i=1,nf
       do j=1,nf
        ll(i,j)=lll(i,j,x)
       enddo
      enddo
      
      call jacobi(ll,nf,nf,d,v,nrot)
       do i=1,nf
       do j=1,nf
        ll(i,j)=0.d0
        ll_inv(i,j)=0.d0
      enddo  
      enddo   
      do i=1,nf
      ll(i,i)=d(i)
      ll_inv(i,i)=1.d0/d(i)
      enddo
      
      return
      end


       function lll(i,j,x)
      use global
      implicit none
      integer i,j
      real*8 x(nf),lll
!      lll=x(i)**2.d0*x(j)**3.d0
!     Default to metric_matrix(i,j) from metric.inc, but allow overriding
!     with coordinate-dependent expressions from metric_function.inc.
      lll=metric_matrix(i,j)
      include 'metric_function.inc'

      return
      end


      FUNCTION llld(jj,kk,ii,x)
      use global
      IMPLICIT NONE
      INTEGER NTAB,ii,kk,jj
      REAL*8 err,h,CON,CON2,BIG,SAFE,x(nf),xp(nf),xm(nf),llld,lll
      PARAMETER (CON=1.4d0,CON2=CON*CON,BIG=1.d30,NTAB=10,SAFE=2.d0)
      INTEGER i,j,k
      REAL*8 errt,fac,hh,a(NTAB,NTAB)
      h=0.001d0
      if(h.eq.0.d0) pause 'h must be nonzero in dfridr'
      hh=h

      do k=1,nf
      xp(k)=x(k)
      xm(k)=x(k)
      enddo
      xp(ii)=x(ii)+hh
      xm(ii)=x(ii)-hh
      a(1,1)=(lll(jj,kk,xp)-lll(jj,kk,xm))/(2.d0*hh)
      err=BIG
      do 12 i=2,NTAB
        hh=hh/CON
          do k=1,nf
          xp(k)=x(k)
          xm(k)=x(k)
          enddo
          xp(ii)=x(ii)+hh
          xm(ii)=x(ii)-hh
        a(1,i)=(lll(jj,kk,xp)-lll(jj,kk,xm))/(2.d0*hh)
        fac=CON2
        do 11 j=2,i
          a(j,i)=(a(j-1,i)*fac-a(j-1,i-1))/(fac-1.d0)
          fac=CON2*fac
          errt=max(abs(a(j,i)-a(j-1,i)),abs(a(j,i)-a(j-1,i-1)))
          if (errt.le.err) then
            err=errt
            llld=a(j,i)
          endif
11      enddo
        if(abs(a(i,i)-a(i-1,i-1)).ge.SAFE*err)return
12    enddo
      return
      END





      subroutine dmetric(lld,x)
      use global
      implicit none
      integer i,j,k
      real*8 x(nf),lld(nf,nf,nf),llld
      do i=1,nf
       do j=1,nf
        do k=1,nf
        lld(i,j,k)=llld(i,j,k,x)
        enddo
       enddo
      enddo
      return
      end


      function g(i,j,k,x)
      use global
      implicit none
      integer i,j,k,l
      real*8 g,x(nf),ll(nf,nf),ll_inv(nf,nf),lld(nf,nf,nf)
      
      
      call metric(ll,ll_inv,x)
      call dmetric(lld,x)
      g=0.d0
      do l=1,nf
       if(k.eq.l) then
       g=g+0.5d0*(1.d0/ll(k,l))*(lld(l,i,j)+lld(l,j,i)-lld(i,j,l))
       endif
      enddo
      
      
      end


      subroutine christoffel(chris,x)
      use global
      implicit none
      integer i,j,k
      real*8 x(nf)
      real*8 Chris(nf,nf,nf),g
        
      do i=1,nf
       do j=1,nf
         do k=1,nf
         
         Chris(i,j,k)=g(i,j,k,x)
         
         enddo       
       enddo
      enddo
      
      return
      end


  
      FUNCTION ggd(ll,jj,kk,ii,x)
      use global
      IMPLICIT NONE
      INTEGER NTAB,ii,kk,jj,ll
      REAL*8 err,h,CON,CON2,BIG,SAFE,x(nf),xp(nf),xm(nf),ggd,g
      PARAMETER (CON=1.4d0,CON2=CON*CON,BIG=1.d30,NTAB=10,SAFE=2.d0)
      INTEGER i,j,k
      REAL*8 errt,fac,hh,a(NTAB,NTAB)
      h=0.001d0
      if(h.eq.0.d0) pause 'h must be nonzero in dfridr'
      hh=h

      do k=1,nf
      xp(k)=x(k)
      xm(k)=x(k)
      enddo
      xp(ii)=x(ii)+hh
      xm(ii)=x(ii)-hh
      a(1,1)=(g(ll,jj,kk,xp)-g(ll,jj,kk,xm))/(2.d0*hh)
      err=BIG
      do 12 i=2,NTAB
        hh=hh/CON
          do k=1,nf
          xp(k)=x(k)
          xm(k)=x(k)
          enddo
          xp(ii)=x(ii)+hh
          xm(ii)=x(ii)-hh
        a(1,i)=(g(ll,jj,kk,xp)-g(ll,jj,kk,xm))/(2.d0*hh)
        fac=CON2
        do 11 j=2,i
          a(j,i)=(a(j-1,i)*fac-a(j-1,i-1))/(fac-1.d0)
          fac=CON2*fac
          errt=max(abs(a(j,i)-a(j-1,i)),abs(a(j,i)-a(j-1,i-1)))
          if (errt.le.err) then
            err=errt
            ggd=a(j,i)
          endif
11      enddo
        if(abs(a(i,i)-a(i-1,i-1)).ge.SAFE*err)return
12    enddo
      return
      END




      subroutine dchristoffel(dChris,x)
      use global
      implicit none
      integer i,j,k,l
      real*8 x(nf)
      real*8 dChris(nf,nf,nf,nf),ggd
        
      do i=1,nf
       do j=1,nf
         do k=1,nf
          do l=1,nf
         dChris(l,i,j,k)=ggd(l,i,j,k,x)
          enddo
         enddo       
       enddo
      enddo
      
      return
      end


     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     !!   ABUNDANCE OF PBH            !!
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     
      subroutine ps(kfinal,kk,prk,dc,opbh,mpbh,mpeak,ppeak,
     +vita,s2m,om)
      implicit none
      INTEGER*4 j,i,kfinal,jj
      real*8 kk(kfinal),prk(kfinal),mpbh(kfinal),s2(kfinal)
       ! real*8 parameters(20)
        real*8 vita(kfinal),s2m(kfinal),om(kfinal), kktemp(kfinal)
        real*8 kpeak,kl,ww,stemp,msun,dc,opbh,Gammaa,a3,pi,gt
        real*8 dvita,nvita,gamma,s2mm!,parameter(20)
        real*8 mpeak,ppeak
      ! parameter(msun=1.988d0*1.d33)!,gt=106.75d0,gamma=0.2d0,dvita=0.0001d0,
             !+nvita=1000000)
      msun=1.988d33
      pi = dacos(-1.d0)
      gt=106.75d0
      gamma=0.2d0
      dvita=0.0001
      nvita=1000000
         do j=1,kfinal
         
       kktemp(j)=kk(j)
!       kk(j)=dexp(nfold(j)-63.5d0)*0.05d0*10.d-6
       enddo


      !! Sigma(M(k)) apo (2.5) Ballesteros-arxiv 1709.05565v2     !!
      do i=1,kfinal
      kpeak=kk(i)

      do j=1,kfinal
      kl=kk(j)/kpeak
      ww=dexp(-0.5d0*kl**2.d0)
      s2(j)=(16.d0*kl**4.d0*prk(j)*ww**2.d0)/(81.d0*kk(j))
 !    
      enddo

      stemp=0.5d0*(kk(2)-kk(1))*(s2(2)+s2(1))

      do j=2,kfinal
      stemp=stemp+0.5d0*(kk(j)-kk(j-1))*(s2(j)+s2(j-1))
      enddo

      s2m(i)=dsqrt(stemp)
      enddo

      !! Ypologismos vita(M(k)) apo (2.4) Ballesteros-arxiv 1709.05565v2    !!

      do i=1,kfinal
      a3=dc**2.d0/(2.d0*s2m(i)**2.d0)
      vita(i)=Gammaa(a3)/(2.d0*dsqrt(pi))
      enddo
      !!  Ypologismos M(k) apo (2.3) Ballesteros-arxiv 1709.05565v2         !!
      do i=1,kfinal

      stemp=10.d18*gamma/0.2d0
      stemp=stemp*(gt/106.75d0)**(-1.d0/6.d0)
      stemp=stemp*(kk(i)/(7.d0*1.d13))**(-2.d0)

      mpbh(i)=stemp!/msun
      enddo

      !! logos apo (2.7) Ballesteros-arxiv 1709.05565v2     !!

      do i=1,kfinal
      stemp=vita(i)/(8.d0*10.d-16)
      stemp=stemp*(gamma/0.2d0)**1.5d0
      stemp=stemp*(gt/106.75d0)**(-1.d0/4.d0)
      stemp=stemp*(mpbh(i)/10.d18)**(-0.5d0)
      om(i)=stemp
      enddo

      ! opbh

      do j=1,kfinal
      s2(j)=2.d0*om(j)/kk(j)
      enddo

      stemp=0.5d0*(kk(2)-kk(1))*(s2(2)+s2(1))

      do j=2,kfinal
      stemp=stemp+0.5d0*(kk(j)-kk(j-1))*(s2(j)+s2(j-1))
       enddo

      opbh=stemp
      
 !!     write(57,*) "Nstar:"
 !!     write(57,*) nnstar
 !     write(57,*) "Abundance:"
  !    write(57,*) opbh
      

       ppeak= MAXVAL(prk)
 !     write(57,*) "Pr_k:"
 !     write(57,*) MAXVAL(prk)
      
      s2mm=MAXVAL(om)
      do j=1,kfinal
      if(om(j).ge.s2mm) then
       jj=j
      endif
      enddo
      mpeak=mpbh(jj)/msun
      !print*,mpbh(jj)/msun
 !     write(57,*) "M_pbh(peak):"
 !     write(57,*) mpbh(jj)/msun
       
      
      
      ! Print abundance, k , vita
       do j=1,kfinal
       mpbh(j)=mpbh(j)/msun
 !      write(59,*) kk(j),vita(j),s2m(j)
!       write(58,*) mpbh(j)/msun,om(j),kk(j)
       enddo
       return
       end


      subroutine pt(kfinal,kk,prk,dc,opbh_pt,mpbh_pt,mpeak_pt,
     +ppeak_pt,vita_pt,s2m,om_pt)
      implicit none
      INTEGER*4 j,i,kfinal,jj
      real*8 kk(kfinal),prk(kfinal),mpbh_pt(kfinal),s2(kfinal)
      real*8 vita_pt(kfinal),s2m(kfinal),om_pt(kfinal), kktemp(kfinal)
      real*8 kpeak,kl,ww,stemp,msun,dc,opbh_pt,Gammaa,a3,pi,gt
      real*8 dvita,nvita,gamma,s2mm,ktemp
      real*8 npeak(kfinal),k2m(kfinal),k2s(kfinal)!k2(kfinal),
      real*8 mpeak_pt,ppeak_pt
      pi = dacos(-1.d0)
      
      msun=1.988d33
      gt=106.75d0
      gamma=0.2d0
      dvita=0.0001
      nvita=1000000
        
       do j=1,kfinal
       kktemp(j)=kk(j)
!       kk(j)=dexp(nfold(j)-63.5d0)*0.05d0*10.d-6
       enddo


      !! Sigma(M(k)) apo (2.5) Ballesteros-arxiv 1709.05565v2     !!
      do i=1,kfinal
      kpeak=kk(i)

      do j=1,kfinal
      kl=kk(j)/kpeak
      ww=dexp(-0.5d0*kl**2.d0)
      s2(j)=(16.d0*kl**4.d0*prk(j)*ww**2.d0)/(81.d0*kk(j))
 !    
      enddo

      stemp=0.5d0*(kk(2)-kk(1))*(s2(2)+s2(1))

      do j=2,kfinal
      stemp=stemp+0.5d0*(kk(j)-kk(j-1))*(s2(j)+s2(j-1))
      enddo

      s2m(i)=dsqrt(stemp)
      enddo
      !! ??????????? ?? k   !!
      do i=1,kfinal
      kpeak=kk(i)
        do j=1,kfinal
      kl=kk(j)/kpeak
      ww=dexp(-0.5d0*kl**2.d0)
      k2s(j)=(16.d0*kl**4.d0*prk(j)*kk(j)**2.d0*ww**2.d0)/(81.d0*kk(j))
 !    
      enddo
            ktemp=0.5d0*(kk(2)-kk(1))*(k2s(2)+k2s(1))

      do j=2,kfinal
      ktemp=ktemp+0.5d0*(kk(j)-kk(j-1))*(k2s(j)+k2s(j-1))
      enddo
             k2m(i)=ktemp/(s2m(i)**2.d0)

      enddo
      
      
        do i=1,kfinal
      npeak(i)=((k2m(i)/3)**(1.5d0)*((dc/s2m(i))**2.d0-1)*
     + exp(-((dc/s2m(i))**2.d0)/2))/((2*pi)**2.d0)

      enddo

            !!  Ypologismos M(k) apo (2.3) Ballesteros-arxiv 1709.05565v2         !!
      do i=1,kfinal

      stemp=10.d18*gamma/0.2d0
      stemp=stemp*(gt/106.75d0)**(-1.d0/6.d0)
      stemp=stemp*(kk(i)/(7.d0*1.d13))**(-2.d0)

      mpbh_pt(i)=stemp!/msun
      enddo

      !! ??????????? ?? ?   !!

      do i=1,kfinal
      vita_pt(i)=npeak(i)*(2*pi)**(1.5d0)*(1.d0/(kk(i)**3.d0))
 !     vita_pt(i)=npeak(i)*mpbh_pt(i)

      enddo


      !! logos apo (2.7) Ballesteros-arxiv 1709.05565v2     !!

      do i=1,kfinal
      stemp=vita_pt(i)/(8.d0*10.d-16)
      stemp=stemp*(gamma/0.2d0)**1.5d0
      stemp=stemp*(gt/106.75d0)**(-1.d0/4.d0)
      stemp=stemp*(mpbh_pt(i)/10.d18)**(-0.5d0)
      om_pt(i)=stemp
      enddo

      ! opbh

      do j=1,kfinal
      s2(j)=2.d0*om_pt(j)/kk(j)
      enddo

      stemp=0.5d0*(kk(2)-kk(1))*(s2(2)+s2(1))

      do j=2,kfinal
      stemp=stemp+0.5d0*(kk(j)-kk(j-1))*(s2(j)+s2(j-1))
       enddo

      opbh_pt=stemp
      
 !!     write(57,*) "Nstar:"
 !!     write(57,*) nnstar
 !!     write(99,*) "Abundance:"
 !!     write(99,*) opbh_pt
      

       ppeak_pt= MAXVAL(prk)
 !     write(57,*) "Pr_k:"
 !     write(57,*) MAXVAL(prk)
      
      s2mm=MAXVAL(om_pt)
      do j=1,kfinal
      if(om_pt(j).ge.s2mm) then
       jj=j
      endif
      enddo
      mpeak_pt=mpbh_pt(jj)/msun

       do j=1,kfinal
       mpbh_pt(j)=mpbh_pt(j)/msun
 !      write(59,*) kk(j),vita(j),s2m(j)
!       write(58,*) mpbh(j)/msun,om(j),kk(j)
       enddo
       return
       end


      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!    Methods of Integrations (1) !!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE rk4(y,dydx,n,x,h)
      INTEGER n,NMAX
      REAL*8 h,x,dydx(n),y(n),yout(n)
      EXTERNAL derivs
      PARAMETER (NMAX=50)
      INTEGER*4 i
      REAL*8 h6,hh,xh,dym(NMAX),dyt(NMAX),yt(NMAX)
      hh=h*0.5d0
      h6=h/6.d0
      xh=x+hh
      do i=1,n
       yt(i)=y(i)+hh*dydx(i)
       enddo
       call derivs(n,xh,yt,dyt)
       do i=1,n
       yt(i)=y(i)+hh*dyt(i)
       enddo
       call derivs(n,xh,yt,dym)
       do i=1,n
       yt(i)=y(i)+h*dym(i)
       dym(i)=dyt(i)+dym(i)
       enddo
       call derivs(n,x+h,yt,dyt)
       do i=1,n
       yout(i)=y(i)+h6*(dydx(i)+dyt(i)+2.d0*dym(i))
       enddo
       do i=1,n
       y(i)=yout(i)
       enddo
       return
       END  



      SUBROUTINE rk4two(y,dydx,n,x,h)
      INTEGER n,NMAX
      REAL*8 h,x,dydx(n),y(n),yout(n)
      EXTERNAL derivstwo
      PARAMETER (NMAX=50)
      INTEGER*4 i
      REAL*8 h6,hh,xh,dym(NMAX),dyt(NMAX),yt(NMAX)
      hh=h*0.5d0
      h6=h/6.d0
      xh=x+hh
      do i=1,n
       yt(i)=y(i)+hh*dydx(i)
       enddo
       call derivstwo(n,xh,yt,dyt)
       do i=1,n
       yt(i)=y(i)+hh*dyt(i)
       enddo
       call derivstwo(n,xh,yt,dym)
       do i=1,n
       yt(i)=y(i)+h*dym(i)
       dym(i)=dyt(i)+dym(i)
       enddo
       call derivstwo(n,x+h,yt,dyt)
       do i=1,n
       yout(i)=y(i)+h6*(dydx(i)+dyt(i)+2.d0*dym(i))
       enddo
       do i=1,n
       y(i)=yout(i)
       enddo
       return
       END  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE odeint(ystart,nvar,x1,x2,eps,h1)
      IMPLICIT NONE
      INTEGER*4 nbad,nok,nvar,KMAXX,MAXSTP,NMAX
      REAL*8 eps,h1,hmin,x1,x2,ystart(nvar),TINY
      EXTERNAL derivstwo,rkqs
      PARAMETER (MAXSTP=100000,NMAX=50,KMAXX=200,TINY=1.d-30)
      INTEGER*4 i,kmax,kount,nstp
      REAL*8 dxsav,h,hdid,hnext,x,xsav,dydx(NMAX),xp(KMAXX),y(NMAX),
     *yp(NMAX,KMAXX),yscal(NMAX)
      COMMON /path/ kmax,kount,dxsav,xp,yp
      hmin=0.d0
      x=x1
      h=sign(h1,x2-x1)
      nok=0
      nbad=0
      kount=0
      do 11 i=1,nvar
        y(i)=ystart(i)
11    continue
      if (kmax.gt.0) xsav=x-2.d0*dxsav
      do 16 nstp=1,MAXSTP
        call derivstwo(nvar,x,y,dydx) !derivs(n,t,q,dqdt,parameters)      
        do 12 i=1,nvar
          yscal(i)=abs(y(i))+abs(h*dydx(i))+TINY
12      continue
        if(kmax.gt.0)then
          if(abs(x-xsav).gt.abs(dxsav)) then
            if(kount.lt.kmax-1)then
              kount=kount+1
              xp(kount)=x
              do 13 i=1,nvar
                yp(i,kount)=y(i)
13            continue
              xsav=x
            endif
          endif
        endif
        if((x+h-x2)*(x+h-x1).gt.0.) h=x2-x
       call rkqs(y,dydx,nvar,x,h,eps,yscal,hdid,hnext)
        if(hdid.eq.h)then
          nok=nok+1
        else
          nbad=nbad+1
        endif
        if((x-x2)*(x2-x1).ge.0.)then
          do 14 i=1,nvar
            ystart(i)=y(i)
14        continue
          if(kmax.ne.0)then
            kount=kount+1
            xp(kount)=x
            do 15 i=1,nvar
              yp(i,kount)=y(i)
15          continue
          endif
          return
        endif
        if(abs(hnext).lt.hmin) pause
     *'stepsize smaller than minimum in odeint'
        h=hnext
16    continue
       pause 'too many steps in odeint'
      return
      END

      SUBROUTINE rkqs(y,dydx,n,x,htry,eps,yscal,hdid,hnext
     * )
      IMPLICIT NONE
      INTEGER*4 n,NMAX
      REAL*8 eps,hdid,hnext,htry,x,dydx(n),y(n),yscal(n)
      EXTERNAL derivstwo
      PARAMETER (NMAX=50)
CU    USES derivs,rkck
      INTEGER*4 i
      REAL*8 errmax,h,htemp,xnew,yerr(NMAX),ytemp(NMAX),SAFETY,PGROW,
     *PSHRNK,ERRCON
      PARAMETER(SAFETY=0.9d0,PGROW=-0.2d0,PSHRNK=-0.25d0,ERRCON=1.89d-4)
      h=htry
1     call rkck(y,dydx,n,x,h,ytemp,yerr)
      errmax=0.d0
      do 11 i=1,n
        errmax=max(errmax,abs(yerr(i)/yscal(i)))
11    continue
      errmax=errmax/eps
      if(errmax.gt.1.)then
        htemp=SAFETY*h*(errmax**PSHRNK)
        h=sign(max(abs(htemp),0.1d0*abs(h)),h)
        xnew=x+h
        if(xnew.eq.x)pause 'stepsize underflow in rkqs'
        goto 1
      else
        if(errmax.gt.ERRCON)then
          hnext=SAFETY*h*(errmax**PGROW)
        else
          hnext=5.d0*h
        endif
        hdid=h
        x=x+h
        do 12 i=1,n
          y(i)=ytemp(i)
12      continue
        return
      endif
      END


      SUBROUTINE rkck(y,dydx,n,x,h,yout,yerr)
      IMPLICIT NONE
      INTEGER*4 n,NMAX
      REAL*8 h,x,dydx(n),y(n),yerr(n),yout(n)
      EXTERNAL derivstwo
      PARAMETER (NMAX=50)
CU    USES derivs
      INTEGER*4 i
      REAL*8 ak2(NMAX),ak3(NMAX),ak4(NMAX),ak5(NMAX),ak6(NMAX),
     *ytemp(NMAX),A2,A3,A4,A5,A6,B21,B31,B32,B41,B42,B43,B51,B52,B53,
     *B54,B61,B62,B63,B64,B65,C1,C3,C4,C6,DC1,DC3,DC4,DC5,DC6
      PARAMETER(A2=0.2d0,A3=0.3d0,A4=0.6d0,A5=1.d0,A6=0.875d0,B21=0.2d0,
     *B31=3.d0/40.d0,B32=9.d0/40.d0,B41=0.3d0,B42=-0.9d0,B43=1.2d0,
     *B51=-11.d0/54.d0,B52=2.5d0,B53=-70.d0/27.d0,B54=35.d0/27.d0,
     *B61=1631.d0/55296.d0,B62=175.d0/512.d0,B63=575.d0/13824.d0,
     *B64=44275.d0/110592.d0,B65=253.d0/4096.d0,C1=37.d0/378.d0,
     *C3=250.d0/621.d0,C4=125.d0/594.d0,C6=512.d0/1771.d0,
     *DC1=C1-2825.d0/27648.d0,DC3=C3-18575.d0/48384.d0,
     *DC4=C4-13525.d0/55296.d0,DC5=-277.d0/14336.d0,
     *DC6=C6-0.25d0)
      do 11 i=1,n
        ytemp(i)=y(i)+B21*h*dydx(i)
11    continue
      call derivstwo(n,x+A2*h,ytemp,ak2)
      do 12 i=1,n
        ytemp(i)=y(i)+h*(B31*dydx(i)+B32*ak2(i))
12    continue
      call derivstwo(n,x+A3*h,ytemp,ak3)
      do 13 i=1,n
        ytemp(i)=y(i)+h*(B41*dydx(i)+B42*ak2(i)+B43*ak3(i))
13    continue
      call derivstwo(n,x+A4*h,ytemp,ak4)!n,x+h,yt,dyt
      do 14 i=1,n
        ytemp(i)=y(i)+h*(B51*dydx(i)+B52*ak2(i)+B53*ak3(i)+B54*ak4(i))
14    continue
      call derivstwo(n,x+A5*h,ytemp,ak5)
      do 15 i=1,n
        ytemp(i)=y(i)+h*(B61*dydx(i)+B62*ak2(i)+B63*ak3(i)+B64*ak4(i)+
     *B65*ak5(i))
15    continue
      call derivstwo(n,x+A6*h,ytemp,ak6)
      do 16 i=1,n
        yout(i)=y(i)+h*(C1*dydx(i)+C3*ak3(i)+C4*ak4(i)+C6*ak6(i))
16    continue
      do 17 i=1,n
        yerr(i)=h*(DC1*dydx(i)+DC3*ak3(i)+DC4*ak4(i)+DC5*ak5(i)+DC6*
     *ak6(i))
17    continue
      return
      END



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE odeintone(ystart,nvar,x1,x2,eps,h1)
      IMPLICIT NONE
      INTEGER*4 nbad,nok,nvar,KMAXX,MAXSTP,NMAX
      REAL*8 eps,h1,hmin,x1,x2,ystart(nvar),TINY
      EXTERNAL derivs,rkqsone
      PARAMETER (MAXSTP=100000,NMAX=50,KMAXX=200,TINY=1.d-30)
      INTEGER*4 i,kmax,kount,nstp
      REAL*8 dxsav,h,hdid,hnext,x,xsav,dydx(NMAX),xp(KMAXX),y(NMAX),
     *yp(NMAX,KMAXX),yscal(NMAX)
      COMMON /path/ kmax,kount,dxsav,xp,yp
      hmin=0.d0
      x=x1
      h=sign(h1,x2-x1)
      nok=0
      nbad=0
      kount=0
      do 11 i=1,nvar
        y(i)=ystart(i)
11    continue
      if (kmax.gt.0) xsav=x-2.d0*dxsav
      do 16 nstp=1,MAXSTP
        call derivs(nvar,x,y,dydx) !derivs(n,t,q,dqdt,parameters)      
        do 12 i=1,nvar
          yscal(i)=abs(y(i))+abs(h*dydx(i))+TINY
12      continue
        if(kmax.gt.0)then
          if(abs(x-xsav).gt.abs(dxsav)) then
            if(kount.lt.kmax-1)then
              kount=kount+1
              xp(kount)=x
              do 13 i=1,nvar
                yp(i,kount)=y(i)
13            continue
              xsav=x
            endif
          endif
        endif
        if((x+h-x2)*(x+h-x1).gt.0.) h=x2-x
       call rkqsone(y,dydx,nvar,x,h,eps,yscal,hdid,hnext)
        if(hdid.eq.h)then
          nok=nok+1
        else
          nbad=nbad+1
        endif
        if((x-x2)*(x2-x1).ge.0.)then
          do 14 i=1,nvar
            ystart(i)=y(i)
14        continue
          if(kmax.ne.0)then
            kount=kount+1
            xp(kount)=x
            do 15 i=1,nvar
              yp(i,kount)=y(i)
15          continue
          endif
          return
        endif
        if(abs(hnext).lt.hmin) pause
     *'stepsize smaller than minimum in odeintone'
        h=hnext
16    continue
       pause 'too many steps in odeintone'
      return
      END

      SUBROUTINE rkqsone(y,dydx,n,x,htry,eps,yscal,hdid,hnext
     * )
      IMPLICIT NONE
      INTEGER*4 n,NMAX
      REAL*8 eps,hdid,hnext,htry,x,dydx(n),y(n),yscal(n)
      EXTERNAL derivs
      PARAMETER (NMAX=50)
CU    USES derivs,rkck
      INTEGER*4 i
      REAL*8 errmax,h,htemp,xnew,yerr(NMAX),ytemp(NMAX),SAFETY,PGROW,
     *PSHRNK,ERRCON
      PARAMETER(SAFETY=0.9d0,PGROW=-0.2d0,PSHRNK=-0.25d0,ERRCON=1.89d-4)
      h=htry
1     call rkckone(y,dydx,n,x,h,ytemp,yerr)
      errmax=0.d0
      do 11 i=1,n
        errmax=max(errmax,abs(yerr(i)/yscal(i)))
11    continue
      errmax=errmax/eps
      if(errmax.gt.1.)then
        htemp=SAFETY*h*(errmax**PSHRNK)
        h=sign(max(abs(htemp),0.1d0*abs(h)),h)
        xnew=x+h
        if(xnew.eq.x)pause 'stepsize underflow in rkqsone'
        goto 1
      else
        if(errmax.gt.ERRCON)then
          hnext=SAFETY*h*(errmax**PGROW)
        else
          hnext=5.d0*h
        endif
        hdid=h
        x=x+h
        do 12 i=1,n
          y(i)=ytemp(i)
12      continue
        return
      endif
      END


      SUBROUTINE rkckone(y,dydx,n,x,h,yout,yerr)
      IMPLICIT NONE
      INTEGER*4 n,NMAX
      REAL*8 h,x,dydx(n),y(n),yerr(n),yout(n)
      EXTERNAL derivs
      PARAMETER (NMAX=50)
CU    USES derivs
      INTEGER*4 i
      REAL*8 ak2(NMAX),ak3(NMAX),ak4(NMAX),ak5(NMAX),ak6(NMAX),
     *ytemp(NMAX),A2,A3,A4,A5,A6,B21,B31,B32,B41,B42,B43,B51,B52,B53,
     *B54,B61,B62,B63,B64,B65,C1,C3,C4,C6,DC1,DC3,DC4,DC5,DC6
      PARAMETER(A2=0.2d0,A3=0.3d0,A4=0.6d0,A5=1.d0,A6=0.875d0,B21=0.2d0,
     *B31=3.d0/40.d0,B32=9.d0/40.d0,B41=0.3d0,B42=-0.9d0,B43=1.2d0,
     *B51=-11.d0/54.d0,B52=2.5d0,B53=-70.d0/27.d0,B54=35.d0/27.d0,
     *B61=1631.d0/55296.d0,B62=175.d0/512.d0,B63=575.d0/13824.d0,
     *B64=44275.d0/110592.d0,B65=253.d0/4096.d0,C1=37.d0/378.d0,
     *C3=250.d0/621.d0,C4=125.d0/594.d0,C6=512.d0/1771.d0,
     *DC1=C1-2825.d0/27648.d0,DC3=C3-18575.d0/48384.d0,
     *DC4=C4-13525.d0/55296.d0,DC5=-277.d0/14336.d0,
     *DC6=C6-0.25d0)
      do 11 i=1,n
        ytemp(i)=y(i)+B21*h*dydx(i)
11    continue
      call derivs(n,x+A2*h,ytemp,ak2)
      do 12 i=1,n
        ytemp(i)=y(i)+h*(B31*dydx(i)+B32*ak2(i))
12    continue
      call derivs(n,x+A3*h,ytemp,ak3)
      do 13 i=1,n
        ytemp(i)=y(i)+h*(B41*dydx(i)+B42*ak2(i)+B43*ak3(i))
13    continue
      call derivs(n,x+A4*h,ytemp,ak4)!n,x+h,yt,dyt
      do 14 i=1,n
        ytemp(i)=y(i)+h*(B51*dydx(i)+B52*ak2(i)+B53*ak3(i)+B54*ak4(i))
14    continue
      call derivs(n,x+A5*h,ytemp,ak5)
      do 15 i=1,n
        ytemp(i)=y(i)+h*(B61*dydx(i)+B62*ak2(i)+B63*ak3(i)+B64*ak4(i)+
     *B65*ak5(i))
15    continue
      call derivs(n,x+A6*h,ytemp,ak6)
      do 16 i=1,n
        yout(i)=y(i)+h*(C1*dydx(i)+C3*ak3(i)+C4*ak4(i)+C6*ak6(i))
16    continue
      do 17 i=1,n
        yerr(i)=h*(DC1*dydx(i)+DC3*ak3(i)+DC4*ak4(i)+DC5*ak5(i)+DC6*
     *ak6(i))
17    continue
      return
      END


      SUBROUTINE jacobi(a,n,np,d,v,nrot)
      INTEGER n,np,nrot,NMAX
      REAL*8 a(np,np),d(np),v(np,np)
      PARAMETER (NMAX=500)
      INTEGER i,ip,iq,j
      REAL*8 c,g,h,s,sm,t,tau,theta,tresh,b(NMAX),z(NMAX)
      do 12 ip=1,n
        do 11 iq=1,n
          v(ip,iq)=0.d0
11      continue
        v(ip,ip)=1.d0
12    continue
      do 13 ip=1,n
        b(ip)=a(ip,ip)
        d(ip)=b(ip)
        z(ip)=0.d0
13    continue
      nrot=0
      do 24 i=1,50
        sm=0.d0
        do 15 ip=1,n-1
          do 14 iq=ip+1,n
            sm=sm+abs(a(ip,iq))
14        continue
15      continue
        if(sm.eq.0.d0)return
        if(i.lt.4)then
          tresh=0.2d0*sm/n**2.d0
        else
          tresh=0.d0
        endif
        do 22 ip=1,n-1
          do 21 iq=ip+1,n
            g=100.d0*abs(a(ip,iq))
            if((i.gt.4).and.(abs(d(ip))+
     *g.eq.abs(d(ip))).and.(abs(d(iq))+g.eq.abs(d(iq))))then
              a(ip,iq)=0.d0
            else if(abs(a(ip,iq)).gt.tresh)then
              h=d(iq)-d(ip)
              if(abs(h)+g.eq.abs(h))then
                t=a(ip,iq)/h
              else
                theta=0.5d0*h/a(ip,iq)
                t=1.d0/(abs(theta)+sqrt(1.d0+theta**2.d0))
                if(theta.lt.0.d0)t=-t
              endif
              c=1.d0/sqrt(1.d0+t**2.d0)
              s=t*c
              tau=s/(1.d0+c)
              h=t*a(ip,iq)
              z(ip)=z(ip)-h
              z(iq)=z(iq)+h
              d(ip)=d(ip)-h
              d(iq)=d(iq)+h
              a(ip,iq)=0.d0
              do 16 j=1,ip-1
                g=a(j,ip)
                h=a(j,iq)
                a(j,ip)=g-s*(h+g*tau)
                a(j,iq)=h+s*(g-h*tau)
16            continue
              do 17 j=ip+1,iq-1
                g=a(ip,j)
                h=a(j,iq)
                a(ip,j)=g-s*(h+g*tau)
                a(j,iq)=h+s*(g-h*tau)
17            continue
              do 18 j=iq+1,n
                g=a(ip,j)
                h=a(iq,j)
                a(ip,j)=g-s*(h+g*tau)
                a(iq,j)=h+s*(g-h*tau)
18            continue
              do 19 j=1,n
                g=v(j,ip)
                h=v(j,iq)
                v(j,ip)=g-s*(h+g*tau)
                v(j,iq)=h+s*(g-h*tau)
19            continue
              nrot=nrot+1
            endif
21        continue
22      continue
        do 23 ip=1,n
          b(ip)=b(ip)+z(ip)
          d(ip)=b(ip)
          z(ip)=0.d0
23      continue
24    continue
      pause 'too many iterations in jacobi'
      return
      END

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!   GAMMA FUNCTION        !!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      
      FUNCTION igf(alpha, x)
      implicit none
      COMPLEX*16 alpha,x,fn_val,cdig,p,q,cone,re,cdh,igf
      INTEGER*4 i, ilim,ibuf
      REAL*8 zero,xlim,dnrm
      zero = 0.0d0
      xlim = 1.0d0
      cone = dcmplx(1.0d0, 0.0d0)
      re = dcmplx(0.36787944117144232d0, 0.0d0)
      ibuf = 34

! --- If x is near the negative real axis, then shift to x=1.
      IF (dnrm(x).lt.xlim.OR.dREAL(x).lt.zero.AND.
     + ABS(dIMAG(x)).lt.xlim) THEN
      fn_val = re / cdh(alpha, cone)
      ilim = INT(x/re)
      DO  i = 0, ibuf - ilim
      CALL term(alpha, x, i, p, q)
      fn_val = fn_val + p * q
      ENDDO
      ELSE
      fn_val = EXP(-x + alpha*LOG(x)) / cdh(alpha, x)
      ENDIF
      igf=fn_val
      RETURN
      END



      FUNCTION cdh(alpha, x)
! --- Written By Eric Kostlan & Dmitry Gokhman
! --- March  1986
      implicit none
      COMPLEX*16 alpha,x,fn_val,cdh,term, sum, cn, alpha1,cdhs
      INTEGER*4 i,n
      REAL*8 one
      one=1.d0

! --- If Re(alpha-x) is too big, shift alpha.
      n = INT(alpha-x)
      IF (n.gt.0) THEN
      cn = n
      alpha1 = alpha - cn
      term = one / x
      sum = term
      DO  i = 1, n - 1
      cn = n - i
      term = term * (alpha1 + cn) / x
      sum = term + sum
      ENDDO
      sum = sum + term * alpha1 / cdhs(alpha1, x)
      fn_val = one / sum
      ELSE
      fn_val = cdhs(alpha, x)
      END IF
      cdh=fn_val
      RETURN
      END



      FUNCTION cdhs(alpha, x)
! --- Written By Eric Kostlan & Dmitry Gokhman
! --- March  1986
      implicit none
      COMPLEX*16 alpha,x,fn_val,p0, q0, p1, q1, r0, r1, ci, factor,cdhs
      INTEGER*4 i,ilim
      REAL*8 zero,half,one,tol1,tol2,error,dnrm
      ilim = 100000
      zero = 0.0d0
      half = 0.5d0
      one = 1.0d0
      tol1 = 1.0D+10
      tol2 = 1.0D-10
      error = 5.D-18

      q0 = one
      q1 = one
      p0 = x
      p1 = x + one - alpha
      DO  i = 1, ilim
      ci = i
      IF (p0.ne.zero.AND.q0.ne.zero.AND.q1.ne.zero) THEN
      r0 = p0 / q0
      r1 = p1 / q1
      IF (dnrm(r0-r1).le.dnrm(r1)*error) THEN
      fn_val = r1
      cdhs=fn_val
      RETURN
      ENDIF
! --------- Occasionally renormalize the sequences to avoid over(under)flow.
      IF (dnrm(p0).gt.tol1.OR.dnrm(p0).lt.tol2.OR.dnrm(q0).gt.tol1
     +    .OR.dnrm(q0).lt.tol2) THEN
      factor = p0 * q0
      p0 = p0 / factor
      q0 = q0 / factor
      p1 = p1 / factor
      q1 = q1 / factor
      END IF
      ENDIF
      p0 = x * p1 + ci * p0
      q0 = x * q1 + ci * q0
      p1 = p0 + (ci+one-alpha) * p1
      q1 = q0 + (ci+one-alpha) * q1
      ENDDO
! --- If the peripheral routines are written correctly,
! --- the following four statements should never be executed.
      WRITE(*, *) 'cdhs:  *** Warning: i >', ilim
      WRITE(*, *) 'cdhs:  *** r0,r1= ', r0, r1
      fn_val = half * (r0+r1)
      cdhs=fn_val
      RETURN
      END



      SUBROUTINE term(alpha, x, i, p, q)
! --- Calculate p*q = -1**i(1 - x**(alpha+i))/(alpha+i)i ! carefully.
      implicit none
      COMPLEX*16 alpha,x,p,q,ci, cdlx, alphai
      INTEGER*4 i
      REAL*8 zero,one,two,tol,xlim,dnrm
      zero = 0.0d0
      one = 1.0d0
      two = 2.0d0
      tol = 3.0D-7
      xlim = 39.0d0

      IF (i.eq.0) q = one
      ci = i
      alphai = alpha + ci
      IF (x.eq.zero) THEN
      p = one / alphai
      IF (i.ne.0) q = -q / ci
      RETURN
      ENDIF
      cdlx = LOG(x)

! --- If (1 - x**alphai) = -x**alphai on the computer,
! --- then change the inductive scheme to avoid overflow.
      IF (dREAL(alphai*cdlx).gt.xlim.AND.i.ne.0) THEN
      p = p * (alphai - one) / alphai
      q = -q * x / ci
      RETURN
      ENDIF
      IF (dnrm(alphai).gt.tol) THEN
      p = (one - x**alphai) / alphai
      ELSE
      p = -cdlx * (one + cdlx*alphai/two)
      ENDIF
      IF (i.eq.0) q = -q / ci
      RETURN
      END



      FUNCTION dnrm(z)
      implicit none
      REAL*8 dnrm
      COMPLEX*16 z
      dnrm = ABS(dREAL(z)) + ABS(dIMAG(z))*1.d0
      RETURN
      END

      SUBROUTINE sort2(n,arr,brr)
      INTEGER*4 n,M,NSTACK
      REAL*8 arr(n),brr(n)
      PARAMETER (M=7,NSTACK=1000)
      INTEGER*4 i,ir,j,jstack,k,l,istack(NSTACK)
      REAL*8 a,b,temp
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.M)then
        do j=l+1,ir
          a=arr(j)
          b=brr(j)
          do i=j-1,l,-1
            if(arr(i).le.a)goto 2
            arr(i+1)=arr(i)
            brr(i+1)=brr(i)
         enddo
          i=l-1
2         arr(i+1)=a
          brr(i+1)=b
         enddo
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        temp=arr(k)
        arr(k)=arr(l+1)
        arr(l+1)=temp
        temp=brr(k)
        brr(k)=brr(l+1)
        brr(l+1)=temp
        if(arr(l).gt.arr(ir))then
          temp=arr(l)
          arr(l)=arr(ir)
          arr(ir)=temp
          temp=brr(l)
          brr(l)=brr(ir)
          brr(ir)=temp
        endif
        if(arr(l+1).gt.arr(ir))then
          temp=arr(l+1)
          arr(l+1)=arr(ir)
          arr(ir)=temp
          temp=brr(l+1)
          brr(l+1)=brr(ir)
          brr(ir)=temp
        endif
        if(arr(l).gt.arr(l+1))then
          temp=arr(l)
          arr(l)=arr(l+1)
          arr(l+1)=temp
          temp=brr(l)
          brr(l)=brr(l+1)
          brr(l+1)=temp
        endif
        i=l+1
        j=ir
        a=arr(l+1)
        b=brr(l+1)
3       continue
          i=i+1
        if(arr(i).lt.a)goto 3
4       continue
          j=j-1
        if(arr(j).gt.a)goto 4
        if(j.lt.i)goto 5
        temp=arr(i)
        arr(i)=arr(j)
        arr(j)=temp
        temp=brr(i)
        brr(i)=brr(j)
        brr(j)=temp
        goto 3
5       arr(l+1)=arr(j)
        arr(j)=a
        brr(l+1)=brr(j)
        brr(j)=b
        jstack=jstack+2
        if(jstack.gt.NSTACK)pause 'NSTACK too small in sort2'
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1
      END

      FUNCTION Gammaa(x)
      IMPLICIT NONE
      REAL*8 x,gammq,gammln,Gammaa
      Gammaa=dexp(gammln(0.5d0))*gammq(0.5d0,x)
      RETURN
      END

      FUNCTION gammq(a,x)
      REAL*8 a,gammq,x
CU    USES gcf,gser
      REAL*8 gammcf,gamser,gln
      if(x.lt.0.d0.or.a.le.0.d0)pause 'bad arguments in gammq'
      if(x.lt.a+1.d0)then
        call gser(gamser,a,x,gln)
        gammq=1.d0-gamser
      else
        call gcf(gammcf,a,x,gln)
        gammq=gammcf
      endif
      return
      END


      SUBROUTINE gcf(gammcf,a,x,gln)
      INTEGER*4 ITMAX
      REAL*8 a,gammcf,gln,x,EPS,FPMIN
      PARAMETER (ITMAX=100,EPS=3.d-7,FPMIN=1.d-30)
CU    USES gammln
      INTEGER*4 i
      REAL*8 an,b,c,d,del,h,gammln
      gln=gammln(a)
      b=x+1.d0-a
      c=1.d0/FPMIN
      d=1.d0/b
      h=d
      do 11 i=1,ITMAX
        an=-i*(i-a)
        b=b+2.d0
        d=an*d+b
        if(abs(d).lt.FPMIN)d=FPMIN
        c=b+an/c
        if(abs(c).lt.FPMIN)c=FPMIN
        d=1.d0/d
        del=d*c
        h=h*del
        if(abs(del-1.d0).lt.EPS)goto 1
11    continue
      pause 'a too large, ITMAX too small in gcf'
1     gammcf=exp(-x+a*log(x)-gln)*h
      return
      END


      SUBROUTINE gser(gamser,a,x,gln)
      INTEGER*4 ITMAX
      REAL*8 a,gamser,gln,x,EPS
      PARAMETER (ITMAX=100,EPS=3.d-7)
CU    USES gammln
      INTEGER*4 n
      REAL*8 ap,del,sum,gammln
      gln=gammln(a)
      if(x.le.0.d0)then
        if(x.lt.0.d0)pause 'x < 0 in gser'
        gamser=0.d0
        return
      endif
      ap=a
      sum=1.d0/a
      del=sum
      do 11 n=1,ITMAX
        ap=ap+1.d0
        del=del*x/ap
        sum=sum+del
        if(abs(del).lt.abs(sum)*EPS)goto 1
11    continue
      pause 'a too large, ITMAX too small in gser'
1     gamser=sum*exp(-x+a*log(x)-gln)
      return
      END


      FUNCTION gammln(xx)
      REAL*8 gammln,xx
      INTEGER*4 j
      DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,
     *24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,
     *-.5395239384953d-5,2.5066282746310005d0/
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*log(tmp)-tmp
      ser=1.000000000190015d0
      do 11 j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
11    continue
      gammln=tmp+log(stp*ser/x)
      return
      END
      

      
      SUBROUTINE GW(kmode_mon,result_mon)
      integer i,n,j,io,iostat,ndata,k,n_gauleg,n_montecarlo
      integer flag_method,idum
      parameter(n=10000,n_gauleg=1000,n_montecarlo=100000)
      REAL*8 efold(n),pr(n),kmode(n)
      real*8 yp1,ypn,y2(n), kstar,y_res,x_res
      real*8 xmin,xmax,ymin,ymax,ymin_new,ymax_new
      real*8 sg(n_gauleg),w_s(n_gauleg),dg(n_gauleg),w_d(n_gauleg),pi
      real*8 fun,s,d,xx,yy,xx1,yy1,pm,ps,inte3,kmode_mon,result_mon,h_2
      real*8 random_x(n_montecarlo),random_y(n_montecarlo),ran2
      real*8 x_montecarlo(n_montecarlo),y_montecarlo(n_montecarlo)
      parameter(flag_method=2)
      !flag_method=1 Integration with Gaussian weight
      !flag_methos=2 monte-carlo
      open(unit=11, file="n_prz_kmode.txt",status="old")
!      open(unit=12, file="testpr.txt",status="unknown")
      !open(unit=13, file="sd.txt",status="unknown")
      !open(unit=99,file="gw1.txt",status="unknown")
       open(unit=89,file="gw2.txt",status="unknown")
             !!idum=-123456789

       pi=dacos(-1.d0)
       j=1
      do
      read(11,*,iostat=io) efold(j),pr(j),kmode(j)
      !y=Log10(y)
      !x=Log10(x)
      if (io/=0) exit ! See where the lines in the txt file end
      j=j+1
      enddo
      ndata=j-1
      close(11)
      
      yp1=(pr(2)-pr(1))/(kmode(2)-kmode(1))
      ypn=(pr(ndata-1)-pr(ndata-2))/(kmode(ndata-1)-kmode(ndata-2))
    
    
      call spline(kmode,pr,ndata,yp1,ypn,y2)
      
      ! do k=1,1000!
       !kstar=((k+60.d0)/60.d0)!
       ! !print*,kstar
        !kstar=10.d0**kstar
!         DO K=1,10000
!       kstar=((k+2000.d0)/800.d0)
!       kstar=10.d0**kstar
!       call splint(kmode,pr,y2,ndata,kstar,y_res)
!      !print*, kstar,y_res
!        write(12,*) kstar,(y_res)
!        enddo!
      
      xmin=0.d0
      xmax= 1.d0/sqrt(3.d0)
      call gauleg(xmin,xmax,dg,w_d,n_gauleg)
      
      
      ymin=1.d0/sqrt(3.d0)
      ymax=1000000.d0
      ymin_new=0.d0
      ymax_new=exp(-ymin)
      call gauleg(ymin_new,ymax_new,sg,w_s,n_gauleg)
      
      !do i=1,n_gauleg
      !write(13,*) sg(i), dg(i)
      !enddo
      
       ! DO K=1,1000
        !kstar=((k+60.d0)/60.d0)
            DO K=1,4000
        kstar=((k+8000.d0)/800.d0)
        ! DO K=1,4000
       !kstar=((k+240.d0)/240.d0)
      kstar=10.d0**kstar
      
        inte3=0.d0
      do i=1, n_gauleg
         do j=1,n_gauleg
         
      xx=-log(sg(j))
      yy=dg(i)
      xx1=kstar*(xx+yy)*dsqrt(3.d0)/2.d0
      yy1=kstar*(xx-yy)*dsqrt(3.d0)/2.d0
      call splint(kmode,pr,y2,ndata,xx1,x_res)
      call splint(kmode,pr,y2,ndata,yy1,y_res)

         !print*, y_res
         pm=(x_res)
         ps=(y_res)
c      enddo
c      enddo
      
     
c      do i=1,n_gauleg
c      do j=1,n_gauleg
      
      inte3=inte3+w_s(j)*w_d(i)*pm*ps
     +*fun(dg(i),(-log(sg(j))))/sg(j)
     
      enddo
      enddo
      !write(99,*) kstar,inte3!/kstar**3
            h_2=0.67d0**2.d0
      
      kmode_mon=((0.9715d0*1.d-14)/(2*pi))*kstar
      result_mon=inte3*h_2
      write(89,*)  kmode_mon,result_mon
      print*,k
      ENDDO
      
      
      !!!!!!!!!!!!!!!!
      ! Monte-Carlo:!!
      !!!!!!!!!!!!!!!!
      pm=0
      ps=0
      inte3=0.d0
      if (flag_method.eq.2) then
      open(unit=88,file="gw1.txt",status="unknown")
       !DO K=1,1000
        !kstar=((k+60.d0)/60.d0)
                DO K=1,4000
        kstar=((k+8000.d0)/800.d0)
      
      kstar=10.d0**kstar
      inte3=0.d0
      do i=1,n_montecarlo
          random_x(i)=ran2(idum)
          random_y(i)=ran2(idum)  
       !print*, random_x(i)
      enddo
       
      xmin=0
      xmax= 1.0/sqrt(3.0)
      
       ymin=1.0/sqrt(3.0)
      ymax=1000000.0
      ymin_new=0.0
      ymax_new=exp(-ymin)
      
      do i=1,n_montecarlo
        x_montecarlo(i)=xmin+(xmax-xmin)*random_x(i) 
        ! print*, x_montecarlo(i)
       ! enddo
 
        ! do i=1,n_montecarlo
        y_montecarlo(i)=ymin-log(random_y(i)) !ymin+(ymax-ymin)*random_y(i) !
        !print*, y_montecarlo(i)
         enddo
         
      do i=1,n_montecarlo
      xx=y_montecarlo(i)!-log(sg(j))
      yy=x_montecarlo(i)!dg(i)
      xx1=kstar*(xx+yy)*dsqrt(3.d0)/2.d0
      yy1=kstar*(xx-yy)*dsqrt(3.d0)/2.d0
      call splint(kmode,pr,y2,ndata,xx1,x_res)
      call splint(kmode,pr,y2,ndata,yy1,y_res)
       
         !print*, y_res
         pm=abs(x_res)
         ps=abs(y_res)
         
         
      !inte3=0.0
      !do j=1,n_montecarlo 
          inte3=inte3+fun(x_montecarlo(i),y_montecarlo(i))*pm*ps
     +/(exp(-ymin)*random_y(i))
          !inte3=inte3+fun(x(i),y(i))/(exp(-ymin)*random_y(i))
          !enddo
          enddo
            inte3=(xmax-xmin)*exp(-ymin)*inte3
      inte3=inte3!/n_montecarlo          
      print*, K
      kmode_mon=((0.9715d0*1.d-14)/(2*pi))*kstar
      result_mon=inte3/(2.47d0*1.d5)
      write(88,*)  kmode_mon,result_mon!/100
      ENDDO
      endif
      
      END 
      
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
      
      
      SUBROUTINE spline(x,y,n,yp1,ypn,y2)
      INTEGER n,NMAX
       REAL*8 yp1,ypn,x(n),y(n),y2(n)
      PARAMETER (NMAX=10000)
      INTEGER i,k
       REAL*8 p,qn,sig,un,u(NMAX)
      if (yp1.gt..99e30) then
        y2(1)=0.
        u(1)=0.
      else
        y2(1)=-0.5
        u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      endif
      do 11 i=2,n-1
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((y(i+1)-y(i))/(x(i+
     *1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*
     *u(i-1))/p
11    continue
      if (ypn.gt..99e30) then
        qn=0.
        un=0.
      else
        qn=0.5
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      endif
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do 12 k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
12    continue
      return
      END
      
      
      
      SUBROUTINE splint(xa,ya,y2a,n,x,y)
      INTEGER n
       REAL*8 x,y,xa(n),y2a(n),ya(n)
      INTEGER k,khi,klo
       REAL*8 a,b,h
      klo=1
      khi=n
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k).gt.x)then
          khi=k
        else
          klo=k
        endif
      goto 1
      endif
      h=xa(khi)-xa(klo)
      if (h.eq.0.) pause 'bad xa input in splint'
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**
     *2)/6.
      return
      END


      SUBROUTINE gauleg(x1,x2,x,w,n)
      INTEGER n
      REAL*8 x1,x2,x(n),w(n)
      DOUBLE PRECISION EPS
      PARAMETER (EPS=3.d-14)
      INTEGER i,j,m
      DOUBLE PRECISION p1,p2,p3,pp,xl,xm,z,z1
      m=(n+1)/2
      xm=0.5d0*(x2+x1)
      xl=0.5d0*(x2-x1)
      do 12 i=1,m
        z=cos(3.141592654d0*(i-.25d0)/(n+.5d0))
1       continue
          p1=1.d0
          p2=0.d0
          do 11 j=1,n
            p3=p2
            p2=p1
            p1=((2.d0*j-1.d0)*z*p2-(j-1.d0)*p3)/j
11        continue
          pp=n*(z*p1-p2)/(z*z-1.d0)
          z1=z
          z=z1-p1/pp
        if(abs(z-z1).gt.EPS)goto 1
        x(i)=xm-xl*z
        x(n+1-i)=xm+xl*z
        w(i)=2.d0*xl/((1.d0-z*z)*pp*pp)
        w(n+1-i)=w(i)
12    continue
      return
      END
      
      
      
      real*8 function fun(d,s)
      implicit none
      integer j,i
      real*8 kstar,pr_k,ps,pm!(kfinal)!,kstar
      real*8 k1,k2,kstar1,kstar2,theta,pi
      real*8 s,d,integr,IC,IS,fin,omr
      real*8 x_kh,x_kplus,x_kminus,kmode
      pi = dacos(-1.d0)
      omr=8.6d-5
       !open(unit=13,file="pr_plus_minus.txt",status="unknown")

      IC= -36.d0*pi*theta(s-1)*(s**2.d0+d**2.d0-2.d0)**2.d0/
     +(s**2.d0-d**2.d0)**3.d0
      IS=-36*((s**2.d0+d**2.d0-2.d0)/(s**2.d0-d**2.d0)**2.d0)*
     + (((s**2.d0+d**2.d0-2)/(s**2.d0-d**2))*
     + dlog(abs((d**2-1) /(s**2-1)))+2.d0)
      
      fun=(omr/36.d0)*(s**2.d0-(1.d0/3.d0))*(d**2.d0-(1.d0/3.d0))**2.d0
     +/((s**2.d0-d**2.d0)**2.d0)*(IC**2.d0+IS**2.d0)
      !if(kmode.eq.1) print*, pm,ps 
      END


      function theta(x)     
      implicit none
      real*8 x,theta,eps
        
       eps=0.d0
       if (x.ge.eps) then
        theta=1.d0
       else
        theta=0.d0
      endif
       
c       theta=HeavisideTheta(x)
       
       return
      end

      
      FUNCTION ran2(idum)
      INTEGER idum,IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
      REAL*8 ran2,AM,EPS,RNMX
      PARAMETER (IM1=2147483563,IM2=2147483399,AM=1./IM1,IMM1=IM1-1,
     *IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791,
     *NTAB=32,NDIV=1+IMM1/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      INTEGER idum2,j,k,iv(NTAB),iy
      SAVE iv,iy,idum2
      DATA idum2/123456789/, iv/NTAB*0/, iy/0/
      if (idum.le.0) then
        idum=max(-idum,1)
        idum2=idum
        do 11 j=NTAB+8,1,-1
          k=idum/IQ1
          idum=IA1*(idum-k*IQ1)-k*IR1
          if (idum.lt.0) idum=idum+IM1
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      if (idum.lt.0) idum=idum+IM1
      k=idum2/IQ2
      idum2=IA2*(idum2-k*IQ2)-k*IR2
      if (idum2.lt.0) idum2=idum2+IM2
      j=1+iy/NDIV
      iy=iv(j)-idum2
      iv(j)=idum
      if(iy.lt.1)iy=iy+IMM1
      ran2=min(AM*iy,RNMX)
      return
      END 
      
