! -----------------------------------------------------------------------------
!                             PROGRAM DESCRIPTION
! -----------------------------------------------------------------------------
!   
! Purpose:
!     - Main function solving "The Distributional Implications of Fiscal
!   Consolidation in Developing Countries"
!  
! Author:
!     Xuan S. Tam @ City Univeristy of Hong Kong
!     Commented by: Xin Tang @ IMF, Spring 2017
!  
! Record of Revisions:
!         Date:                 Description of Changes
!     ===========        =================================
!      12/26/2016:                 Legacy Code
!
! Compiler Used:
!   Intel(R) Visual Fortran Compiler XE 14.0.0.103 [IA-64]
!   Commercial Release: Intel Parallel Studio XE 2013
!   Integrated with Microsoft Visual Studio 2012
!
! Library Used:
!   Parallel: Intel(R) MPI Library 2017 Update 1 for Windows
!   Scientific: HSL Archive Library
!               Scilab Fortran Implementation
! =============================================================================

    program multisectors

    ! Load in Modules
    use params              ! Exogeneous Parameters
    use calibparams         ! Endogeneous Parameters
    use states              ! Environmental Variables
    use arrays              ! Auxilliary Variables
    use prices              ! General Equilibrium Prices
    use aggstats            ! Aggregate States of the Economy
    use zbrent_opt          ! 1D Root Finding using Brent's Method
    use mpiparams           ! Environmetal Variables for MPI
    implicit none
    
    integer, parameter :: n = 11    ! Number of Endogeneous Parameters
    integer, parameter :: m = n     ! Number of Moments
	real(dp), dimension(m) :: f     ! Distance between Model and Data
	real(dp), dimension(n) :: x     ! Parameters to be calibrated
	real(dp) :: h,dmax,acc          ! Aux.variables for invoking VA05AD
	integer  :: maxfun, iprint      ! Aux.variables for invoking VA05AD
	real(dp), dimension(3000) :: ww ! Aux.variable for invoking VA05AD
	real(dp) :: xkinc,btime,etime
	logical  :: calib
	integer  :: i,j
  
    include 'mpif.h'

! =============================================================================
! Initializes MPI Environment    
    call MPI_INIT(mpiierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD,myrank,mpiierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,mpiierr)
    
    call cpu_time(btime)

! =============================================================================
! Generate Geometric Grid Points for Manufacturing Capital
    xkpts(1) = 0.00015_dp
    xkinc = zero
    do j = 1,60
       xkinc = xkinc+0.0065_dp*dble(real(j))
       do i = (j-1)*8+2,j*8+1
          xkpts(i) = xkpts(i-1)+xkinc
       enddo
    enddo

! Generate Linear Grid Points for Farm Capital
    xk2pts(1) = xkpts(1)
    xkinc = (xkpts(nkpts)-xkpts(1))/dble(real(nk2pts-1))
    do i = 2,nk2pts
       xk2pts(i) = xk2pts(i-1)+xkinc
    enddo
    
! =============================================================================
! Distribute Workload to Different Cores
! Distribution is based on Idiosyncratic Productivity Shock 
!   discretized using Tauchen
    call mpirange(nzpts,nprocs,myrank,jsta,jend)
    ipack = nkpts*(jend-jsta+1)
    iget  = nkpts*nzpts

! =============================================================================
! Calibration
! Benchmark
    !eprice(1) =  20.5133438772_dp    ! Service Price
    !eprice(2) =  18.8391036004_dp    ! Agricultural Price
    !eprice(3) =  6.0878980669_dp    ! Manufacturing Wage
    !eprice(4) =  3.8278637784_dp    ! Farm Wage
    !eprice(5) =  0.0074103014_dp    ! Interest Rates    

! New SS
    eprice(1) =  20.2107926451_dp    ! Service Price
    eprice(2) =  17.8504562279_dp    ! Agricultural Price
    eprice(3) =  6.1392647747_dp    ! Manufacturing Wage
    eprice(4) =  3.7113439115_dp    ! Farm Wage
    eprice(5) =  0.0075659625_dp    ! Interest Rates    
    
!    
!     Original Ethiopia Value
    abar   = 0.0008314562_dp    ! Subsistence Level
    psi    = 0.4944552241_dp    ! Service Preference
    gamma  = 0.8167964379_dp    ! Manufacturing Preference
    sigmar = 0.2338952483_dp    ! Rural Shock Variance
    sigmau = 0.6250133420_dp    ! Urban Shock Variance    
    ! taua   = 0.0645361532_dp    ! Agri. Tax
    taua   = 0.1045361532_dp    ! Agri. Tax
    taur   = 0.1155349787_dp    ! Corporate Tax = Trade Tax
    ! taur   = 0.2400349787_dp    ! Corporate Tax = Trade Tax
    tauw   = 0.0555203613_dp    ! Labor Income Tax
    ! tauw   = 0.1515203613_dp    ! Labor Income Tax
    za     = 0.7434502896_dp    ! Agricultural Productivity
    zm     = 10.1858001797_dp    ! Manufacturing Productivity
    z      = 0.6845109209_dp    ! Export Sector Productivity    
    
    solveprices = .false.            ! Whether the general equilibrium needs to be
                                    !   sovled using Mkt clearing conditions.
    calib  = .false.                 ! Whether the model parameters needs to be 
                                    !   calibrated with new targets.         

! Vector x is submitted to function calfun
! Order of elements matters
! In the case of calibration, x serves also as the initial guess.
    x(1)  = abar
    x(2)  = psi
    x(3)  = gamma
    x(4)  = sigmar
    x(5)  = sigmau
    x(6)  = taua
    x(7)  = taur
    x(8)  = tauw
    x(9)  = za
    x(10) = zm
    x(11) = z

! =============================================================================
! Check the initialization of the MPI Environment
    !WRITE (*,*) 'Node ', myrank, 'Report to Work.'
    !IF (myrank .eq. root) THEN
    !    WRITE (*,*) 'And I am the ROOT!'
    !END IF
    calipara = (/'Subsistence','Serv.Pref','Manu.Pref','Var.Rural',&
                 'Var.Urban','Tax.A','Tax.Corp','Tax.Inc',&
                 'Agri.Z','Manu.Z','Export.Z'/)
    calimoment = (/'PoorFood','Serv.C','Manu.C','R.Gini',&
                    'U.Gini','VAT.T','CorpT.T','IncT.T',&
                    'Serv.Y','Manu.Y','Export.Y'/)

    calidata(1) = cali_lowca_c
    calidata(2) = cali_cs_c
    calidata(3) = cali_cm_c
    calidata(4) = cali_gini_r
    calidata(5) = cali_gini_u
    !calidata(6) = cali_vat_tax
    calidata(6) = cali_tax_gdp
    calidata(7) = cali_corp_tax
    calidata(8) = cali_inc_tax
    calidata(9) = cali_cs_y
    calidata(10) = cali_cm_y
    calidata(11) = cali_ex_y
        !calidata(6) = 0.45_dp
    
! =============================================================================
! If the model is re-calibrated, invoke va05ad to minimize the square root of 
!   model implied values with data moments.
! If the model is not calibrated, invoke calfun and the distance between model
!   and data are reported.
    if ( calib ) then
       h = 1.0d-5
       acc = 1.0d-6
       dmax = 1.5_dp
       maxfun = 1000
       if (myrank .eq. root) then
       ! Only the root processor prints intermediate results of va05ad.
          iprint = 2
       else
          iprint = 0
       endif
       IF (myrank .eq. root) THEN
           WRITE (*,*) 'Start Calibrating the Model.'
       END IF
       call va05ad(m,n,f,x,h,dmax,acc,maxfun,iprint,ww)
    else
       IF (myrank .eq. root) THEN
           WRITE (*,*) 'Start Evaluating the Model.'
       END IF 
       call calfun(m,n,f,x)
    endif

! =============================================================================
! Root processor prints general equilibrium prices to file
    if (myrank .eq. root) then   
       open(1,file='parameters.txt',form='formatted')
          write(1,100) 'ps',eprice(1)
          write(1,100) 'pa',eprice(2)
          write(1,100) 'w ',eprice(3)
          write(1,100) 'wf',eprice(4)
          write(1,100) 'r ',eprice(5)
       100   format(a12,f16.10)
       close(1)
          write(*,107) 'ps',eprice(1)
          write(*,107) 'pa',eprice(2)
          write(*,107) 'w ',eprice(3)
          write(*,107) 'wf',eprice(4)
          write(*,107) 'r ',eprice(5)
107       format(a12,f16.10)
          
          write(*,*) ''
          write(*,*) 'Total Tax Revenue: ', vattax + personaltax + businesstax
          write(*,*) 'Net Tax:           ', aggtax
          write(*,*) 'GDP:               ', gdp          
! Root processor prints calibrated parameters to file       
       open(1,file='calibparams.txt',form='formatted')
          do i = 1,n
             write(1,141) x(i)
          enddo
       141   format(f16.10)
       close(1)
    endif  
    
    call cpu_time(etime)

! Root processor prints running time of the code to file           
    if (myrank .eq. root) then 
       WRITE (*,*) ''
       WRITE (*,*) 'Time ',(etime-btime)/60.0_dp, ' Elapsed.'
    endif
    
    call MPI_FINALIZE(mpiierr)  
    end program multisectors
    
