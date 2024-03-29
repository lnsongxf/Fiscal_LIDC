! ----------------------------------------------------------------------------
!                           PROGRAM DESCRIPTION
! ----------------------------------------------------------------------------
! Purpose:
!   Module of parameters exogeneously calibrated.
!
! Author:
!     Xuan S. Tam @ City Univeristy of Hong Kong
!     Commented by: Xin Tang @ IMF, Spring 2017
!
! Record of Revisions:
!       Date                Description of Changes
!   ===========        =================================
!    12/26/2016                 Legacy Code
!
! ===============================================================================
    
    module params
      
         implicit none
         integer, parameter  :: dp = kind(1.0d0)

         integer, parameter :: nk2pts = 10001   ! # of Linear Grids
         integer, parameter :: nkpts = 481      ! # of Geometric Grids
         integer, parameter :: nzpts = 15       ! # of Markovian States
         integer, parameter :: mxiter = 10000
         integer, parameter :: npol = 200
         integer, parameter :: iwsize = 500
         integer, parameter :: nwsize = 900
         integer, parameter :: nparam = 2
         integer, parameter :: nf = 1
         integer, parameter :: nineq = 1
         integer, parameter :: nineqn = 1
         integer, parameter :: neq = 0
         integer, parameter :: neqn = 0
         integer, parameter :: nval = 5         ! # of market clearing conditions
	     integer, parameter :: ldfjac = nval
	     integer, parameter :: lr = nval*(nval+1)/2
         
         real(dp), parameter :: one = 1.0_dp
         real(dp), parameter :: zero = 0.0_dp
         real(dp), parameter :: ten = 10.0_dp
         real(dp), parameter :: infty = 1.0d+40
         real(dp), parameter :: error = 1.0d-9
         
         real(dp), parameter :: pm = one
         real(dp), parameter :: pst = 20.7_dp           ! Exchange Rate
         real(dp), parameter :: zs = one
         ! Start changing Service productivity
         
         real(dp), parameter :: tu = zero               ! Urban Transfer
         real(dp), parameter :: tr = zero               ! Rural Transfer
         real(dp), parameter :: tf = zero               ! Farmer Transfer
         
         real(dp), parameter :: alphaa = 0.49_dp        ! Land Share in Agri.
         real(dp), parameter :: alphas = 0.365_dp       ! 1 - service labor share
         real(dp), parameter :: alphaa1 = 0.49_dp       ! 1 - service labor share
         real(dp), parameter :: alphast1 = 0.49_dp      ! Land Share in Export
         real(dp), parameter :: alphast2 = 0.32_dp      ! Labor Share in Export
         real(dp), parameter :: alpham = 0.365_dp       ! 1 - manufacturing labor share

         real(dp), parameter :: dr = 0.273_dp           ! Land Size of Rural HH
         real(dp), parameter :: smalld = 0.273_dp       ! Farmer Land for Agri.
         real(dp), parameter :: bigd = 1.0_dp           ! Farmer Land for Export

         real(dp), parameter :: delta = 0.06_dp         ! Depreciation Rate
         real(dp), parameter :: beta = 0.96_dp          ! Discount Rate

         real(dp), parameter :: muu = 0.28_dp           ! Urban HH Measure
         real(dp), parameter :: mur = 0.69_dp           ! Rural HH Measure
         real(dp), parameter :: muf = one - muu - mur   ! Farmers Measure

         real(dp), parameter :: rhou = 0.915_dp         ! Urban Persistence
         real(dp), parameter :: rhor = 0.915_dp         ! Rural Persistence
         
         CHARACTER(LEN = 16), DIMENSION(11) :: calipara, calimoment

! ===============================================================================
! ===============================================================================
! Calibration Targets for Ethopia
         REAL(dp), PARAMETER :: cali_lowca_c = 0.52_dp          ! Moment 1
         REAL(dp), PARAMETER :: cali_cs_c = 0.21_dp             ! Moment 2
         REAL(dp), PARAMETER :: cali_cm_c = 0.33_dp             ! Moment 3
         REAL(dp), PARAMETER :: cali_gini_r = 0.26_dp           ! Moment 4
         REAL(dp), PARAMETER :: cali_gini_u = 0.40_dp           ! Moment 5
         !REAL(dp), PARAMETER :: cali_vat_tax = 0.45_dp          ! Moment 6
         REAL(dp), PARAMETER :: cali_tax_gdp = 0.08_dp         ! Moment 6'
         REAL(dp), PARAMETER :: cali_corp_tax = 0.30_dp         ! Moment 7
         REAL(dp), PARAMETER :: cali_inc_tax = 0.17_dp          ! Moment 8
         REAL(dp), PARAMETER :: cali_cs_y = 0.16_dp             ! Moment 9
         REAL(dp), PARAMETER :: cali_cm_y = 0.33_dp             ! Moment 10
         REAL(dp), PARAMETER :: cali_ex_y = 0.083_dp            ! Moment 11         
         
      end module params