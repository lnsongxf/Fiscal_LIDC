      module params
      
         implicit none
         integer, parameter  :: dp = kind(1.0d0)
         
         ! logical  :: solveprices, equilibrium, extend 
         logical :: equilibrium, extend 
         integer, parameter :: nk2pts = 10001
         integer, parameter :: nkpts = 481
         integer, parameter :: nzpts = 15
         integer, parameter :: ntran = 42 !202
         integer, parameter :: ntran_in = 22
!         integer, parameter :: mval  = ntran*5
!!!!!!!! ffsqp
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
!!!!!!!! hybrd         
         integer, parameter :: nval = 5*(ntran-2)
	     integer, parameter :: ldfjac = nval
	     integer, parameter :: lr = nval*(nval+1)/2
         
         real(dp), parameter :: one = 1.0_dp
         real(dp), parameter :: zero = 0.0_dp
         real(dp), parameter :: ten = 10.0_dp
         real(dp), parameter :: infty = 1.0d+40
         real(dp), parameter :: error = 1.0d-9
         
         real(dp), parameter, dimension(ntran) :: pm = one
         real(dp), parameter :: pst = 20.7_dp
         real(dp), parameter :: zs = one

         real(dp), parameter :: tu = 0.0_dp
         real(dp), parameter :: tr = 0.0_dp
         real(dp), parameter :: tf = 0.0_dp
         
         real(dp), parameter :: alphaa = 0.49_dp
         real(dp), parameter :: alphas = 0.365_dp
         real(dp), parameter :: alphaa1 = 0.49_dp
         real(dp), parameter :: alphast1 = 0.49_dp
         real(dp), parameter :: alphast2 = 0.32_dp
         real(dp), parameter :: alpham = 0.365_dp

         real(dp), parameter :: dr = 0.273_dp
         real(dp), parameter :: smalld = 0.273_dp
         real(dp), parameter :: bigd = 1.0_dp

         real(dp), parameter :: delta = 0.06_dp
         real(dp), parameter :: beta = 0.96_dp

         real(dp), parameter :: muu = 0.28_Dp
         real(dp), parameter :: mur = 0.69_dp
         real(dp), parameter :: muf = one - muu - mur

         real(dp), parameter :: rhou = 0.915_dp
         real(dp), parameter :: rhor = 0.915_dp         

         real(dp), parameter :: abar   = 0.0008314562_dp
         real(dp), parameter :: psi    = 0.4944552241_dp
         real(dp), parameter :: gamma  = 0.8167964379_dp
         real(dp), parameter :: sigmar = 0.2338952483_dp
         real(dp), parameter :: sigmau = 0.6250133420_dp
         real(dp), parameter :: za     = 0.7434502896_dp
         real(dp), parameter :: zm     = 10.1858001797_dp
         real(dp), parameter :: z      = 0.6845109209_dp
         
         !! parameter values for toolkit version 2.0
         !real(dp), parameter :: abar   = 0.0008000000_dp
         !real(dp), parameter :: psi    = 0.4945000000_dp
         !real(dp), parameter :: gamma  = 0.8168000000_dp
         !real(dp), parameter :: sigmar = 0.2338000000_dp
         !real(dp), parameter :: sigmau = 0.7500000000_dp
         !real(dp), parameter :: za     = 0.7434000000_dp
         !real(dp), parameter :: zm     = 8.2858000000_dp
         !real(dp), parameter :: z      = 0.6845000000_dp         

      end module params