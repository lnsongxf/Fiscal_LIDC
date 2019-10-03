! ----------------------------------------------------------------------------
!                           PROGRAM DESCRIPTION
! ----------------------------------------------------------------------------
! Purpose:
!   Module of Variables characterizing the Aggregate States of the Economy.
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
    
!*********************************************************************
    module aggstats
       use params
       implicit none
       real(dp), dimension(11) :: sumsqfunc
       
       real(dp), dimension(3) :: eu,poverty,ruralpoverty,urbanpoverty
       real(dp) :: dmfood,exfood,rmfood,service,manufacture,output
       real(dp) :: lowca, lowfca, meanca
       real(dp) :: personaltax,vattax,businesstax
       real(dp) :: agghu,aggha,agghr,agghst
       real(dP) :: aggku,aggkr,aggkf
       real(dp) :: aggwu,aggwr, aggeubar
       real(dp) :: aggca,aggcm,aggcs
       
       real(dp) :: aggcfa,aggcfs,aggcfm
       real(dp) :: aggcra,aggcrs,aggcrm
       real(dp) :: aggcua,aggcus,aggcum
       real(dp) :: gdp,agginv,export,aggtax,aggcon
       real(dp) :: aggcu,aggcr,aggcf
       real(dp) :: ffyr
       
       real(dp) :: ginicu,giniyu,giniku
       real(dp) :: ginicr,giniyr,ginikr
       real(dp) :: aggginic,aggginiy,aggginik
       
       real(dp) :: cpi
    CONTAINS
       subroutine initialized_aggstats

       implicit none
       poverty = zero
       ruralpoverty = zero
       urbanpoverty = zero
       eu = zero
       dmfood = zero
       exfood = zero
       rmfood = zero
       service = zero
       manufacture = zero
       output = zero
       aggeubar = zero

       personaltax = zero
       vattax = zero
       businesstax = zero
       
       cpi = zero
       
       agghu  = zero
       aggha  = zero
       agghr  = zero
       agghst = zero
       aggku  = zero
       aggkr  = zero
       aggkf  = zero
       aggwu  = zero
       aggwr  = zero
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       aggcus = zero
       aggcrs = zero
       aggcfs = zero
       aggcua = zero
       aggcra = zero
       aggcfa = zero
       aggcum = zero
       aggcrm = zero
       aggcfm = zero

       aggtax = zero
       gdp    = zero
       agginv = zero
       export = zero
       aggcon = zero
       
       aggcu  = zero 
       aggcr  = zero
       aggcf  = zero
       aggca  = zero
       aggcm  = zero
       aggcs  = zero
       
       ffyr   = zero
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       ginicu = zero
       giniyu = zero
       giniku = zero
       
       ginicr = zero
       giniyr = zero
       ginikr = zero
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       end subroutine initialized_aggstats
    end module aggstats