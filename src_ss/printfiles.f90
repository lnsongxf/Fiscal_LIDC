!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine printfiles

    use params
    use calibparams
    use states
    use arrays
    use prices
    use aggstats
    use mpiparams
    implicit none
    
    open(1,file='ExpU.txt',form='formatted')
        write(1,101) eu(1),eu(2),eu(3)
        101 format(3(f12.6 /))
    close(1)
    
    open(1,file='moments.txt',form='formatted')
       write(1,102) poverty(1), &
                    poverty(2), &
                    poverty(3), &
                    urbanpoverty(1), &
                    urbanpoverty(2), &
                    urbanpoverty(3), &
                    ruralpoverty(1), &
                    ruralpoverty(2), &
                    ruralpoverty(3), &
                    ginicu, &
                    ginicr, &
                    giniyu, &
                    giniyr, &
                    giniku, &
                    ginikr, &
                    aggginic, &
                    aggginiy, &
                    aggginik, &
                    pm*manufacture/output, &
                    ps*muu*service/output, &
                    export/gdp, &
                    aggtax/gdp, &
                    personaltax/aggtax, &
                    businesstax/aggtax, &
                    vattax/aggtax, &
                    lowca/meanca, &
                    pm*aggcm/aggcon, &
                    ps*aggcs/aggcon, &
                    agghu, &
       				aggha, &
       				agghr, &
       				agghst, &
       				aggku, &
       				aggkr, &
                    aggkf, &
                    aggcus, &
                    aggcrs, &
                    aggcfs, &
       				aggcua, &
       				aggcra, &
       				aggcfa, &
       				aggcum, &
       				aggcrm, &
       				aggcfm, &
       				aggcu, &
       				aggcr, &
       				aggcf, &
       				ps*aggcs, &
       				pa*aggca, &
       				pm*aggcm, &
       				gdp, &
       				aggcon, &
       				agginv, &
       				aggtax, &
       				export, &
       				cpi

       102 format(100(f12.6 /))
    close(1)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    open(2,file='variables.txt',form='formatted')
       write(2,103) 'poverty,%ca', &
                    'poverty,ca ', &
                    'poverty,p*c', &
                    'u, p   ,%ca', &
                    'u, p   ,ca ', &
                    'u, p   ,p*c', &
                    'r, p   ,%ca', &
                    'r, p   ,ca ', &
                    'r, p   ,p*c', &
                    'ginicu     ', &
                    'ginicr     ', &
                    'giniyu     ', &
                    'giniyr     ', &
                    'giniku     ', &
                    'ginikr     ', &
                    'aggginic   ', &
                    'aggginiy   ', &
                    'aggginik   ', &
                    'ind/Output ', &
                    'ser/Output ', &
                    'exp/GDP    ', &
                    'tax/GDP    ', &
                    'ptax/aggtax', &
                    'btax/aggtax', &
                    'vtax/aggtax', &
                    'loca/lofca ', &
                    'cm/tot c   ', &
                    'cs/tot c   ', &
                    'agghu      ', &
       				'aggha      ', &
       				'agghr      ', &
       				'agghst     ', &
       				'aggku      ', &
       				'aggkr      ', &
                    'aggkf      ', &
                    'aggcus     ', &
                    'aggcrs     ', &
                    'aggcfs     ', &
       				'aggcua     ', &
       				'aggcra     ', &
       				'aggcfa     ', &
       				'aggcum     ', &
       				'aggcrm     ', &
       				'aggcfm     ', &
       				'aggcu      ', &
       				'aggcr      ', &
       				'aggcf      ', &
       				'aggcs      ', &
       				'aggca      ', &
       				'aggcm      ', &
       				'gdp        ', &
       				'aggcon     ', &
       				'agginv     ', &
       				'aggtax     ', &
       				'export     ', &
       				'cpi        '

       103 format(100(a15 /))
                    
    close(2)

    return
    end subroutine printfiles