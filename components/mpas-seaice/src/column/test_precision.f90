

        PROGRAM test_precision
                
                !include "ice_kinds_mod.F90"
                integer, parameter :: char_len  = 80, &
                            char_len_long  = 256, &
                            log_kind  = kind(.true.), &
                            int_kind  = selected_int_kind(6), &
                            real_kind = selected_real_kind(6), &
                            dbl_kind  = selected_real_kind(6), &
                            r16_kind  = selected_real_kind(26)
                            real   (dbl_kind) :: calday
                            print *, dbl_kind
                            print *,  0.0_dbl_kind
                            calday = 1.4e10
                            print *, kind(calday)
        
        end PROGRAM test_precision
