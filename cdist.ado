*! version 1.0.1  18mar2023  Ben Jann

capt mata: assert(mm_version()>=201)
if _rc {
    di as err "{bf:moremata} version 2.0.1 or newer is required; " _c
    di as err "type {stata ssc install moremata, replace}"
    error 499
}

program cdist, eclass
    version 14
    if replay() {
        Display `0'
        exit
    }
    gettoken subcmd 00 : 0, parse(" ,")
    if `"`subcmd'"'=="lincom"  {
        Lincom `00'
        exit
    }
    else if `"`subcmd'"'=="decomp" {
        Decomp `00'
        exit
    }
    local version : di "version " string(_caller()) ":"
    `version' _vce_parserun cdist, wtypes(pw iw) bootopts(force) jkopts(force): `0'
    if "`s(exit)'" == "" {
        if `"`subcmd'"'!=substr("estimate",1,max(3,strlen(`"`subcmd'"'))) {
            local 00 `0'
        }
        Estimate `00' // returns diopts, results
        if `"`lincom'"'!="" {
            _Lincom `lincom'
        }
        Display, `diopts'
        if `"`e(generate)'"'!="" {
            describe `e(generate)'
        }
    }
    ereturn local cmdline `"cdist `0'"'
end

program Display, rclass
    if `"`e(cmd)'"'!="cdist" {
        di as err "last {bf:cdist} results not found"
        exit 301
    }
    syntax [, noHEADer noLEGend noTABle vsquish * ]
    local options vsquish `options'
    if "`header'"=="" {
        local hflex 1
        if      c(stata_version)<17            local hflex 0
        else if d(`c(born_date)')<d(13jul2021) local hflex 0
        local w1 17
        local c1 49
        local c2 = `c1' + `w1' + 1
        local w2 10
        local c3 = `c2' + 2
        if `hflex' local headopts head2left(`w1') head2right(`w2')
        else       local headopts
        _coef_table_header, `headopts'
        if `hflex' { // if _coef_table_header used more space than allocated
            local offset1 = max(0, `s(head2_left)' - `w1')
            local offset2 = max(0, `s(head2_right)' - `w2')
            local c1 = `c1' - `offset1' - `offset2'
            local c2 = `c2' - `offset2'
        }
        if `"`e(pooled)'"'!="" local pooled "yes"
        else                   local pooled "no"
        di as txt _col(`c1') "Pooled" _col(`c2') "=" _col(`c3')/*
            */ as res %`w2's "`pooled'"
        di as txt "Group 0: `e(by)' = " as res e(by0) /*
            */ as txt _col(`c1') "N of obs 0" _col(`c2') "=" _col(`c3')/*
            */ as res %`w2'.0gc e(N0)
        di as txt "Group 1: `e(by)' = " as res e(by1) /*
            */ as txt _col(`c1') "N of obs 1" _col(`c2') "=" _col(`c3')/*
            */ as res %`w2'.0gc e(N1)
        di as txt _col(`c1') "Estimation method" _col(`c2') "=" _col(`c3')/*
            */ as res %`w2's e(method)
        if e(gsize0)==e(gsize1) {
            local gsize = e(gsize0)
            if `gsize'<. local gsize %`w2'.0g `gsize'
            else         local gsize %`w2's "(none)"
            di as txt _col(`c1') "Grid size" _col(`c2') "=" _col(`c3') /*
                */ as res `gsize'
        }
        else {
            forv i=0/1 {
                local gsize = e(gsize`i')
                if `gsize'<. local gsize %`w2'.0g `gsize'
                else         local gsize %`w2's "(none)"
                di as txt _col(`c1') "Grid size `i'" _col(`c2') "=" /*
                    */ _col(`c3') as res `gsize'
            }
        }
        if `"`e(coeftype)'"'!="" {
            di as txt _col(`c1') "Coefficient type" _col(`c2') "=" /*
            */ _col(`c3') as res  %`w2's e(coeftype)
        }
        di ""
    }
    if "`legend'"=="" {
        _lincom_legend "`header'" `e(lincom)'
    }
    if "`table'"=="" {
        eret display, `options'
        return add
        local xvars `"`e(indepvars)'"'
        if `"`xvars'"'!="" {
            if (strlen(`"`xvars'"')>66) {
                local xvars = substr(`"`xvars'"',1,63) + "..."
            }
        }
        else local xvars "(none)"
        di as txt "covariates: " as res `"`xvars'"'
    }
end

program _lincom_legend
    gettoken header 0 : 0
    local legend `"`0'"'
    local hasleg 0
    while (`"`legend'"'!="") {
        gettoken exp legend : legend, match(par)
        capt _on_colon_parse `exp'
        if _rc continue // no name
        local hasleg 1
    }
    if `hasleg'==0 exit
    local legend `"`0'"'
    if "`header'"!="" di ""
    while (`"`legend'"'!="") {
        gettoken exp legend : legend, match(par)
        capt _on_colon_parse `exp'
        if _rc continue // no name
        di as txt %13s abbrev(`"`s(before)'"',13) ":" as res `"`s(after)'"'
    }
    di ""
end

program Estimate, eclass
    // syntax
    syntax varlist(fv) [if] [in] [pw iw fw], by(varname) [ swap /*
        */ POOled jmp JMP2(str) /*
        */ Statistics(str) pdf PDF2(str) cdf CDF2(str) /*
        */ Percentile Percentile2(str) Quantile Quantile2(str) /*
        */ qdef(numlist max=1 int >=0 <=11) /*
        */ lincom(str) DECOmp DECOmp2(str) keep(str) drop(str) /*
        */ Method(str) Gsize(int 100) /*
        */ noINTegrate INTegrate2(numlist max=2 >=0) /*
        */ noDOTs noHEADer noLEGend noTABle /*
        */ GENerate GENerate2(str) replace * ]
    if "`qdef'"=="" local qdef 2
    _get_diopts diopts, `options'
    c_local diopts `header' `legend' `table' `diopts'
    
    // target statistics
    if `"`percentile2'"'!="" local percentile percentile
    if `"`quantile2'"'!=""   local quantile quantile
    if `"`pdf2'"'!=""        local pdf pdf
    if `"`cdf2'"'!=""        local cdf cdf
    local sopts statistics percentile quantile pdf cdf
    local tmp 0
    foreach opt of local sopts {
        local tmp = `tmp' + (`"``opt''"'!="")
    }
    if `tmp'>1 {
        local tmp 0
        di as err "only on of " _c
        foreach opt of local sopts {
            if (`"``opt''"'!="") {
                di "{bf:`opt'()}" _c
                if !`tmp' di " and " _c
                local ++tmp
                if `tmp'>1 continue, break
            }
        }
        di " allowed"
        exit 198
    }
    else if !`tmp' local statistics "mean" // the default
    if `"`statistics'"'!="" {
        _parse_statistics `statistics' // returns statistics
    }
    else if "`percentile'"!="" {
        _parse_stats_num "`percentile'" `"`percentile2'"' ">=0 <=100"
    }
    else if "`quantile'"!="" {
        _parse_stats_num "`quantile'" `"`quantile2'"' ">=0 <=1"
    }
    else { // pdf or cdf
        _parse_stats_num "`pdf'`cdf'" `"`pdf2'`cdf2'"' ""
    }
    
    // method
    _parse_method, `method'
    if "`jmp'`jmp2'"!="" {
        _parse_jmp, `jmp2' // returns jmp
    }
    if "`jmp'"!="" {
        if !inlist("`method'","qr","qr0") {
            di as error "{bf:jmp} requires {bf:method(qr)}"
            exit 198
        }
        if "`pooled'"!="" {
            di as error "{bf:jmp} and {bf:pooled} not both allowed"
            exit 198
        }
    }
    _parse_integrate `integrate2'
    if "`integrate'"!=""                   local integrate2 ""
    else if !inlist("`method'","qr","qr0") local integrate2 ""
    
    // keep()/drop()/decomp()/lincom()
    local eqs "obs0 fit0 adj0"
    if "`jmp'"!="" local eqs "`eqs' loc0"
    local eqs = "`eqs' " + subinstr("`eqs'","0","1",.)
    if `"`keep'"'!="" {
        if `"`drop'"'!="" {
            di as err "{bf:keep()} and {bf:drop()} not both allowed"
            exit 108
        }
        if `"`lincom'"'!="" {
            di as err "{bf:keep()} and {bf:lincom()} not both allowed"
            exit 108
        }
        if `"`decomp'`decom2'"'!="" {
            di as err "{bf:keep()} and {bf:decomp()} not both allowed"
            exit 108
        }
        mata: _parse_keep()
        if "`eqs'"=="" {
            di as err "invalid {bf:keep()}: no equations selected"
            exit 198
        }
    }
    else if `"`drop'"'!="" {
        if `"`lincom'"'!="" {
            di as err "{bf:drop()} and {bf:lincom()} not both allowed"
            exit 108
        }
        if `"`decomp'`decom2'"'!="" {
            di as err "{bf:drop()} and {bf:decomp()} not both allowed"
            exit 108
        }
        mata: _parse_drop()
        if "`eqs'"=="" {
            di as err "invalid {bf:drop()}: all equations dropped"
            exit 198
        }
    }
    else if `"`decomp'`decomp2'"'!="" {
        if `"`lincom'"'!="" {
            di as err "{bf:decomp()} and {bf:lincom()} not both allowed"
            exit 108
        }
        _parse_decomp, `decomp2' // updates d_reverse, d_average, d_threefold
        _Decomp_lincom "`pooled'" "`jmp'" "`d_reverse'" "`d_average'"/*
            */ "`d_threefold'" // returns lincom
    }
    if `"`lincom'"'!="" {
        _parse_lincom "`eqs'" `"`lincom'"'
        c_local lincom `"`lincom'"'
    }
    
    // expand generate
    _parse_generate "`eqs'" "`generate'" `"`generate2'"' "`replace'"
    
    // sample
    marksample touse
    markout `touse' `by'
    local wgt [`weight'`exp']
    _nobs `touse' `wgt'
    local N = r(N)
    qui levelsof `by' if `touse'
    local bylevels `r(levels)'
    if `:list sizeof bylevels'!=2 {
        di as err "{bf:by()} must identify two groups"
        exit 498
    }
    if "`swap'"!="" {
        local by0: word 2 of `bylevels'
        local by1: word 1 of `bylevels'
    }
    else {
        local by0: word 1 of `bylevels'
        local by1: word 2 of `bylevels'
    }
    forv i = 0/1 {
        tempname touse`i'
        qui gen byte `touse`i'' = `by'==`by`i'' & `touse'
        _nobs `touse`i'' `wgt'
        local N`i' = r(N)
    }
    
    // weights: generate variable
    if "`weight'"!="" {
        local wvar = substr(`"`exp'"', 3, .) // strip "= "
        capt confirm var `wvar'
        if _rc {
            tempname wvar
            qui gen double `wvar' `exp'
        }
    }
    
    // create tempvars for generate
    local tmpgen
    local ngen: list sizeof generate
    if `ngen' {
        local i 0
        foreach eq of local eqs {
            if "`eq'"=="obs0" continue
            if "`eq'"=="obs1" continue
            local ++i
            if `i'>`ngen' continue, break
            tempvar tmp
            local tmpgen `tmpgen' `tmp'
            qui gen double `tmp' = .
            lab var `tmp' "`eq': outcome values"
            local ++i
            if `i'>`ngen' continue, break
            tempvar tmp
            local tmpgen `tmpgen' `tmp'
            qui gen double `tmp' = .
            lab var `tmp' "`eq': weights"
        }
    }
    
    // estimate
    tempname b AT
    gettoken depvar xvars : varlist
    _fv_check_depvar `depvar'
    local xvars `xvars'
    fvrevar `xvars' if `touse' // create tempvars for factor variable terms
    local XVARS `r(varlist)'
    mata: cdist()
    
    // results
    eret post `b' `wgt', depname(`depvar') obs(`N') esample(`touse')
    eret local title         "Counterfactual distribution estimation"
    eret local cmd           "cdist"
    eret local method        "`method'"
    eret local indepvars     "`xvars'"
    eret local by            "`by'"
    eret local eqnames       "`eqs'"
    eret local pooled        "`pooled'"
    eret local jmp           "`jmp'"
    eret local statistics    "`statistics'"
    if "`percentile'`quantile'`pdf'`cdf'"!="" {
        eret local coeftype  "`percentile'`quantile'`pdf'`cdf'"
        eret matrix at       = `AT'
    }
    eret scalar N0           = `N0'
    eret scalar N1           = `N1'
    eret scalar by0          = `by0'
    eret scalar by1          = `by1'
    eret scalar gsize        = `gsize'
    eret scalar gsize0       = `gsize0'
    eret scalar gsize1       = `gsize1'
    if "`integrate2'"!="" {
        eret scalar integrate = `integrate2'
        eret scalar integ_pad = `integ_pad'
    }
    eret scalar k            = `k'
    eret scalar k_eq         = `: list sizeof eqs'
    eret scalar qdef         = `qdef'
    eret local generate "`generate'"
    if ("`B0'"!="")    eret matrix B0  = `B0'
    if ("`AT0'"!="")   eret matrix AT0 = `AT0'
    if ("`B1'"!="")    eret matrix B1  = `B1'
    if ("`AT1'"!="")   eret matrix AT1 = `AT1'
    if ("`B0loc'"!="") eret matrix B0loc = `B0loc'
    if ("`B1loc'"!="") eret matrix B1loc = `B1loc'
    
    // generate: rename tempvars
    if `ngen'==0 exit
    foreach tmpv of local tmpgen {
        gettoken v generate : generate
        if "`replace'"!="" {
            capt confirm new variable `v'
            if _rc==1 exit _rc
            if _rc drop `v'
        }
        rename `tmpv' `v'
    }
end

program _parse_method
    cap n syntax [, qr qr0 Logit ]
    if _rc {
        di as err "error in {bf:method()}"
        exit 198
    }
    
    local method `qr' `qr0' `logit'
    if `:list sizeof method'>1 {
        di as err "only one method allowed"
        exit 198
    }
    if "`method'"=="" local method logit
    c_local method `method'
end

program _parse_jmp
    syntax [, MEDian mean ]
    local jmp `median' `mean'
    if `: list sizeof jmp'>1 {
        di as err "{bf:jmp()}: only one keyword allowed"
        exit 198
    }
    if "`jmp'"=="" local jmp median
    c_local jmp `jmp'
end

program _parse_integrate
    args n pad
    if "`n'"==""   local n   1000
    if "`pad'"=="" local pad 5
    capt n numlist "`n'", integer range(>0)
    if _rc {
        di as err "error in integrate()"
        exit _rc
    }
    c_local integrate2 `n'
    c_local integ_pad  `pad'
end

program _parse_statistics
    local S
    while (`"`0'"'!="") {
        gettoken s 0 : 0, bind
        __parse_statistics, `s'
        local S `S' `s'
    }
    c_local statistics `S'
end

program __parse_statistics
    local stats Mean Variance sd MEDian iqr Gini mld theil cv vlog sdlog
    cap n syntax [, `stats' /*
        */ IQR2(numlist min=2 max=2 >=0 <=100) /*
        */ QRatio(numlist min=2 max=2 >=0 <=100) /*
        */ Q(numlist max=1 >=0 <=100) /*
        */ P(numlist max=1 >=0 <=100) /*
        */ ]
    if _rc {
        di as err "error in {bf:statistics()}"
        exit 198
    }
    local s
    else {
        local stats = strlower("`stats'")
        foreach stat of local stats {
            local s `s' ``stat''
        }
    }
    if "`iqr2'"!="" {
        local iqr2: subinstr local iqr2 " " ","
        local s `s' iqr(`iqr2')
    }
    if "`qratio'"!="" {
        local qratio: subinstr local qratio " " ","
        local s `s' qratio(`qratio')
    }
    if "`q'"!="" {
        local s `s' q(`q')
    }
    if "`p'"!="" {
        local s `s' p(`p')
    }
    c_local s `s'
end

program _parse_stats_num
    args nm nlist range
    // opt(#k)
    if substr(`"`nlist'"',1,1)=="#" {
        local nlist = substr(`"`nlist'"',2,.)
        capt n numlist `"`nlist'"', min(1) max(1) int range(>0)
        if _rc {
            di as err "error in {bf:`nm'()}"
            exit _rc
        }
        local nlist
        local k "`r(numlist)'"
    }
    // opt(numlist)
    else if `"`nlist'"'!="" {
        capt n numlist `"`nlist'"', range(`range')
        if _rc {
            di as err "error in {bf:`nm'()}"
            exit _rc
        }
        local nlist "`r(numlist)'"
        local k: list sizeof nlist
    }
    c_local `nm'2  "`nlist'"
    c_local `nm'_k "`k'"
end

program _parse_generate
    args eqs gen gen2 replace
    if `"`gen2'"'=="" {
        if "`gen'"=="" exit
        local gen2 "_cdist_*"
    }
    if `: list sizeof gen2'==1 {
        if substr(`"`gen2'"',-1,1)=="*" {
            local pfx = substr(`"`gen2'"',1,strlen(`"`gen2'"')-1)
            local gen2 ""
            foreach eq of local eqs {
                if "`eq'"=="obs0" continue
                if "`eq'"=="obs1" continue
                local gen2 `gen2' `pfx'`eq'_y `pfx'`eq'_w
            }
        }
    }
    if "`replace'"=="" {
        confirm new var `gen2'
    }
    else {
        confirm names `gen2'
    }
    c_local generate `gen2'
    c_local generate2 ""
end

program Decomp
    if `"`e(cmd)'"'!="cdist" {
        di as err "last {bf:cdist} results not found"
        exit 301
    }
    _parse_decomp * `0' // returns d_reverse, d_average, d_threefold, options
    _Decomp_lincom `"`e(pooled)'"' `"`e(jmp)'"' "`d_reverse'" "`d_average'"/*
            */ "`d_threefold'" // returns lincom
    Lincom `lincom', `options'
end

program _parse_decomp
    _parse comma star 0 : 0
    capt n syntax [, REVerse AVErage THREEfold `star' ]
    if _rc {
        if "`star'"=="" di as err "error in {bf:decomp()}"
        exit _rc
    }
    if "`average'"!="" & "`threefold'"!="" {
        di as err "only one of {bf:average} and {bf:threefold} allowed"
        if "`star'"=="" di as err "error in {bf:decomp()}"
        exit 198
    }
    if "`average'"!="" & "`reverse'"!="" {
        di as err "only one of {bf:average} and {bf:reverse} allowed"
        if "`star'"=="" di as err "error in {bf:decomp()}"
        exit 198
    }
    c_local d_reverse   `reverse'
    c_local d_average   `average'
    c_local d_threefold `threefold'
    if "`star'"=="" exit
    c_local options `options'
end

program _Decomp_lincom
    args pooled jmp reverse average threefold
    local Delta Delta // Delta    difference
    local Chars Chars // Delta_X  composition characteristics
    local Coefs Coefs // Delta_S  mechanism   coefficients
    local Resid Resid // Delta_e  residual
    local Inter Inter // Delta_XS interaction
    if `"`pooled'"'!="" {
        if "`reverse'"!="" {
            di as err "{bf:reverse} not allowed if {bf:pooled} has been specified"
            exit 198
        }
        if "`average'"!="" {
            di as err "{bf:average} not allowed if {bf:pooled} has been specified"
            exit 198
        }
        if "`threefold'"!="" {
            di as err "{bf:threefold} not allowed if {bf:pooled} has been specified"
            exit 198
        }
        local lincom (`Delta': fit0 - fit1) /*
                  */ (`Chars': fit0 - adj0 + adj1 - fit1) /*
                  */ (`Coefs': adj0 - adj1)
    }
    else if `"`jmp'"'!="" {
        if "`threefold'"!="" {
            di as err "{bf:threefold} not allowed if {bf:jmp} has been specified"
            exit 198
        }
        if "`average'"!="" {
            local lincom (`Delta': fit0 - fit1) /*
                      */ (`Chars': (fit0 - adj0 + adj1 - fit1)/2) /*
                      */ (`Coefs': (adj0 - loc0 + loc1 - adj1)/2) /*
                      */ (`Resid': (loc0 - fit1 + fit0 - loc1)/2)
        }
        else if "`reverse'"!="" {
            local lincom (`Delta': fit0 - fit1) /*
                      */ (`Chars': adj1 - fit1) /*
                      */ (`Coefs': loc1 - adj1) /*
                      */ (`Resid': fit0 - loc1)
        }
        else {
            local lincom (`Delta': fit0 - fit1) /*
                      */ (`Chars': fit0 - adj0) /*
                      */ (`Coefs': adj0 - loc0) /*
                      */ (`Resid': loc0 - fit1)
        }
    }
    else if "`average'"!="" {
        local lincom (`Delta': fit0 - fit1) /*
                  */ (`Chars': (fit0 - adj0 + adj1 - fit1)/2) /*
                  */ (`Coefs': (fit0 - adj1 + adj0 - fit1)/2)
    }
    else if "`reverse'"!="" {
        if "`threefold'"!="" {
            local lincom (`Delta': fit0 - fit1) /*
                      */ (`Chars': fit0 - adj0) /*
                      */ (`Coefs': fit0 - adj1) /*
                      */ (`Inter': adj0 + adj1 - fit0 - fit1)
        }
        else {
            local lincom (`Delta': fit0 - fit1) /*
                      */ (`Chars': adj1 - fit1) /*
                      */ (`Coefs': fit0 - adj1)
        }
    }
    else {
        if "`threefold'"!="" {
            local lincom (`Delta': fit0 - fit1) /*
                      */ (`Chars': adj1 - fit1) /*
                      */ (`Coefs': adj0 - fit1) /*
                      */ (`Inter': fit0 + fit1 - adj0 - adj1)
        }
        else {
            local lincom (`Delta': fit0 - fit1) /*
                      */ (`Chars': fit0 - adj0) /*
                      */ (`Coefs': adj0 - fit1)
        }
    }
    c_local lincom `lincom'
end

program _parse_lincom
    args eqs lincom
    // create e(b) and e(V) using eqs as colnames
    tempname ecurrent
    _estimates hold `ecurrent', restore nullok
    _Lincom_make_bV `eqs'
    // apply test and collect matched names
    tempname R
    _Lincom_get_R `R' `"`lincom'"'
end

program Lincom
    if `"`e(cmd)'"'!="cdist" {
        di as err "last {bf:cdist} results not found"
        exit 301
    }
    _parse comma lincom 0 : 0
    syntax [, HEADer noLEGend noTABle * ]
    if "`header'"=="" local header noheader
    _get_diopts diopts, `options'
    local diopts `header' `legend' `table' `diopts'
    _Lincom `lincom'
    Display, `diopts'
end

program _Lincom, eclass
    capt confirm matrix e(b_0)
    if _rc==0 local suffix "_0"     // use e(b_0), e(V_0), e(eqnames_0)
    else      local suffix ""
    // determine transformation matrix
    local eqs `"`e(eqnames`suffix')'"'
    tempname ecurrent
    _estimates hold `ecurrent', restore
    _Lincom_make_bV `eqs'
    tempname R
    _Lincom_get_R `R' `"`0'"'
    _estimates unhold `ecurrent'
    // apply transformations
    tempname b
    capt confirm matrix e(V`suffix')
    if _rc==0 tempname V
    mata: _Lincom_apply("`suffix'")
    // backup original results if needed
    if "`suffix'"=="" {
        tempname b_0
        mat `b_0' = e(b)
        if "`V'"!="" {
            tempname V_0
            mat `V_0' = e(V)
        }
        local eqnames_0 `"`e(eqnames)'"'
        local k_eq_0 = e(k_eq)
    }
    // post new results
    local eqs: coleq `b', quoted
    local eqs: list uniq eqs
    local eqs: list clean eqs
    if "`V'"!="" local V "V = `V'"
    eret repost b = `b' `V', resize
    eret local eqnames `"`eqs'"'
    eret local lincom `"`lincom'"'
    eret scalar k_eq = `: list sizeof eqs'
    // post original results if needed
    if "`suffix'"=="" {
        eret matrix b_0 = `b_0'
        if "`V'"!="" eret matrix V_0 = `V_0'
        eret local eqnames_0  `"`eqnames_0'"'
        eret scalar k_eq_0 = `k_eq_0'
    }
end

program _Lincom_make_bV
    tempname b V
    local k: list sizeof 0
    mat `b' = J(1,`k',0)
    mat coln `b' = `0'
    mat `V' = `b''*`b'
    eret post `b' `V'
end

program _Lincom_get_R
    args R lincom
    if `"`lincom'"'=="" local lincom "()"
    tempname r
    while (`"`lincom'"'!="") {
        gettoken exp lincom : lincom, match(par)
        capt _on_colon_parse `exp'
        if _rc==0 {
            local nm = strtrim(`"`s(before)'"')
            local exp `"`s(after)'"'
        }
        else local nm ""
        local exp: list retokenize exp
        if `"`nm'"'=="" {
            local nm `"`exp'"'
            local lcom `lcom' (`exp')
        }
        else {
            local lcom `lcom' (`nm': `exp')
        }
        if `"`exp'"'=="" {
            di as err "{it:exp} is missing"
            di as err "error in {bf:lincom()}"
            exit 198
        }
        capt n qui _test `exp' = 0 // evaluate exp
        if _rc {
            di as err "error in {bf:lincom()}"
            exit 198
        }
        mat `r' = get(Rr)
        capt n qui mat roweq `r' = `"`nm'"' // check name
        if _rc {
            di as err "error in {bf:lincom()}"
            exit 198
        }
        mat `R' = nullmat(`R') \ `r'
    }
    mat `R' = `R'[1...,1..colsof(`R')-1]
    c_local lincom `lcom'
end


version 14
mata:
mata set matastrict on

// helper functions called from ado -------------------------------------------

void _parse_keep()
{
    real scalar      n, i
    string rowvector keep, eqs
    real rowvector   idx, match, matched
    
    keep = tokens(st_local("keep"))
    n = length(keep)
    if (!n) return // nothing to do
    eqs = tokens(st_local("eqs"))
    matched = J(1, length(eqs), 0)
    idx = J(1, 0, .)
    for (i=1;i<=n;i++) {
         match = strmatch(eqs, keep[i]) :& !matched
         idx = idx, selectindex(match)
         matched = matched + match
    }
    st_local("eqs", invtokens(eqs[idx]))
}

void _parse_drop()
{
    real scalar      n, i
    string rowvector drop, eqs
    real rowvector   matched
    
    drop = tokens(st_local("drop"))
    n = length(drop)
    if (!n) return // nothing to do
    eqs = tokens(st_local("eqs"))
    matched = J(1, length(eqs), 0)
    for (i=1;i<=n;i++) matched = matched + strmatch(eqs, drop[i])
    st_local("eqs", invtokens(eqs[selectindex(matched:==0)]))
}

void _Lincom_apply(string scalar sfx)
{
    real scalar   k, i, j, i0, i1, j0, j1
    real matrix   R, r
    string matrix cstripe
    
    // expand r
    k = st_numscalar("e(k)")
    r = st_matrix(st_local("R"))
    i = rows(r)
    j = cols(r)
    R = J(i*k, j*k, .)
    i0 = i*k + 1
    for (;i;i--) {
        i1 = i0 - 1
        i0 = i0 - k
        j = cols(r)
        j0 = j*k + 1
        for (;j;j--) {
            j1 = j0 - 1
            j0 = j0 - k
            R[|i0,j0 \ i1,j1|] = I(k) * r[i,j]
        }
    }
    // evaluate
    cstripe = mm_expand(st_matrixrowstripe(st_local("R"))[,1], k, 1, 1), 
              J(rows(r), 1, st_matrixcolstripe("e(b"+sfx+")")[|1,2\k,2|])
    st_matrix(st_local("b"), st_matrix("e(b"+sfx+")") * R')
    st_matrixcolstripe(st_local("b"), cstripe)
    if (st_local("V")!="") {
        st_matrix(st_local("V"), R * st_matrix("e(V"+sfx+")") * R')
        st_matrixcolstripe(st_local("V"), cstripe)
        st_matrixrowstripe(st_local("V"), cstripe)
    }
}

// Main routine --------------------------------------------------------------

// struct for settings etc.

struct CDIST {
    string scalar          method,  // estimation method
                           jmp,     // type of locatioon shift
                           touse,   // varname of sample identifier
                           xvars,   // varnames of covariates
                           wvar     // varname of weights
    real scalar            g,       // target size of approximation grid
                           int_n,   // number of integration points (qr)
                           int_p,   // padding proportion of integration grid
                           pooled,  // used pooled X distribution
                           dots     // display progress dots
    string rowvector       eqs      // equations to be included in results
    real colvector         w        // pooled weights
    real matrix            X        // pooled X
    real rowvector         yminmax  // min and max of dependent variable
    pointer scalar         y0, y1,  // pointers to G0.y and G1.y
                           w0, w1   // pointers to G0.w and G1.w
    struct CDIST_S scalar  s        // target statistics
}

// struct for target statistics

struct CDIST_S {
    string scalar          typ      // type of target statistics
    real scalar            k        // number of statistics
    string colvector       s        // statistics (if typ==s)
    real colvector         at       // evaluation points
    real scalar            qdef     // quantile definition
    class mm_density scalar D       // density estimation object
    real scalar            pw,      // weights are pweights
                           bw       // bandwidth for pdf
}

// struct for groups

struct CDIST_G {
    string scalar          touse    // varname of group identifier
    real scalar            g        // size of approximation grid
    real colvector         y,       // depvar
                           w,       // weights
                           p        // evaluation grid (probabilities)
    real matrix            X        // covariates
    real matrix            b        // regression coefficients
    real colvector         bl       // coefficients of location regression
    real colvector         obs,     // results: observed
                           fit,     // results: fitted
                           adj,     // results: X adjusted
                           loc      // results: X adjusted and location shifted
    struct CDIST_V scalar  Y        // fitted and counterfactual outcomes
    struct CDIST_V scalar  W        // fitted and counterfactual weights
}

// struct for predictions and weights

struct CDIST_V {
    pointer (real colvector) scalar fit, adj, loc
}

// struct for progress dots

struct CDIST_DOTS {
    real scalar dots, i, j, n
}

// functions to retrieve overall min, max, and range of outcome variable
// (computations will only be done once)

real scalar _ymin(struct CDIST scalar S)
{
    if (!length(S.yminmax)) S.yminmax = minmax(*S.y0 \ *S.y1)
    return(S.yminmax[1])
}

real scalar _ymax(struct CDIST scalar S)
{
    if (!length(S.yminmax)) S.yminmax = minmax(*S.y0 \ *S.y1)
    return(S.yminmax[2])
}

real scalar _yrange(struct CDIST scalar S)
{
    if (!length(S.yminmax)) S.yminmax = minmax(*S.y0 \ *S.y1)
    return(S.yminmax[2]-S.yminmax[1])
}

// main function called by ado

void cdist()
{
    real scalar           j, i0, i1
    string colvector      s
    real colvector        b
    pointer scalar        f
    struct CDIST   scalar S
    struct CDIST_G scalar G0, G1
    
    // setup S
    S.dots   = st_local("dots")==""
    S.method = st_local("method")
    S.jmp    = st_local("jmp")
    S.pooled = st_local("pooled")!=""
    S.g      = strtoreal(st_local("gsize"))
    S.int_n  = strtoreal(st_local("integrate2"))
    S.int_p  = strtoreal(st_local("integ_pad"))/100
    S.eqs = tokens(st_local("eqs"))
    S.touse = st_local("touse")
    S.xvars = st_local("XVARS")
    S.wvar  = st_local("wvar")
    S.y0 = &G0.y; S.y1 = &G0.y;S.w0 = &G0.w; S.w1 = &G0.w
    // setup G0 and G1
    G0.touse = st_local("touse0")
    G1.touse = st_local("touse1")
    st_view(G0.y, ., st_local("depvar"), G0.touse)
    st_view(G1.y, ., st_local("depvar"), G1.touse)
    if (S.xvars!="") {
        st_view(G0.X=., ., S.xvars, G0.touse)
        st_view(G1.X=., ., S.xvars, G1.touse)
        if (S.pooled) st_view(S.X=., ., S.xvars, S.touse)
    }
    else {
        G0.X = J(rows(G0.y), 0, .)
        G1.X = J(rows(G1.y), 0, .)
        if (S.pooled) S.X = J(rows(st_data(., S.touse, S.touse)), 0, .)
    }
    if (S.wvar!="") {
        st_view(G0.w, ., S.wvar, G0.touse)
        st_view(G1.w, ., S.wvar, G1.touse)
        if (S.pooled) st_view(S.w, ., S.wvar, S.touse)
    }
    else {
        G0.w = G1.w = 1
        if (S.pooled) S.w = 1
    }
    // target stats
    S.s.qdef = strtoreal(st_local("qdef"))
    S.s.pw   = st_local("weight")=="pweight"
    s = _cdist_stats_setup(S)
    if (length(s)==0) {
        S.s.typ = "s"
        S.s.s   = s = tokens(st_local("statistics"))'
        S.s.k   = length(s)
    }
    // loc0/loc1: need to obtain median fits
    if (anyof(S.eqs,"loc0") | anyof(S.eqs,"loc1")) {
        if (S.jmp=="mean") {
            G0.bl = mm_lsfit(G0.y, G0.X, G0.w)
            G1.bl = mm_lsfit(G1.y, G1.X, G1.w)
        }
        else {
            G0.bl = mm_qrfit(G0.y, G0.X, G0.w, 0.5)
            G1.bl = mm_qrfit(G1.y, G1.X, G1.w, 0.5)
        }
    }
    // compute results
    if (S.method=="logit") f = &_cdist_dr()  // distribution regression
    else                   f = &_cdist_qr()  // quantile regression
    if (anyof(S.eqs,"obs0")) G0.obs = _cdist_stats(G0.y, G0.w, S.s,)
    (*f)(S, G0, G1, "0")
    if (anyof(S.eqs,"obs1")) G1.obs = _cdist_stats(G1.y, G1.w, S.s)
    (*f)(S, G1, G0, "1")
    // return results
    j = length(S.eqs)
    b = J(j*S.s.k, 1, .)
    i0 = j * S.s.k + 1
    for (;j;j--) {
        i1 = i0 - 1
        i0 = i0 - S.s.k
        if      (S.eqs[j]=="obs0") b[|i0 \ i1|] = G0.obs
        else if (S.eqs[j]=="fit0") b[|i0 \ i1|] = G0.fit
        else if (S.eqs[j]=="adj0") b[|i0 \ i1|] = G0.adj
        else if (S.eqs[j]=="loc0") b[|i0 \ i1|] = G0.loc
        else if (S.eqs[j]=="obs1") b[|i0 \ i1|] = G1.obs
        else if (S.eqs[j]=="fit1") b[|i0 \ i1|] = G1.fit
        else if (S.eqs[j]=="adj1") b[|i0 \ i1|] = G1.adj
        else if (S.eqs[j]=="loc1") b[|i0 \ i1|] = G1.loc
    }
    st_matrix(st_local("b"), b')
    st_matrixcolstripe(st_local("b"), (mm_expand(S.eqs', S.s.k, 1, 1),
        J(length(S.eqs),1,s)))
    st_local("gsize0", strofreal(G0.g))
    st_local("gsize1", strofreal(G1.g))
    st_local("k",      strofreal(S.s.k))
    if (length(S.s.at)) st_matrix(st_local("AT"), S.s.at')
    _cdist_store_coefs(S, G0, "0")
    _cdist_store_coefs(S, G1, "1")
    // store distribution functions in data (if requested)
    _cdist_store(S, G0, G1, tokens(st_local("tmpgen")))
}

string colvector _cdist_stats_setup(struct CDIST scalar S)
{
    string colvector s

    S.s.typ = st_local("percentile") + st_local("quantile") + st_local("cdf") +
              st_local("pdf")
    if (S.s.typ=="") return(J(0,1,""))
    if (st_local(S.s.typ+"2")!="") {
        s = tokens(st_local(S.s.typ+"2"))'
        S.s.at = strtoreal(s)
        S.s.k = length(s)
    }
    else {
        S.s.k = strtoreal(st_local(S.s.typ+"_k"))
        if (S.s.k>=.) S.s.k = 9
        if (S.s.typ=="percentile")    S.s.at = (1::S.s.k) / (S.s.k + 1) * 100
        else if (S.s.typ=="quantile") S.s.at = (1::S.s.k) / (S.s.k + 1)
        else                          S.s.at = rangen(_ymin(S), _ymax(S), S.s.k)
        s = strofreal(S.s.at)
        st_local(S.s.typ+"2", invtokens(s'))
    }
    if (S.s.typ=="pdf") _cdist_pdf_bw(S)
    return(s)
}

void _cdist_pdf_bw(struct CDIST scalar S)
{
    S.s.D.bw("dpi")
    S.s.D.data(*S.y0, *S.w0, S.s.pw)
    S.s.bw = S.s.D.h()
    S.s.D.data(*S.y1, *S.w1, S.s.pw)
    S.s.bw = (S.s.bw + S.s.D.h()) / 2
}

// functions for progress dots

void _cdist_progress_init(struct CDIST_DOTS scalar D, real scalar n,
    string scalar msg)
{
    if (D.dots==0) return
    D.n = n
    D.i = D.j = 0
    printf("{txt}"+msg)
    if (D.n) printf(" 0%%")
    displayflush()
}

void _cdist_progress_dot(struct CDIST_DOTS scalar D)
{
    if (D.dots==0) return
    D.i = D.i + 1
    if (D.j>=floor(D.i/D.n*20)) return
    printf(".")
    displayflush()
    D.j = D.j + 1
    if (!mod(D.j,4)) {
        printf("%g%%", D.j*5)
        displayflush()
    }
}

void _cdist_progress_done(struct CDIST_DOTS scalar D)
{
    if (D.dots==0) return
    if (D.n) printf("\n")
    else     printf("{txt} done\n")
    displayflush()
}

// store predictions and weights as variables

void _cdist_store(struct CDIST scalar S, struct CDIST_G scalar G0,
    struct CDIST_G scalar G1, string rowvector vnms)
{
    real scalar j, k, i, n, rc

    k = length(vnms)
    if (k==0) return
    n = length(S.eqs)
    j = rc = 0
    for (i=1;i<=n;i++) {
        if      (S.eqs[i]=="fit0") rc = _store(*G0.Y.fit, *G0.W.fit, j, k, vnms)
        else if (S.eqs[i]=="adj0") rc = _store(*G0.Y.adj, *G0.W.adj, j, k, vnms)
        else if (S.eqs[i]=="loc0") rc = _store(*G0.Y.loc, *G0.W.loc, j, k, vnms)
        else if (S.eqs[i]=="fit1") rc = _store(*G1.Y.fit, *G1.W.fit, j, k, vnms)
        else if (S.eqs[i]=="adj1") rc = _store(*G1.Y.adj, *G1.W.adj, j, k, vnms)
        else if (S.eqs[i]=="loc1") rc = _store(*G1.Y.loc, *G1.W.loc, j, k, vnms)
        if (rc) return
    }
}

real scalar _store(real colvector y, real colvector w, real scalar j, 
    real scalar k, string rowvector vnms)
{
    real scalar n
    
    // store Y
    j++
    if (j>k) return(1) // no more varnames
    n = rows(y)
    if (n>st_nobs()) _store_addobs(n)
    st_store((1,n), vnms[j], y)
    // store W
    j++
    if (j>k) return(1) // no more varnames
    if (rows(w)==1) st_store((1,n), vnms[j], J(n,1,w))
    else            st_store((1,n), vnms[j], w)
    return(0)
}

void _store_addobs(real scalar n)
{
    if (setmore()) {
        printf("need to create additional observations; ")
        printf("press break to abort\n")
        more()
    }
    stata("set obs " + strofreal(n, "%18.0g"))
    stata("replace \`touse' = 0 if \`touse'>=.", 1) // fix e(sample)
}

// function to copy coefficients to Stata

void _cdist_store_coefs(struct CDIST scalar S, struct CDIST_G scalar G,
    string scalar g)
{
    string scalar    nm
    string rowvector xvars
    string matrix    cs
    
    if (!length(G.b) & !length(G.bl)) return
    cs = tokens(st_local("xvars")), "_cons"
    cs = J(length(cs),1,""), cs'
    if (length(G.b)) {
        nm = st_tempname()
        st_local("B"+g, nm)
        if (S.method=="logit") {
            st_matrix(nm, G.b')
            st_matrixcolstripe(nm, cs)
        }
        else {
            st_matrix(nm, G.b')
            st_matrixcolstripe(nm, cs)
        }
        nm = st_tempname()
        st_local("AT"+g, nm)
        if (S.method=="logit") st_matrix(nm, (*G.Y.fit)[|1\G.g|])
        else                   st_matrix(nm, G.p)
    }
    if (length(G.bl)) {
        nm = st_tempname()
        st_local("B"+g+"loc", nm)
        st_matrix(nm, G.bl')
        st_matrixcolstripe(nm, cs)
    }
}

// function to generate predictions

real colvector _cdist_Xb(real matrix X, real colvector b, real scalar k)
{
    if (k==0) return(J(rows(X), 1, b))
    return(X * b[|1 \ k|] :+ b[k+1])
}

// subroutine to compute results based on quantile regression -----------------

void _cdist_qr(struct CDIST scalar S, struct CDIST_G scalar G,
    struct CDIST_G scalar G1, string scalar g)
{
    real rowvector           todo
    struct CDIST_DOTS scalar D
    
    // anything to do?
    todo = anyof(S.eqs,"fit"+g), anyof(S.eqs,"adj"+g), anyof(S.eqs,"loc"+g)
    if (!any(todo)) return
    // determine evaluation points (regular grid)
    G.g = S.g
    G.p = (1::G.g)/G.g :- .5/G.g
    // estimate coefficients
    D.dots = S.dots
    _cdist_progress_init(D, G.g, "group "+g+": fitting models")
    G.b = J(cols(G.X) + 1, G.g, .)
    if (S.method=="qr0") _cdist_qr_b0(G, D)
    else                 _cdist_qr_b(G, D)
    _cdist_progress_done(D)
    _cdist_progress_init(D, 0, "enumerating predictions ...")
    // setup integration grid (with 5% padding on each side)
    if (S.int_n<.) {
        G.Y.fit = G.Y.adj = G.Y.loc =
            &rangen(_ymin(S) - _yrange(S)*S.int_p,
                    _ymax(S) + _yrange(S)*S.int_p, S.int_n)
    }
    // obtain fit
    if (todo[1]) {
        _cdist_qr_p(G.Y.fit, G.W.fit, G.X, G.b, G.w)
        G.fit = _cdist_stats(*G.Y.fit, *G.W.fit, S.s)
    }
    // obtain adj
    if (todo[2]) {
        if (S.pooled) _cdist_qr_p(G.Y.adj, G.W.adj, S.X, G.b, S.w)
        else          _cdist_qr_p(G.Y.adj, G.W.adj, G1.X, G.b, G1.w)
        G.adj = _cdist_stats(*G.Y.adj, *G.W.adj, S.s)
    }
    // obtain loc
    if (todo[3]) {
        _cdist_qr_p(G.Y.loc, G.W.loc, G1.X, (G.b:-(G.bl-G1.bl)), G1.w)
        G.loc = _cdist_stats(*G.Y.loc, *G.W.loc, S.s)
    }
    _cdist_progress_done(D)
}

// generate predictions from quantile regressions (and possibly integrate)

void _cdist_qr_p(pointer scalar Y, pointer scalar W, real matrix X,
    real matrix B, real colvector w)
{
    real scalar    i, k, g, n
    real colvector F, p
    
    k = cols(X)
    g = cols(B)
    // Variant 1: nointegrate
    if (Y==NULL) {
        n = rows(X)
        Y = &J(n*g, 1, 0)
        for (i=1;i<=g;i++) (*Y)[|(i-1)*n+1 \ i*n|] = _cdist_Xb(X, B[,i], k)
        W = &(rows(w)==1 ? w/g : J(g, 1, w/g))
        return
    }
    // Variant 2: integrate
    n = rows(*Y)
    W = &J(n, 1, 0)
    if (rows(w)==1) {
        for (i=1;i<=g;i++) {
            F = _mm_relrank(sort(_cdist_Xb(X, B[,i], k),1), w, *Y, 0, 1)
            *W = *W + (mm_diff(0 \ F) :- *W) / i // mean updating
        }
         // some mass will be missing if there are predictions larger than
         // max(Y); add the missing mass to the top
        (*W)[n] = (*W)[n] + w:*rows(X) - quadsum(*W)
    }
    else {
        for (i=1;i<=g;i++) {
            F = _cdist_Xb(X, B[,i], k)
            p = order(F,1)
            F = _mm_relrank(F[p], w[p], *Y, 0, 1)
            *W = *W + (mm_diff(0 \ F) :- *W) / i // mean updating
        }
        (*W)[n] = (*W)[n] + quadsum(w) - quadsum(*W)
    }
}

// obtain quantile regressions

void _cdist_qr_b0(struct CDIST_G scalar G, struct CDIST_DOTS scalar D)
{
    real scalar i
    
    for (i=1; i<=G.g; i++) {
        G.b[,i] = mm_qrfit(G.y, G.X, G.w, G.p[i])
        _cdist_progress_dot(D)
    }
}

// obtain quantile regressions; somewhat faster than _cdist_qr_b0()

void _cdist_qr_b(struct CDIST_G scalar G, struct CDIST_DOTS scalar D)
{
    real scalar        i
    real colvector     b_mid, b_i
    class mm_qr scalar Q

    // get 1st estimate in middle
    Q.data(G.y, G.X, G.w)
    i = ceil(G.g/2)
    Q.p(G.p[i])
    G.b[,i] = b_mid = b_i = Q.b()
    i++
    _cdist_progress_dot(D)
    // move up
    for (; i<=G.g; i++) {
        Q.p(G.p[i])
        Q.b_init(b_i)
        G.b[,i] = b_i = Q.b()
        _cdist_progress_dot(D)
    }
    // move down
    i = ceil(G.g/2) - 1
    b_i = b_mid
    for (; i; i--) {
        Q.p(G.p[i])
        Q.b_init(b_i)
        G.b[,i] = b_i = Q.b()
        _cdist_progress_dot(D)
    }
}

// subroutine to compute results based on distribution regression -------------

void _cdist_dr(struct CDIST scalar S, struct CDIST_G scalar G,
    struct CDIST_G scalar G1, string scalar g)
{
    real rowvector           todo
    struct CDIST_DOTS scalar D
    
    // anything to do?
    todo = anyof(S.eqs,"fit"+g), anyof(S.eqs,"adj"+g), anyof(S.eqs,"loc"+g)
    if (!any(todo)) return
    // determine evaluation points
    if (mm_nunique(G.y)<=S.g) *G.Y.fit = mm_unique(G.y) // use observed values
    else {
        G.p = (1::S.g)/S.g:-.5/S.g
        G.Y.fit = &mm_unique(mm_quantile(G.y, G.w, G.p \ 1, 1/*low quantile*/))
    }
    G.g = rows(*G.Y.fit) - 1 // last element is maximum
    G.Y.adj = G.Y.fit
    // estimate coefficients
    D.dots = S.dots
    _cdist_progress_init(D, G.g, "group "+g+": fitting models")
    G.b = J(cols(G.X) + 1, G.g, .)
    _cdist_dr_logit(S, G, D, *G.Y.fit)
    assert(!hasmissing(G.b))
    _cdist_progress_done(D)
    // obtain fit
    _cdist_progress_init(D, 0, "enumerating predictions ...")
    if (todo[1]) {
        G.W.fit = &mm_diff(0 \ _cdist_dr_F(G.X, G.b, G.w))
        G.fit = _cdist_stats(*G.Y.fit, *G.W.fit, S.s)
    }
    // obtain adj
    if (todo[2]) {
        if (S.pooled) G.W.adj = &mm_diff(0 \ _cdist_dr_F(S.X, G.b, S.w))
        else          G.W.adj = &mm_diff(0 \ _cdist_dr_F(G1.X, G.b, G1.w))
        G.adj = _cdist_stats(*G.Y.fit, *G.W.adj, S.s)
    }
    _cdist_progress_done(D)
}

// subroutine to obtain distribution function from predictions

real colvector _cdist_dr_F(real matrix X, real matrix B, real colvector w)
{
    real scalar    i, k
    real colvector F
    
    k = cols(X)
    i = cols(B)
    F = J(i+1, 1, 1) // set last element to 1
    for (;i;i--) F[i] = mean(invlogit(_cdist_Xb(X, B[,i], k)), w)
    _sort(F, 1) // rearrange if there are crossings
    return(F)
}

// subroutine to fit the logit models in Stata

void _cdist_dr_logit(struct CDIST scalar S, struct CDIST_G scalar G,
    struct CDIST_DOTS scalar D, real colvector y)
{
    real scalar   i, Y
    string scalar Ynm, cmd
    
    Ynm = st_tempname()
    Y   = st_addvar("byte", Ynm)
    cmd = "version 10: logit " + // use old logit version; less overhead
          Ynm +  " " + S.xvars + " " +
          (S.wvar!="" ? "[iw=" + S.wvar + "] " : "") +
          "if " + G.touse + ", asis"
    for (i=1; i<=G.g; i++) {
        st_store(., Y, G.touse, G.y:<=y[i])
        stata(cmd, 1)
        G.b[,i] = st_matrix("e(b)")'
        _cdist_progress_dot(D)
    }
    st_dropvar(Ynm)
}

// subroutine to compute statistics from distribution -------------------------

real colvector _cdist_stats(real colvector y, real colvector w,
    struct CDIST_S scalar s)
{
    string rowvector S
    string scalar    si
    real scalar      i
    real colvector   b
    real matrix      tmp
    
    if (s.typ=="percentile") return(mm_quantile(y, w, s.at/100, s.qdef))
    if (s.typ=="quantile")   return(mm_quantile(y, w, s.at, s.qdef))
    if (s.typ=="cdf")        return(mm_relrank(y, w, s.at))
    if (s.typ=="pdf")        return(_cdist_stats_pdf(y, w, s))
    i = s.k
    b = J(i,1,.)
    for (;i;i--) {
        si = s.s[i]
        if      (si=="mean")     b[i] = mean(y, w)
        else if (si=="variance") b[i] = mm_variance0(y, w)
        else if (si=="sd")       b[i] = sqrt(mm_variance0(y, w))
        else if (si=="median")   b[i] = mm_median(y, w, s.qdef)
        else if (si=="iqr")      b[i] = mm_iqrange(y, w, s.qdef)
        else if (si=="gini")     b[i] = mm_gini(y, w)
        else if (si=="cv") {
            tmp = mean(y, w)
            if (tmp==0) {
                errprintf("statistic %s not allowed with outcome" +
                    " mean equal to zero\n", si)
                exit(3498)
            }
            b[i] = sqrt(mm_variance0(y, w)) / tmp
        }
        else if (si=="mld") {
            _cdist_stats_check_ypos(y, "mld")
            b[i] = ln(mean(y, w)) - mean(ln(y), w)
        }
        else if (si=="theil") {
            tmp = mean(y, w) 
            b[i] = mean(y:*ln(y), w)/tmp - ln(tmp)
        }
        else if (si=="vlog") {
            _cdist_stats_check_ypos(y, "vlog")
            b[i] = mm_variance0(ln(y), w)
        }
        else if (si=="sdlog") {
            _cdist_stats_check_ypos(y, "sdlog")
            b[i] = sqrt(mm_variance0(ln(y), w))
        }
        else {
            S = tokens(si, "(,)")
            if (S[1]=="iqr") {
                b[i] = mm_diff(mm_quantile(y, w, strtoreal(S[(3,5)])/100, s.qdef))
            }
            else if (S[1]=="qratio") {
                tmp = mm_quantile(y, w, strtoreal(S[(3,5)])/100, s.qdef)
                b[i] = tmp[2] / tmp[1]
            }
            else if (S[1]=="q") {
                b[i] = mm_quantile(y, w, strtoreal(S[3])/100, s.qdef)
            }
            else if (S[1]=="p") {
                b[i] = mm_quantile(y, w, strtoreal(S[3])/100, s.qdef)
            }
            else {
                errprintf("%s not supported\n", si)
                exit(3498)
            }
        }
    }
    return(b)
}

real colvector _cdist_stats_pdf(real colvector y, real colvector w,
    struct CDIST_S scalar s)
{
    s.D.data(y, w, s.pw)
    s.D.bw(s.bw)
    return(s.D.d(s.at, 1))
}

void _cdist_stats_check_ypos(real colvector y, string scalar s)
{
    if (min(y)>0) return
    errprintf("statistic %s not allowed with outcome values" + 
        "lower than or equal to 0\n", s)
    exit(3498)
}


end

exit

