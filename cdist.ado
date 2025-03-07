*! version 1.1.0  07mar2025  Ben Jann

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
            if "`e(coeftype)'"=="pdf" {
                di as txt _col(`c1') "PDF bandwidth" _col(`c2') "=" /*
                    */ _col(`c3') as res %`w2'.0g e(pdf_bwidth)
            }
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
        if `"`e(stats_nolog)'"'!="" {
            di as txt "(" as res `"`e(stats_nolog)'"' as txt " based on antilog)"
        }
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
        */ Statistics(str) islog pdf PDF2(str) cdf CDF2(str) /*
        */ Percentile Percentile2(str) Quantile Quantile2(str) /*
        */ qdef(numlist max=1 int >=0 <=11) /*
        */ lincom(str) DECOmp DECOmp2(str) keep(str) drop(str) /*
        */ Method(str) LOGfit Gsize(int 100) /*
        */ nobin BIN2(numlist max=2 >=0) /*
        */ noDOTs noHEADer noLEGend noTABle /*
        */ GENerate GENerate2(str) replace * ]
    if "`islog'"!="" & "`logfit'"!="" {
        di as err "{bf:logfit} and {bf:islog} not both allowed"
        exit 198
    }
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
    else if "`pdf'"!="" { // pdd
        _parse comma pdf2 pdfopts : pdf2 
        _parse_stats_num "`pdf'" `"`pdf2'"' ""
        _parse_pdfopts `pdfopts'
    }
    else { // cdf
        _parse_stats_num "`cdf'" `"`cdf2'"' ""
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
    _parse_bin `bin2'
    if "`bin'"!=""                         local bin2 ""
    else if !inlist("`method'","qr","qr0") local bin2 ""
    
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
        _parse_decomp, `decomp2' // d_reverse, d_average, d_threefold, d_swap
        _Decomp_lincom "`pooled'" "`jmp'" "`d_reverse'" "`d_average'"/*
            */ "`d_threefold'" "`d_swap'" // returns lincom
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
    tempname b AT BW
    gettoken depvar xvars : varlist
    _fv_check_depvar `depvar'
    local xvars `xvars'
    fvexpand `xvars' if `touse'
    local Xvars `r(varlist)'
    fvrevar `Xvars' if `touse' // create tempvars for factor variable terms
    local XVARS `r(varlist)'
    mata: cdist()
    
    // results
    eret post `b' `wgt', depname(`depvar') obs(`N') esample(`touse')
    eret local title         "Counterfactual distribution estimation"
    eret local cmd           "cdist"
    eret local method        "`method'"
    eret local logfit        "`logfit'"
    eret local indepvars     "`xvars'"
    eret local by            "`by'"
    eret local eqnames       "`eqs'"
    eret local pooled        "`pooled'"
    eret local jmp           "`jmp'"
    eret local statistics    "`statistics'"
    eret local islog         "`islog'"
    eret local stats_nolog   "`stats_nolog'"
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
    if "`bin2'"!="" {
        eret scalar bin     = `bin2'
        eret scalar bin_pad = `bin_pad'
    }
    eret scalar k            = `k'
    eret scalar k_eq         = `: list sizeof eqs'
    eret scalar qdef         = `qdef'
    if "`pdf'"!="" {
        eret scalar pdf_bwidth = `BW'
        if "`pdf_bwmethod'"!="" {
            if "`pdf_bwmethod'"=="dpi" {
                local pdf_bwmethod `pdf_bwmethod'(`pdf_bwdpi')
            }
            eret local pdf_bwmethod "`pdf_bwmethod'"
        }
        eret local pdf_kernel    "`pdf_kernel'"
        eret scalar pdf_adaptive = `pdf_adaptive'
        if `pdf_ll'<. | `pdf_ul'<. {
            eret local pdf_boundary "`pdf_boundary'"
            if `pdf_ll'<. eret scalar pdf_ll = `pdf_ll'
            if `pdf_ll'<. eret scalar pdf_ul = `pdf_ul'
        }
    }
    eret local generate      "`generate'"
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

program _parse_pdfopts
    syntax [, ///
        BWidth(str) Kernel(string) ADAPTive(numlist int >=0 max=1) ///
        ll(numlist max=1 missingok) ul(numlist max=1 missingok) ///
        BOundary(str) ]
    if "`ll'"=="" local ll .
    if "`ul'"=="" local ul .
    if `ll'<. & `ll'>`ul' {
        di as err "{bf:pdf(,ll())} may not be larger than {bf:pdf(,ul())}"
        exit 198
    }
    _parse_pdfopts_bw `bwidth'
    if `"`kernel'"'=="" local kernel "gaussian"
    if "`adaptive'"=="" local adaptive 0
    _parse_pdfopts_boundary, `boundary'
    c_local pdf_bwidth `bwidth'
    c_local pdf_bwmethod `bwmethod'
    c_local pdf_bwdpi `bwdpi'
    c_local pdf_kernel `"`kernel'"'
    c_local pdf_adaptive `adaptive'
    c_local pdf_ll `ll'
    c_local pdf_ul `ul'
    c_local pdf_boundary `boundary'
end

program _parse_pdfopts_bw // returns: bwidth, bwmethod, bwdpi
    // case 1: bwidth(numlist) => return bwidth
    capt numlist `"`0'"'
    if _rc==1 exit _rc
    if _rc==0 {
        local 0 `", bwidth(`0')"'
        capt n syntax, bwidth(numlist >0 max=1)
        if _rc==1 exit _rc
        if _rc {
            di as err "error in option {bf:pdf(,bwidth())}"
            exit _rc
        }
        c_local bwidth `"`bwidth'"'
        exit
    }
    // case 2; bwidth(method) => return bwmethod, bwdpi
    local 0 `", `0'"'
    capt n syntax [, Silverman Normalscale Oversmoothed SJpi /*
        */ Dpi Dpi2(numlist int >=0 max=1) ISJ ]
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:pdf(,bwidth())}"
        exit 198
    }
    if "`dpi2'"!="" local dpi dpi
    local bwmethod `silverman' `normalscale' `oversmoothed' `sjpi' `dpi' `isj'
    if "`bwmethod'"=="" {
        local bwmethod "dpi"
        local dpi2 2
    }
    if `: list sizeof bwmethod'>1 {
        di as err "too many methods specified"
        di as err "error in option {bf:pdf(,bwidth())}"
        exit 198
    }
    c_local bwidth
    c_local bwmethod `bwmethod'
    c_local bwdpi `dpi2'
end

program _parse_pdfopts_boundary // returns: boundary
    capt n syntax [, RENorm REFlect lc ]
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:pdf(,boundary())}"
        exit 198
    }
    local boundary `renorm' `reflect' `lc'
    if "`boundary'"=="" local boundary "renorm"
    if `: list sizeof boundary'>1 {
        di as err "too many methods specified"
        di as err "error in option {bf:pdf(,boundary())}"
        exit 198
    }
    c_local boundary `boundary'
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

program _parse_bin
    args n pad
    if "`n'"==""   local n   1000
    if "`pad'"=="" local pad 5
    capt n numlist "`n'", integer range(>0)
    if _rc {
        di as err "error in bin()"
        exit _rc
    }
    c_local bin2 `n'
    c_local bin_pad  `pad'
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
    _parse_decomp * `0' // d_reverse, d_average, d_threefold, d_swap, options
    _Decomp_lincom `"`e(pooled)'"' `"`e(jmp)'"' "`d_reverse'" "`d_average'"/*
            */ "`d_threefold'" "`d_swap'" // returns lincom
    Lincom `lincom', `options'
end

program _parse_decomp
    _parse comma star 0 : 0
    capt n syntax [, REVerse AVErage THREEfold swap `star' ]
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
    c_local d_swap      `swap'
    if "`star'"=="" exit
    c_local options `options'
end

program _Decomp_lincom
    args pooled jmp reverse average threefold swap
    local Delta Delta // Delta    difference
    local Chars Chars // Delta_X  composition characteristics
    local Coefs Coefs // Delta_S  mechanism   coefficients
    local Resid Resid // Delta_e  residual
    local Inter Inter // Delta_XS interaction
    if "`swap'"!="" {
        local 0 1
        local 1 0
    }
    else {
        local 0 0
        local 1 1
    }
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
        local lincom (`Delta': fit`0' - fit`1') /*
                  */ (`Chars': fit`0' - adj`0' + adj`1' - fit`1') /*
                  */ (`Coefs': adj`0' - adj`1')
    }
    else if `"`jmp'"'!="" {
        if "`threefold'"!="" {
            di as err "{bf:threefold} not allowed if {bf:jmp} has been specified"
            exit 198
        }
        if "`average'"!="" {
            local lincom (`Delta': fit`0' - fit`1') /*
                      */ (`Chars': (fit`0' - adj`0' + adj`1' - fit`1')/2) /*
                      */ (`Coefs': (adj`0' - loc`0' + loc`1' - adj`1')/2) /*
                      */ (`Resid': (loc`0' - fit`1' + fit`0' - loc`1')/2)
        }
        else if "`reverse'"!="" {
            local lincom (`Delta': fit`0' - fit`1') /*
                      */ (`Chars': adj`1' - fit`1') /*
                      */ (`Coefs': loc`1' - adj`1') /*
                      */ (`Resid': fit`0' - loc`1')
        }
        else {
            local lincom (`Delta': fit`0' - fit`1') /*
                      */ (`Chars': fit`0' - adj`0') /*
                      */ (`Coefs': adj`0' - loc`0') /*
                      */ (`Resid': loc`0' - fit`1')
        }
    }
    else if "`average'"!="" {
        local lincom (`Delta': fit`0' - fit`1') /*
                  */ (`Chars': (fit`0' - adj`0' + adj`1' - fit`1')/2) /*
                  */ (`Coefs': (fit`0' - adj`1' + adj`0' - fit`1')/2)
    }
    else if "`reverse'"!="" {
        if "`threefold'"!="" {
            local lincom (`Delta': fit`0' - fit`1') /*
                      */ (`Chars': fit`0' - adj`0') /*
                      */ (`Coefs': fit`0' - adj`1') /*
                      */ (`Inter': adj`0' + adj`1' - fit`0' - fit`1')
        }
        else {
            local lincom (`Delta': fit`0' - fit`1') /*
                      */ (`Chars': adj`1' - fit`1') /*
                      */ (`Coefs': fit`0' - adj`1')
        }
    }
    else {
        if "`threefold'"!="" {
            local lincom (`Delta': fit`0' - fit`1') /*
                      */ (`Chars': adj`1' - fit`1') /*
                      */ (`Coefs': adj`0' - fit`1') /*
                      */ (`Inter': fit`0' + fit`1' - adj`0' - adj`1')
        }
        else {
            local lincom (`Delta': fit`0' - fit`1') /*
                      */ (`Chars': fit`0' - adj`0') /*
                      */ (`Coefs': adj`0' - fit`1')
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
                           xvars,   // varnames of covariates (after fvrevar)
                           wvar     // varname of weights
    real scalar            g,       // target size of approximation grid
                           logfit,  // use log scale for estimation
                           bin_n,   // size of binning grid (qr)
                           bin_p,   // padding proportion of binning grid
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
    real colvector         at,      // evaluation points
                           nolog    // statistics using exp(depvar)
    class mm_density scalar D       // density estimation object
    real scalar            qdef,    // quantile definition
                           islog,   // use exp(depvar) for inequality measures
                           pw,      // weights are pweights
                           bw       // bandwidth for pdf
}

// struct for groups

struct CDIST_G {
    string scalar          touse    // varname of group identifier
    real scalar            g        // size of approximation grid
    real colvector         y,       // depvar
                           w,       // weights
                           p,       // evaluation grid (probabilities)
                           at       // evaluation grid (levels)
    real matrix            X        // covariates
    real matrix            b        // regression coefficients
    pointer rowvector      R        // rules for perfect predictors (logit)
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
    S.logfit = st_local("logfit")!=""
    S.jmp    = st_local("jmp")
    S.pooled = st_local("pooled")!=""
    S.g      = strtoreal(st_local("gsize"))
    S.bin_n  = strtoreal(st_local("bin2"))
    S.bin_p  = strtoreal(st_local("bin_pad"))/100
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
    S.s.islog = st_local("islog")!=""
    S.s.qdef  = strtoreal(st_local("qdef"))
    S.s.pw    = st_local("weight")=="pweight"
    s = _cdist_stats_setup(S)
    if (length(s)==0) {
        S.s.typ = "s"
        S.s.s   = s = tokens(st_local("statistics"))'
        S.s.k   = length(s)
        if (S.s.islog) S.s.nolog = J(S.s.k, 1, 0)
    }
    // method
    if (S.method=="logit") f = &_cdist_dr()  // distribution regression
    else                   f = &_cdist_qr()  // quantile regression
    // obs0/obs1
    if (anyof(S.eqs,"obs0")) G0.obs = _cdist_stats(G0.y, G0.w, S.s,)
    if (anyof(S.eqs,"obs1")) G1.obs = _cdist_stats(G1.y, G1.w, S.s)
    // transform data in case of logfit
    if (S.logfit) {
        G0.y = ln(G0.y)
        G1.y = ln(G1.y)
        S.yminmax = J(1,0,.)
    }
    // loc0/loc1: need to obtain median fits
    if (anyof(S.eqs,"loc0") | anyof(S.eqs,"loc1")) {
        if (S.jmp=="mean") {
            G0.bl = mm_lsfit(G0.y, G0.X, G0.w)
            G1.bl = mm_lsfit(G1.y, G1.X, G1.w)
        }
        else {
            G0.bl = _cdist_lad(G0.y, G0.X, G0.w, S.method)
            G1.bl = _cdist_lad(G1.y, G1.X, G1.w, S.method)
        }
    }
    // compute results
    (*f)(S, G0, G1, "0")
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
    if (S.s.typ=="pdf") st_numscalar(st_local("BW"), S.s.bw)
    // list of statistics that have been based on exp(depvar)
    if (S.s.islog) {
        if (any(S.s.nolog))
            st_local("stats_nolog", invtokens(select(S.s.s, S.s.nolog)'))
    }
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
    if (S.s.typ=="pdf") _cdist_pdf_setup(S)
    return(s)
}

void _cdist_pdf_setup(struct CDIST scalar S)
{
    S.s.D.kernel(st_local("pdf_kernel"), strtoreal(st_local("pdf_adaptive")))
    S.s.D.support(strtoreal((st_local("pdf_ll"), st_local("pdf_ul"))), 
        st_local("pdf_boundary")=="lc" ? "linear" : st_local("pdf_boundary"))
    if (st_local("pdf_bwidth")!="") S.s.bw = strtoreal(st_local("pdf_bwidth"))
    else {
        S.s.D.bw(st_local("pdf_bwmethod"), ., strtoreal(st_local("pdf_bwdpi")))
        S.s.D.data(*S.y0, *S.w0, S.s.pw)
        S.s.bw = S.s.D.h()
        S.s.D.data(*S.y1, *S.w1, S.s.pw)
        S.s.bw = (S.s.bw + S.s.D.h()) / 2
    }
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
    string matrix    cs
    
    if (!length(G.b) & !length(G.bl)) return
    cs = tokens(st_local("Xvars")), "_cons"
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
        if (S.method=="logit") st_matrix(nm, (G.at)[|1\G.g|])
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

// function to clip from below
void _cdist_clipmin(real colvector x, real scalar min)
{
    real colvector p
    
    p = x:<min
    if (any(p)) {
        p = selectindex(p)
        x[p] = J(length(p), 1, min)
    }
}

// median regression, optionally using preprocessing algorithm 

real colvector _cdist_lad(real colvector y, real matrix X, real colvector w,
    string scalar method)
{
    real scalar        k, M, m, n
    real rowvector     yLH
    real matrix        XXinv
    class mm_qr scalar Q
    pragma unset XXinv
    
    if (method=="qr0") return(mm_qrfit(y, X, w, 0.5))
    // preprocessing algorithm; see _cdist_qr_b() below
    yLH = minmax(y)
    yLH = 2*yLH[1] - yLH[2], 2*yLH[2] - yLH[1]
    k = __cdist_qr_b_XXinv(y, X, w, XXinv)
    n = rows(y)
    m = 0.8
    M = (k * n)^(2/3)
    Q.p(0.5)
    __cdist_qr_b_a1(Q, y, X, w, m, n, M, yLH, XXinv)
    return(Q.b())
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
    if (S.method=="qr0") _cdist_qr0_b(G, D)
    else                 _cdist_qr_b(G, D)
    _cdist_progress_done(D)
    _cdist_progress_init(D, 0, "enumerating predictions ...")
    // setup binning grid (with 5% padding on each side)
    if (S.bin_n<.) {
        G.Y.fit = G.Y.adj = G.Y.loc =
            &rangen(_ymin(S) - _yrange(S)*S.bin_p,
                    _ymax(S) + _yrange(S)*S.bin_p, S.bin_n)
    }
    // obtain fit
    if (todo[1]) {
        _cdist_qr_p(G.Y.fit, G.W.fit, G.X, G.b, G.w)
        if (S.logfit) G.Y.fit = &exp(*G.Y.fit)
        G.fit = _cdist_stats(*G.Y.fit, *G.W.fit, S.s)
    }
    // obtain adj
    if (todo[2]) {
        if (S.pooled) _cdist_qr_p(G.Y.adj, G.W.adj, S.X, G.b, S.w)
        else          _cdist_qr_p(G.Y.adj, G.W.adj, G1.X, G.b, G1.w)
        if (S.logfit) G.Y.adj = &exp(*G.Y.adj)
        G.adj = _cdist_stats(*G.Y.adj, *G.W.adj, S.s)
    }
    // obtain loc
    if (todo[3]) {
        _cdist_qr_p(G.Y.loc, G.W.loc, G1.X, (G.b:-(G.bl-G1.bl)), G1.w)
        if (S.logfit) G.Y.loc = &exp(*G.Y.loc)
        G.loc = _cdist_stats(*G.Y.loc, *G.W.loc, S.s)
    }
    _cdist_progress_done(D)
}

// generate predictions from quantile regressions (possibly binned)

void _cdist_qr_p(pointer scalar Y, pointer scalar W, real matrix X,
    real matrix B, real colvector w)
{
    real scalar    i, k, g, n
    
    k = cols(X)
    g = cols(B)
    // Variant 1: no binning
    if (Y==NULL) {
        n = rows(X)
        Y = &J(n*g, 1, .)
        for (i=1;i<=g;i++) (*Y)[|(i-1)*n+1 \ i*n|] = _cdist_Xb(X, B[,i], k)
        W = &(rows(w)==1 ? w/g : J(g, 1, w/g))
        return
    }
    // Variant 2: binning
    W = &J(rows(*Y), 1, 0)
    for (i=1;i<=g;i++) {
        *W = quadrowsum((*W, mm_linbin(_cdist_Xb(X, B[,i], k), w, *Y)))
    }
    *W = *W / g
}

// obtain quantile regressions

void _cdist_qr0_b(struct CDIST_G scalar G, struct CDIST_DOTS scalar D)
{
    real scalar i
    
    for (i=1; i<=G.g; i++) {
        G.b[,i] = mm_qrfit(G.y, G.X, G.w, G.p[i])
        _cdist_progress_dot(D)
    }
}

// obtain quantile regressions using preprocessing algorithm (see Algorithms 1
// and 2 in Chernozhukov et al. 2022, Fast algorithms for the quantile
// regression process, Empirical Economics 62:7–33)

void _cdist_qr_b(struct CDIST_G scalar G, struct CDIST_DOTS scalar D)
{
    real scalar        i, i0, j, k, M, m, n
    real rowvector     yLH
    real colvector     idx
    real matrix        XXinv
    class mm_qr scalar Q
    pragma unset XXinv

    // settings for preselection algorithm
    // - lower and upper y-values for the "globs"
    yLH = minmax(G.y)
    yLH = 2*yLH[1] - yLH[2], 2*yLH[2] - yLH[1] // = (min-range, max+range)
    // - setup indices for looping
    i = ceil(G.g/2) // middle
    idx = (i>1 ? 1::(i-1) : J(0,1,.)) \ (i<G.g ? -(G.g::(i+1)) : J(0,1,.))
    // - get number of parameters and XXinv
    k = __cdist_qr_b_XXinv(G.y, G.X, G.w, XXinv)
    // get 1st estimate (in middle) using preprocessing algorithm 1
    n = rows(G.y)
    m = 0.8
    M = (k * n)^(2/3)
    Q.p(G.p[i])
    __cdist_qr_b_a1(Q, G.y, G.X, G.w, m, n, M, yLH, XXinv)
    G.b[,i] = Q.b()
    _cdist_progress_dot(D)
    // obtain remaining estimates (first moving up, then moving down) using
    // preprocessing algorithm 2
    m = 1 // (Chernozhukov et al. use m = 3)
    M = sqrt(k * n)
    for (j=length(idx); j; j--) {
        i  = abs(idx[j])
        i0 = i + sign(idx[j])
        Q.p(G.p[i])
        __cdist_qr_b_a2(Q, G.y, G.X, G.w, m, n, M, yLH, G.b[,i0])
        G.b[,i] = Q.b()
        _cdist_progress_dot(D)
    }
}

real scalar __cdist_qr_b_XXinv(real colvector y, real matrix X, 
    real colvector w, real matrix XXinv)
{
    transmorphic S
    
    S = mm_ls(y, X, w)
    XXinv = mm_ls_XXinv(S)
    return(cols(X)+1-mm_ls_k_omit(S)) // k
}

real colvector __cdist_qr_b_z(real matrix X, real matrix XXinv)
{
    real scalar    k
    real colvector z
    
    k = cols(X)
    if (!k) return(1) // no covariates
    else    z = sqrt(rowsum((X * XXinv[|1,1\k,.|] :+ XXinv[k+1,]):^2))
    _cdist_clipmin(z, 1e-6)
    return(z)
}

void __cdist_qr_b_a1(class mm_qr scalar Q, real colvector y, real matrix X,
    real colvector w, real scalar m0, real scalar n, real scalar M,
    real rowvector yLH, real matrix XXinv)
{
    real scalar    m
    real colvector b0, r, p
    real colvector z
    
    // compute z
    m = m0
    if (trunc(m*M)<n) z = __cdist_qr_b_z(X, XXinv)
    // outer loop (subsampling)
    while (1) {
        if (trunc(m*M)>=n) {
            Q.data(y, X, w) // no subsampling
            return
        }
        p = mm_srswor(trunc(m*M), n, 0, 1)
        Q.data(y[p], X[p,], rows(w)!=1 ? w[p] : 1)
        b0 = Q.b()
        r = (y - _cdist_Xb(X, b0, cols(X))) :/ z
        // inner loop (find solution)
        if (__cdist_qr_b_inner(Q, y, X, w, m, n, M, yLH, b0, r)) return
    }
}

void __cdist_qr_b_a2(class mm_qr scalar Q, real colvector y, real matrix X,
    real colvector w, real scalar m0, real scalar n, real scalar M,
    real rowvector yLH, real colvector b0)
{
    real scalar    m
    real colvector r
    
    m = m0
    r = y - _cdist_Xb(X, b0, cols(X))
    while (1) {
        if (__cdist_qr_b_inner(Q, y, X, w, m, n, M, yLH, b0, r)) return
    }
}

real scalar __cdist_qr_b_inner(class mm_qr scalar Q, real colvector y,
    real matrix X, real colvector w, real scalar m, real scalar n,
    real scalar M, real rowvector yLH, real colvector b0, real colvector r)
{
    real scalar    wt, nL, nH, mL, mH
    real rowvector qLH
    real colvector sL, sH, p, pL, pH, qL, qH
    real colvector yy, ww
    real matrix    xx
    
    // identify the subsets of obs below and above the target window
    wt  = rows(w)!=1
    qLH = mm_quantile(r, w, (Q.p()-m*M/(2*n), Q.p()+m*M/(2*n)))
    sL  = r :< qLH[1]; pL = selectindex(sL); nL = length(pL)
    sH  = r :> qLH[2]; pH = selectindex(sH); nH = length(pH)
    // loop until convergence
    while (1) {
        // full sample
        if (nL==0 & nH==0) {
            Q.data(y, X, w)
            Q.b_init(b0)
            return(1)
        }
        // obtain fit from data window and globs
        p = selectindex(!sL :& !sH)
        yy = y[p]
        xx = X[p,]
        ww = wt ? w[p] : J(n-nL-nH, 1, 1)
        if (nL) {
            yy = yy \ yLH[1]
            xx = xx \ mean(X[pL,], wt ? w[pL] : 1)
            ww = ww \ (wt ? quadsum(w[pL]) : nL)
        }
        if (nH) {
            yy = yy \ yLH[2]
            xx = xx \ mean(X[pH,], wt ? w[pH] : 1)
            ww = ww \ (wt ? quadsum(w[pH]) : nH)
        }
        Q.data(yy, xx, ww)
        Q.b_init(b0)
        // diagnose solution
        if (nL) qL = selectindex((y[pL] - _cdist_Xb(X[pL,], Q.b(), cols(X))):>0)
        else    qL = J(0,1,.)
        if (nH) qH = selectindex((y[pH] - _cdist_Xb(X[pH,], Q.b(), cols(X))):<0)
        else    qH = J(0,1,.)
        mL = length(qL); mH = length(qH)
        // case 1: more than 0.1*M wrong signs; restart with doubled subsample
        if ((mL+mH) > 0.1*M) {
            m = 2 * m
            return(0)
        }
        // case 2: less than 0.1*M wrong sings; edit subsample 
        if (mL) {
            sL[pL[qL]] = J(mL,1,0)
            pL = select(pL, sL[pL])
            nL = nL - mL
        }
        if (mH) {
            sH[pH[qH]] = J(mH,1,0)
            pH = select(pH, sH[pH])
            nH = nH - mH
        }
        // case 2: no wrong signs; done
        if (mL==0 & mH==0) return(1)
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
    if (mm_nunique(G.y)<=S.g) {
        G.at = mm_unique(G.y) // use observed values
        G.Y.fit = G.Y.adj = &G.at
    }
    else {
        G.p = (1::S.g)/S.g:-.5/S.g
        G.at = mm_unique(mm_quantile(G.y, G.w, G.p \ 1, 1/*low quantile*/))
        G.Y.fit = G.Y.adj = &_cdist_dr_at(G.y, G.w, G.at, S.logfit)
    }
    G.g = rows(G.at) - 1 // last element is maximum
    // estimate coefficients
    D.dots = S.dots
    _cdist_progress_init(D, G.g, "group "+g+": fitting models")
    G.b = J(cols(G.X) + 1, G.g, .)
    G.R = J(1, G.g, NULL)
    _cdist_dr_logit(S, G, D, G.at)
    assert(!hasmissing(G.b))
    _cdist_progress_done(D)
    // obtain fit
    _cdist_progress_init(D, 0, "enumerating predictions ...")
    if (todo[1]) {
        G.W.fit = &mm_diff(0 \ _cdist_dr_F(G.X, G.b, G.w, G.R))
        if (S.logfit) G.Y.fit = &exp(*G.Y.fit)
        G.fit = _cdist_stats(*G.Y.fit, *G.W.fit, S.s)
    }
    // obtain adj
    if (todo[2]) {
        if (S.pooled) G.W.adj = &mm_diff(0 \ _cdist_dr_F(S.X, G.b, S.w, G.R))
        else          G.W.adj = &mm_diff(0 \ _cdist_dr_F(G1.X, G.b, G1.w, G.R))
        if (S.logfit) G.Y.adj = &exp(*G.Y.adj)
        G.adj = _cdist_stats(*G.Y.adj, *G.W.adj, S.s)
    }
    _cdist_progress_done(D)
}

real colvector _cdist_dr_at(real colvector y, real colvector w,
    real colvector at0, real scalar logfit)
{   // adjust integration points if grid does not contain all levels of Y
    // (use the mean within each bin)
    real scalar    j, a, b, hasw
    real colvector p, at, yj, wj
    
    if (rows(w)!=1) hasw = 1
    else {;         hasw = 0; wj = w; }
    p =  mm_order(y,1,1) // use stable sort order
    at = at0
    a = rows(y) + 1
    for (j=rows(at)-1;j;j--) {
        a = b = a - 1
        while (y[p[a]]>at[j]) a--
        a++
        yj = y[p[|a\b|]]
        if (hasw) wj = w[p[|a\b|]]
        if (logfit) at[j+1] = ln(mean(exp(yj), wj))
        else        at[j+1] = mean(yj, wj)

    }
    b = a - 1; a = 1
    yj = y[p[|a\b|]]
    if (hasw) wj = w[p[|a\b|]]
    if (logfit) at[j+1] = ln(mean(exp(yj), wj))
    else        at[j+1] = mean(yj, wj)
    return(at)
}

// subroutine to obtain distribution function from predictions

real colvector _cdist_dr_F(real matrix X, real matrix B, real colvector w,
    pointer rowvector R)
{
    real scalar    i, k
    real colvector F
    
    k = cols(X)
    i = cols(B)
    F = J(i+1, 1, 1) // set last element to 1
    for (;i;i--) F[i] = mean(_cdist_dr_F_pr(X, B[,i], k, R[i]), w)
    _sort(F, 1) // rearrange if there are crossings
    return(F)
}

real colvector _cdist_dr_F_pr(real matrix X, real colvector b, real scalar k,
    pointer scalar rules)
{
    real scalar    i
    real colvector pr
    
    pr = invlogit(_cdist_Xb(X, b, k))
    if (rules==NULL) return(pr) // no perfect predictor rules
    i = rows(*rules)
    for (;i;i--) _cdist_dr_F_pr_rule(pr, X, (*rules)[i,])
    return(pr)
}

void _cdist_dr_F_pr_rule(real colvector pr, real matrix X, real rowvector rule)
{   // see source of _pred_rules.ado
    real scalar    xid, rel, rhs, val
    real colvector p
    
    xid = rule[1]
    rel = rule[2]
    rhs = rule[3]
    val = (rule[4]!=0)
    if (rel==1) {
        p = selectindex(X[,xid]:!=rhs)
        if (length(p)) pr[p] = J(length(p), 1, val)
    }
    else if (rel==2) {
        p = selectindex(X[,xid]:>rhs)
        if (length(p)) pr[p] = J(length(p), 1, val)
        p = selectindex(X[,xid]:<rhs)
        if (length(p)) pr[p] = J(length(p), 1, 1-val)
    }
    else if (rel==3) {
        p = selectindex(X[,xid]:<rhs)
        if (length(p)) pr[p] = J(length(p), 1, val)
        p = selectindex(X[,xid]:>rhs)
        if (length(p)) pr[p] = J(length(p), 1, 1-val)
    }
}

// subroutine to fit the logit models in Stata

void _cdist_dr_logit(struct CDIST scalar S, struct CDIST_G scalar G,
    struct CDIST_DOTS scalar D, real colvector y)
{
    real scalar      i, Y
    string scalar    Ynm, cmd
    string rowvector Xnm
    
    Xnm = tokens(S.xvars)
    Ynm = st_tempname()
    Y   = st_addvar("byte", Ynm)
    cmd = "version 10: logit " + // use old logit version; less overhead
          Ynm + " " + S.xvars + " " + 
          (S.wvar!="" ? "[iw=" + S.wvar + "] " : "") + 
          "if " + G.touse
    for (i=1; i<=G.g; i++) {
        st_store(., Y, G.touse, G.y:<=y[i])
        if (_stata(cmd, 1)) stata(cmd+",asis", 1) // try -asis- if logit fails
        else G.R[i] = _cdist_dr_logit_rule(Xnm)   // else collect rules
        G.b[,i] = _cdist_dr_logit_collect_b(Xnm)
        _cdist_progress_dot(D)
    }
    st_dropvar(Ynm)
}

pointer scalar _cdist_dr_logit_rule(string rowvector X)
{
    real scalar      i
    string rowvector x
    real rowvector   codes, p
    real matrix      rules
    
    rules = st_matrix("e(rules)")
    if (rules[1,1]==0) return(NULL)
    x = st_matrixrowstripe("e(rules)")[,2]'
    i = rows(rules)
    rules = J(i, 1, .), rules[|1,1 \ .,3|]
    codes = (1,2,3)
    for (;i;i--) {
        if (!anyof(codes, rules[i,2])) continue
        p = selectindex(X:==x[i])
        if (length(p)!=1) {
            // should never happen
            errprintf("failure to match %s from e(rules)\n", x[i])
            exit(498)
        }
        rules[i,1] = p
    }
    rules = select(rules, rules[,1]:<.)
    if (rows(rules)) return(&rules)
    return(NULL)
}

real colvector _cdist_dr_logit_collect_b(string rowvector X)
{
    real scalar      i, j
    string rowvector x
    real colvector   B, b
    
    b = st_matrix("e(b)")'
    x = st_matrixcolstripe("e(b)")[,2]'
    i = cols(x)
    x = x[|1 \ i-1|] // last element is _cons
    if (x==X) return(b)
    // some vars have been dropped; need to match names
    j = cols(X) + 1
    B = J(j, 1, 0)
    B[j] = b[i] // _cons
    j--; i--
    for (;j;j--) {
        if (!i) break
        if (X[j]==x[i]) {
            B[j] = b[i]
            i--
        }
    }
    if (i) {
        // this should never happen
        errprintf("inconsistent returns from {bf:logit};" +
            " could not match all coefficients\n")
        exit(error(498))
    }
    return(B)
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
        else if (si=="gini") {
            if (s.islog) {
                s.nolog[i] = 1
                b[i] = mm_gini(exp(y), w)
            }
            else b[i] = mm_gini(y, w)
        }
        else if (si=="cv") {
            if (s.islog) {
                s.nolog[i] = 1
                tmp = mean(exp(y), w)
                b[i] = sqrt(mm_variance0(exp(y), w)) / tmp
            }
            else {
                tmp = mean(y, w)
                b[i] = sqrt(mm_variance0(y, w)) / tmp
            }
            if (tmp==0) {
                errprintf("statistic %s not allowed with outcome" +
                    " mean equal to zero\n", si)
                exit(3498)
            }
        }
        else if (si=="mld") {
            if (s.islog) {
                s.nolog[i] = 1
                b[i] = ln(mean(exp(y), w)) - mean(y, w)
            }
            else {
                _cdist_stats_check_ypos(y, "mld")
                b[i] = ln(mean(y, w)) - mean(ln(y), w)
            }
        }
        else if (si=="theil") {
            if (s.islog) {
                s.nolog[i] = 1
                tmp = mean(exp(y), w) 
                b[i] = mean(exp(y):*y, w)/tmp - ln(tmp)
            }
            else {
                _cdist_stats_check_ypos(y, "theil")
                tmp = mean(y, w) 
                b[i] = mean(y:*ln(y), w)/tmp - ln(tmp)
            }
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

