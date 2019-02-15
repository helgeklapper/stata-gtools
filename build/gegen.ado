*! version 1.1.3 23Jan2019 Mauricio Caceres Bravo, mauricio.caceres.bravo@gmail.com
*! implementation -egen- using C for faster processing

/*
 * syntax:
 *     gegen [type] varname = fun(args) [if] [in], [options]
 *     passed to fun are
 *         [type] varname = fun(args) [if] [in], [options]
 */

/*
 * stata's egen does not parse types correctly.  If the requested result is
 * a sum, stata will happily create a float, despite the risk of overflow.
 * If the source variable is a double, stata will also create a float, even
 * though that might cause a loss in precision. I do not imitate this behavior
 * because I consider it flawed. I upgrade types whenever necessary.
 *
 */

/*
 * TODO: implement label, lname, and truncate for group
 */

capture program drop gegen
program define gegen, byable(onecall) rclass
    version 13.1
local debug0: copy local 0

disp "    debug 1: `:set level' (call `debug0')"
    local 00 `0'
    qui syntax anything(equalok) [if] [in] [aw fw iw pw], [by(str) *]
    local byvars `by'
    local 0 `00'

disp "    debug 2: `:set level' (call `debug0')"
    * Parse weights
    * -------------

    local wgt = cond(`"`weight'"' != "", `"[`weight' `exp']"', "")

    * Parse egen call
    * ---------------

disp "    debug 3: `:set level' (call `debug0')"
    gettoken type 0 : 0, parse(" =(")
    gettoken name 0 : 0, parse(" =(")

disp "    debug 4: `:set level' (call `debug0')"
    if ( `"`name'"' == "=" ) {
        local name   `"`type'"'
        local type   : set type
        local retype = 1
        local btype  double
    }
    else {
        gettoken eqsign 0 : 0, parse(" =(")
        if ( `"`eqsign'"' != "=" ) {
            error 198
        }
        local btype  `type'
        local retype = 0
    }

disp "    debug 5: `:set level' (call `debug0')"
    confirm name `name'
    gettoken fcn  0: 0, parse(" =(")
    gettoken args 0: 0, parse(" ,") match(par)

disp "    debug 6: `:set level' (call `debug0')"
    if ( "`fcn'"   == "total" ) local fcn sum
    if ( "`fcn'"   == "sem"   ) local fcn semean
    if ( "`fcn'"   == "seb"   ) local fcn sebinomial
    if ( "`fcn'"   == "sep"   ) local fcn sepoisson
    if ( "`fcn'"   == "kurt"  ) local fcn kurtosis
    if ( "`fcn'"   == "skew"  ) local fcn skewness
    if ( `"`par'"' != "("     ) exit 198
    if ( "`fcn'"   == "sum"   ) local type `btype'

    * Parse by call
    * -------------

disp "    debug 7: `:set level' (call `debug0')"
    if ( _by() ) local byvars `_byvars'

    * Pre-compiled functions
    * ----------------------

    local funcs tag        ///
                group      ///
                total      ///
                sum        ///
                nansum     ///
                mean       ///
                sd         ///
                max        ///
                min        ///
                count      ///
                median     ///
                iqr        ///
                percent    ///
                first      ///
                last       ///
                firstnm    ///
                lastnm     ///
                semean     ///
                sebinomial ///
                sepoisson  ///
                pctile     ///
                nunique    ///
                nmissing   ///
                skewness   ///
                kurtosis

    * If function does not exist, fall back on egen
    * ---------------------------------------------

disp "    debug 8: `:set level' (call `debug0')"
    if !( `:list fcn in funcs' ) {
disp "    debug 9: `:set level' (call `debug0')"
        confirm new variable `name'

disp "    debug 10: `:set level' (call `debug0')"
        if ( `"`c(adoarchive)'"' == "1" ) {
            capture qui _stfilearchive find _g`fcn'.ado
            if ( _rc ) {
                di as error "`fcn'() is neither a gtools nor an egen function"
                exit 133
            }
disp "    debug 11: `:set level' (call `debug0')"
        }
        else {
disp "    debug 12: `:set level' (call `debug0')"
            capture qui findfile _g`fcn'.ado
            if ( `"`r(fn)'"' == "" ) {
                di as error "`fcn'() is neither a gtools nor an egen function"
                exit 133
            }
disp "    debug 13: `:set level' (call `debug0')"
        }

        if ( `"`weight'"' != "" ) {
disp "    debug 14: `:set level' (call `debug0')"
            di as txt "`fcn'() is not a gtools function; falling back on egen"
            di as err "weights are not allowed for egen-only functions"
disp "    debug 15: `:set level' (call `debug0')"
            exit 101
        }

        if ( `"`args'"' == "_all" ) | ( `"`args'"' == "*" ) {
disp "    debug 16: `:set level' (call `debug0')"
            unab args : _all
        }

disp "    debug 17: `:set level' (call `debug0')"
        local gtools_args HASHmethod(passthru)     ///
                          oncollision(passthru)    ///
                          Verbose                  ///
                          _subtract                ///
                          _CTOLerance(passthru)    ///
                          compress                 ///
                          forcestrl                ///
                          BENCHmark                ///
                          BENCHmarklevel(passthru) ///
                          gtools_capture(str)
disp "    debug 18: `:set level' (call `debug0')"
        syntax [if] [in] [, `gtools_args' *]

disp "    debug 19: `:set level' (call `debug0')"
        if ( "`byvars'" == "" ) {
disp "    debug 20: `:set level' (call `debug0')"
            di as txt "`fcn'() is not a gtools function and no by(); falling back on egen"
            cap noi egen `type' `name' = `fcn'(`args') `if' `in', `options' `gtools_capture'
            exit _rc
        }
        else {
disp "    debug 21: `:set level' (call `debug0')"
            di as txt "`fcn'() is not a gtools function; will hash and use egen"

            local gopts `hashmethod' `oncollision' `verbose' `_subtract' `_ctolerance'
            local gopts `gopts' `compress' `forcestrl' `benchmark' `benchmarklevel'

            local popts _type(`type') _name(`name') _fcn(`fcn') _args(`args') _byvars(`byvars')
            cap noi egen_fallback `if' `in', kwargs(`gopts') `popts' `options' `gtools_capture'
disp "    debug 22: `:set level' (call `debug0')"
            exit _rc
        }
    }

disp "    debug 23: `:set level' (call `debug0')"
    gtools_timer on 97
    global GTOOLS_CALLER gegen

    * Parse syntax call if function is known
    * --------------------------------------

    * gegen [type] varname = fun(args) [if] [in], [options]

disp "    debug 24: `:set level' (call `debug0')"
    syntax                        /// Main call was parsed manually
        [if] [in]                 /// [if condition] [in start / end]
        [aw fw iw pw] ,           /// [weight type = exp]
    [                             ///
        by(str)                   /// Collapse by variabes: [+|-]varname [[+|-]varname ...]
                                  ///
        p(real 50)                /// Percentile to compute, #.# (only with pctile). e.g. 97.5
                                  ///
        missing                   /// for group(), tag(); does not get rid of missing values
        counts(passthru)          /// for group(), tag(); create `counts' with group counts
        fill(str)                 /// for group(), tag(); fills rest of group with `fill'
                                  ///
        replace                   /// Replace target variable with output, if target already exists
                                  ///
        compress                  /// Try to compress strL variables
        forcestrl                 /// Force reading strL variables (stata 14 and above only)
        Verbose                   /// Print info during function execution
        _subtract                 /// (Undocumented) Subtract result from source variable
        _CTOLerance(passthru)     /// (Undocumented) Counting sort tolerance; default is radix
        BENCHmark                 /// print function benchmark info
        BENCHmarklevel(int 0)     /// print plugin benchmark info
        HASHmethod(passthru)      /// Hashing method: 0 (default), 1 (biject), 2 (spooky)
        oncollision(passthru)     /// error|fallback: On collision, use native command or throw error
        gtools_capture(passthru)  /// Ignored (captures fcn options if fcn is not known)
                                  ///
                                  /// Unsupported egen options
                                  /// ------------------------
                                  ///
        Label                     ///
        lname(passthru)           ///
        Truncate(passthru)        ///
   ]

disp "    debug 25: `:set level' (call `debug0')"
    if ( `benchmarklevel' > 0 ) local benchmark benchmark
    local benchmarklevel benchmarklevel(`benchmarklevel')
    local keepmissing = cond("`missing'" == "", "", "keepmissing")

disp "    debug 26: `:set level' (call `debug0')"
    foreach opt in label lname truncate {
        if ( `"``opt''"' != "" ) {
            di as txt ("Option -`opt'- is not implemented."
            exit 198
        }
    }

disp "    debug 27: `:set level' (call `debug0')"
    if ( "`gtools_capture'" != "" ) {
        di as txt ("option -gtools_capture()- ignored with supported function `fcn')"
    }

disp "    debug 28: `:set level' (call `debug0')"
    local bench = ( "`benchmark'" != "" )

    * Parse weights
    * -------------

disp "    debug 29: `:set level' (call `debug0')"
    if ( `:list posof "sd" in fcn' > 0 ) {
        if ( `"`weight'"' == "pweight" ) {
            di as err "sd not allowed with pweights"
            exit 135
        }
    }
    if ( `:list posof "semean" in fcn' > 0 ) {
        if ( inlist(`"`weight'"', "pweight", "iweight") ) {
            di as err "semean not allowed with `weight's"
            exit 135
        }
    }
    if ( `:list posof "sebinomial" in fcn' > 0 ) {
        if ( inlist(`"`weight'"', "aweight", "iweight", "pweight") ) {
            di as err "sebinomial not allowed with `weight's"
            exit 135
        }
    }
    if ( `:list posof "sepoisson" in fcn' > 0 ) {
        if ( inlist(`"`weight'"', "aweight", "iweight", "pweight") ) {
            di as err "sepoisson not allowed with `weight's"
            exit 135
        }
    }

disp "    debug 30: `:set level' (call `debug0')"
	if ( `"`weight'"' != "" ) {
		tempvar w touse
		qui gen double `w' `exp' `if' `in'

		local wgt `"[`weight'=`w']"'
        local weights weights(`weight' `w')
        local anywgt anywgt

        mark `touse' `if' `in' `wgt'
        local ifin if `touse' `in'
	}
    else {
		local wgt
        local weights
        local anywgt
        local ifin `if' `in'
    }

    * Parse quantiles
    * ---------------

disp "    debug 31: `:set level' (call `debug0')"
    local ofcn `fcn'
    if ( "`fcn'" == "pctile" ) {
        local quantbad = !( (`p' < 100) & (`p' > 0) )
        if ( `quantbad' ) {
            di as error "Invalid quantile: `p'; p() should be in (0, 100)"
            cap timer clear 97
            global GTOOLS_CALLER ""
            exit 110
        }
        local fcn p`p'
    }
    else if ( `p' != 50  ) {
        di as err "Option {opt p()} not allowed"
        cap timer clear 97
        global GTOOLS_CALLER ""
        exit 198
    }

disp "    debug 32: `:set level' (call `debug0')"
    * Target and stats
    * ----------------

disp "    debug 33: `:set level' (call `debug0')"
    if ( "`replace'" == "" ) {
disp "    debug 34: `:set level' (call `debug0')"
        confirm new variable `name'
        tempvar dummy
        local rename rename `dummy' `name'
        local addvar qui mata: st_addvar("`type'", "`dummy'")
        local noobs  ""
        local retype = `retype' & 1
disp "    debug 35: `:set level' (call `debug0')"
    }
    else {
disp "    debug 36: `:set level' (call `debug0')"

        * NOTE: Addvar should be "" with replace; the problem was that
        * the internals did not empty the variable before writing to
        * it. With if/in conditions, this caused problems because the
        * variable was not set to missing outside the range, as it
        * should.
        *
        * As a quickfix I thought I could just empty it before calling
        * internals. However, this causesd two issues: The variable
        * would be missing on error, and if the target is also a source,
        * the source would be all misssing when read by the plugin!
        *
        * The easiest fix was to require the target to not be in the
        * sources, but there was an easier fix! I already empty the
        * targets fot gcollapse, so I simply set that boolean to true
        * (init_targ) when gegen was called with replace! This impacts
        * the check in lines 489-492.

disp "    debug 37: `:set level' (call `debug0')"
        cap confirm new variable `name'
        if ( _rc ) {
            local dummy `name'
            local rename ""
            local addvar ""
            local noobs qui replace `dummy' = .
            local retype = `retype' & 0
        }
        else {
            tempvar dummy
            local rename rename `dummy' `name'
            local addvar qui mata: st_addvar("`type'", "`dummy'")
            local noobs  ""
            local retype = `retype' & 1
        }
    }

disp "    debug 38: `:set level' (call `debug0')"
    local targets targets(`dummy')
    local stats   stats(`fcn')

    * If tag or group requested, then do that right away
    * --------------------------------------------------

disp "    debug 39: `:set level' (call `debug0')"
    local opts  `compress' `forcestrl' `_subtract' `_ctolerance'
    local opts  `opts' `verbose' `benchmark' `benchmarklevel'
    local opts  `opts' `oncollision' `hashmethod'
    local sopts `counts'

disp "    debug 40: `:set level' (call `debug0')"
    if ( inlist("`fcn'", "tag", "group") | (("`fcn'" == "count") & ("`args'" == "1")) ) {
disp "    debug 41: `:set level' (call `debug0')"
        if ( "`fill'" != "" ) local fill fill(`fill')

disp "    debug 42: `:set level' (call `debug0')"
        if ( `"`weight'"' != "" ) {
            di as txt "(weights are ignored for egen function {opt `fcn'})"
        }

disp "    debug 43: `:set level' (call `debug0')"
        gtools_timer info 97 `"Plugin setup"', prints(`bench') off

disp "    debug 44: `:set level' (call `debug0')"
        if ( "`fcn'" == "tag" ) {
            local action tag(`type' `dummy') gfunction(hash) unsorted
            local noobs qui replace `dummy' = 0
        }

disp "    debug 45: `:set level' (call `debug0')"
        if ( inlist("`fcn'", "group", "count") ) {
            if ( `=_N' < maxbyte() ) {
                * All types are OK
            }
            else if ( `=_N' < `=2^24' ) {
                if inlist("`type'", "byte") {
                    * byte is no longer OK; int, float still OK
                    local upgraded = cond(`retype', "", "`type'")
                    local type int
                }
            }
            else if ( `=_N' < maxint() ) {
                if inlist("`type'", "byte", "float") {
                    * byte and float no longer OK; int still OK
                    local upgraded = cond(`retype', "", "`type'")
                    local type int
                }
            }
            else if ( `=_N' < maxlong() ) {
                if inlist("`type'", "byte", "int", "float") {
                    * byte, float, int no longer OK; must upgrade to long
                    local upgraded = cond(`retype', "", "`type'")
                    local type long
                }
            }
            else {
                if ( "`type'" != "double" ) {
                    * Only double can maintain precision
                    local upgraded = cond(`retype', "", "`type'")
                    local type double
                }
            }
        }

disp "    debug 46: `:set level' (call `debug0')"
        if ( "`upgraded'" != "" ) {
            disp "(warning: user-requested type '`upgraded'' upgraded to '`type'')"
        }

disp "    debug 47: `:set level' (call `debug0')"
        if ( "`fcn'" == "group" ) {
disp "    debug 49: `:set level' (call `debug0')"
            local action gen(`type' `dummy') gfunction(hash) countmiss
            if ( `=_N' > 1 ) local s s
            local noobs qui replace `dummy' = .
            local notxt di as txt "(`=_N' missing value`s' generated)"
disp "    debug 50: `:set level' (call `debug0')"
        }

disp "    debug 48: `:set level' (call `debug0')"
        if ( "`fcn'" == "count" ) {
disp "    debug 51: `:set level' (call `debug0')"
            local missing missing
            local fill fill(group)
            local action counts(`type' `dummy') gfunction(hash) countmiss unsorted
            if ( `=_N' > 1 ) local s s
            local noobs qui replace `dummy' = .
            local notxt di as txt "(`=_N' missing value`s' generated)"
disp "    debug 52: `:set level' (call `debug0')"
        }

disp "    debug 53: `:set level' (call `debug0')"
        if ( ("`byvars'" != "") & inlist("`fcn'", "tag", "group") ) {
disp "    debug 54: `:set level' (call `debug0')"
            di as err "egen ... `fcn'() may not be combined with with by"
            global GTOOLS_CALLER ""
disp "    debug 55: `:set level' (call `debug0')"
            exit 190
        }

disp "    debug 56: `:set level' (call `debug0')"
        if ( ("`byvars'" == "") & inlist("`fcn'", "tag", "group") ) {
disp "    debug 57: `:set level' (call `debug0')"
            local byvars `args'
        }

disp "    debug 58: `:set level' (call `debug0')"
        cap noi _gtools_internal `byvars' `ifin', `opts' `sopts' `action' `missing' `replace' `fill'
        local rc = _rc
        global GTOOLS_CALLER ""

disp "    debug 59: `:set level' (call `debug0')"
        if ( `rc' == 17999 ) {
            local gtools_args `hashmethod'     ///
                              `oncollision'    ///
                              `verbose'        ///
                              `_subtract'      ///
                              `_ctolerance'    ///
                              `compress'       ///
                              `forcestrl'      ///
                              `benchmark'      ///
                              `benchmarklevel' ///
                              `gtools_capture'
            local gtools_opts `counts' fill(`fill') `replace' p(`p') `missing'
            collision_fallback, gtools_call(`"`type' `name' = `fcn'(`args') `ifin'"') `gtools_args' `gtools_opts'
            exit 0
        }
        else if ( `rc' == 17001 ) {
            if ( "${GTOOLS_DUPS}" == "" ) {
                if ( `=_N' > 0 ) {
                    `noobs'
                    `notxt'
                }
                `rename'
                exit 0
            }
            else {
                error 2000
            }
        }
        else if ( `rc' ) {
            exit `rc'
        }

disp "    debug 60: `:set level' (call `debug0')"
        return scalar N    = `r(N)'
        return scalar J    = `r(J)'
        return scalar minJ = `r(minJ)'
        return scalar maxJ = `r(maxJ)'

        `rename'
disp "    debug 61: `:set level' (call `debug0')"
        exit 0
    }

    * Parse source(s)
    * ---------------

disp "    debug 62: `:set level' (call `debug0')"
    unab memvars: _all

disp "    debug 63: `:set level' (call `debug0')"
    local rc = 0
    if ( !((`:list sizeof args' == 1) & (`:list args in memvars')) ) {
disp "    debug 64: `:set level' (call `debug0')"
        tempvar exp
        cap gen double `exp' = `args'
        local rc = _rc
disp "    debug 65: `:set level' (call `debug0')"
    }

disp "    debug 66: `:set level' (call `debug0')"
    if ( ((`:list sizeof args' == 1) & (`:list args in memvars')) | `rc' ) {
disp "    debug 67: `:set level' (call `debug0')"
        cap ds `args'
        if ( _rc ) {
            global GTOOLS_CALLER ""
            di as error "Invalid call; please specify {opth `ofcn'(varlist)} or {opth `ofcn'(exp)}."
            exit 198
        }
        else {
disp "    debug 68: `:set level' (call `debug0')"
            local sametype 1
            local sources `r(varlist)'
            cap confirm numeric v `sources'
            if ( _rc ) {
                global GTOOLS_CALLER ""
                di as err "{opth `ofcn'(varlist)} must call a numeric variable list."
                exit _rc
            }

disp "    debug 69: `:set level' (call `debug0')"
            * See notes in lines 294-310
            * if ( "`:list sources & dummy'" != "" ) { 
            *     if ( "`replace'" != "" ) local extra " even with -replace-"
            *     di as error "Variable `dummy' canot be a source and a target`extra'"
            *     exit 198
            * }
        }
    }
    else if ( `rc' == 0 ) {
        local sources `exp'
        local sametype 0
    }

disp "    debug 70: `:set level' (call `debug0')"
    * cap ds `args'
    * if ( _rc == 0 ) {
    *     local sametype 1
    *     local sources `r(varlist)'
    *     cap confirm numeric v `sources'
    *     if ( _rc ) {
    *         global GTOOLS_CALLER ""
    *         di as err "{opth `ofcn'(varlist)} must call a numeric variable list."
    *         exit _rc
    *     }
    * }
    * else {
    *     local sametype 0
    *     tempvar exp
    *     cap gen double `exp' = `args'
    *     if ( _rc ) {
    *         global GTOOLS_CALLER ""
    *         di as error "Invalid call; please specify {opth `ofcn'(varlist)} or {opth `ofcn'(exp)}."
    *
    *         exit 198
    *     }
    *     local sources `exp'
    * }

    * Parse target type
    * -----------------

    * if ( ("`addvar'" != "") & `retype' ) {
    if ( `retype' ) {
disp "    debug 71: `:set level' (call `debug0')"
        parse_target_type `sources', fcn(`ofcn') sametype(`sametype') `anywgt'
        local type = "`r(retype)'"
        local addvar qui mata: st_addvar("`type'", "`dummy'")
    }

disp "    debug 72: `:set level' (call `debug0')"

    * Parse counts into freq for gfunction call
    * -----------------------------------------

    if ( "`counts'" != "" ) {
disp "    debug 73: `:set level' (call `debug0')"
        local 0, `counts'
        syntax, [counts(str)]

disp "    debug 74: `:set level' (call `debug0')"
        gettoken ftype fname: counts
        if ( "`fname'" == "" ) {
            local fname `ftype'
            if ( `=_N < maxlong()' ) local ftype long
            else local ftype double
        }

disp "    debug 75: `:set level' (call `debug0')"
        cap confirm new variable `fname'
        if ( _rc ) {
disp "    debug 75: `:set level' (call `debug0')"
            local rc = _rc
            if ( "`replace'" == "" ) {
disp "    debug 76: `:set level' (call `debug0')"
                global GTOOLS_CALLER ""
                di as err "Variable `fname' exists; try a different name or run with -replace-"
                exit `rc'
            }
            else if ( ("`replace'" != "") & ("`addvar'" != "") ) {
disp "    debug 77: `:set level' (call `debug0')"
                qui replace `fname' = .
                local replace ""
            }
        }
        else {
            if ( "`addvar'" == "" ) {
disp "    debug 78: `:set level' (call `debug0')"
                local addvar qui mata: st_addvar("`ftype'", "`counts'")
            }
            else {
disp "    debug 79: `:set level' (call `debug0')"
                local addvar qui mata: st_addvar(("`type'", "`ftype'"), ("`name'", "`counts'"))
                local replace ""
            }
        }

disp "    debug 80: `:set level' (call `debug0')"
        local counts freq(`counts')
    }

    * Call the plugin
    * ---------------

disp "    debug 81: `:set level' (call `debug0')"
    local unsorted = cond("`fill'" == "data", "", "unsorted")
    gtools_timer info 97 `"Plugin setup"', prints(`bench') off

disp "    debug 81: `:set level' (call `debug0')"
    `addvar'
disp "    debug 82: `:set level' (call `debug0')"
    local action sources(`sources') `targets' `stats' fill(`fill') `counts' countmiss
disp "    debug 83: `:set level' (call `debug0')"
    cap noi _gtools_internal `byvars' `ifin', `unsorted' `opts' `action' `weights' missing `keepmissing' `replace'
    local rc = _rc
    global GTOOLS_CALLER ""
disp "    debug 84: `:set level' (call `debug0')"

    if ( `rc' == 17999 ) {
disp "    debug 85: `:set level' (call `debug0')"
        if ( `"`weight'"' != "" ) {
            di as err "Cannot use fallback with weights."
            exit 17000
        }
        local gtools_args `hashmethod'     ///
                          `oncollision'    ///
                          `verbose'        ///
                          `_subtract'      ///
                          `_ctolerance'    ///
                          `compress'       ///
                          `forcestrl'      ///
                          `benchmark'      ///
                          `benchmarklevel' ///
                          `gtools_capture'
        local gtools_opts `counts' fill(`fill') `replace' p(`p') `missing'
        collision_fallback, gtools_call(`"`type' `name' = `fcn'(`args') `ifin'"') `gtools_args' `gtools_opts'
        exit 0
    }
    else if ( `rc' == 17001 ) {
        if ( "${GTOOLS_DUPS}" == "" ) {
            `noobs'
            `rename'
            exit 0
        }
        else {
            error 2000
        }
    }
    else if ( `rc' ) exit `rc'

disp "    debug 86: `:set level' (call `debug0')"
    return scalar N      = `r(N)'
    return scalar J      = `r(J)'
    return scalar minJ   = `r(minJ)'
    return scalar maxJ   = `r(maxJ)'

    `rename'
disp "    debug 87: `:set level' (call `debug0')"
    exit 0
end

capture program drop egen_fallback
program egen_fallback, sortpreserve
    syntax [if] [in],          ///
    [                          ///
        _type(str)             ///
        _name(str)             ///
        _fcn(str)              ///
        _args(str)             ///
        _byvars(str)           ///
        by(passthru)           ///
        kwargs(str)            ///
        *                      ///
    ]

    tempvar dummy
    global EGEN_Varname  `_name'
    global EGEN_SVarname `_sortindex'

	local cvers = _caller()
    if ( "`_fcn'" == "mode" | "`_fcn'" == "concat" ) {
        local vv : display "version " string(`cvers') ", missing:"
    }

    if ( "`: sortedby'" == "`_byvars'" ) {
        local byid `: sortedby'
    }
    else {
        tempvar byid
        hashsort `_byvars', gen(`byid') sortgen skipcheck `kwargs'
    }

    capture noisily `vv' _g`_fcn' `_type' `dummy' = (`_args') `if' `in', by(`byid') `options'
    global EGEN_SVarname
    global EGEN_Varname
    if ( _rc ) exit _rc

    quietly count if missing(`dummy')
    if ( `r(N)' ) {
        local s = cond(r(N) > 1, "s", "")
        di in bl "(" r(N) " missing value`s' generated)"
    }
    rename `dummy' `_name'
    exit 0
end

capture program drop gtools_timer
program gtools_timer, rclass
    syntax anything, [prints(int 0) end off]
    tokenize `"`anything'"'
    local what  `1'
    local timer `2'
    local msg   `"`3'; "'

    if ( inlist("`what'", "start", "on") ) {
        cap timer off `timer'
        cap timer clear `timer'
        timer on `timer'
    }
    else if ( inlist("`what'", "info") ) {
        timer off `timer'
        qui timer list
        return scalar t`timer' = `r(t`timer')'
        return local pretty`timer' = trim("`:di %21.4gc r(t`timer')'")
        if ( `prints' ) {
            di `"`msg'`:di trim("`:di %21.4gc r(t`timer')'")' seconds"'
        }
        timer off `timer'
        timer clear `timer'
        timer on `timer'
    }

    if ( "`end'`off'" != "" ) {
        timer off `timer'
        timer clear `timer'
    }
end

capture program drop parse_target_type
program parse_target_type, rclass
    syntax varlist, fcn(str) sametype(int) [anywgt]

    gettoken var restvars: varlist

    local maxtype: type `var'
    encode_vartype `maxtype'
    local maxcode `r(typecode)'

    foreach var in `restvars' {
        local stype: type `var'
        encode_vartype `stype'
        local scode `r(typecode)'
        if ( `scode' > `maxcode' ) {
            local maxtype `stype'
            local maxcode `scode'
        }
    }

    if ( `sametype' ) local retype_A `maxtype'
    else local retype_A: set type

    if ( "`maxtype'" == "double" ) local retype_B double
    else local retype_B: set type

    if ( `=_N < maxlong()' & ("`anywgt'" == "") ) local retype_C long
    else local retype_C double

    if ( "`fcn'" == "tag"        ) return local retype = "byte"
    if ( "`fcn'" == "group"      ) return local retype = "`retype_C'"
    if ( "`fcn'" == "total"      ) return local retype = "double"
    if ( "`fcn'" == "sum"        ) return local retype = "double"
    if ( "`fcn'" == "nansum"     ) return local retype = "double"
    if ( "`fcn'" == "mean"       ) return local retype = "`retype_B'"
    if ( "`fcn'" == "sd"         ) return local retype = "`retype_B'"
    if ( "`fcn'" == "max"        ) return local retype = "`retype_A'"
    if ( "`fcn'" == "min"        ) return local retype = "`retype_A'"
    if ( "`fcn'" == "count"      ) return local retype = "`retype_C'"
    if ( "`fcn'" == "median"     ) return local retype = "`retype_B'"
    if ( "`fcn'" == "iqr"        ) return local retype = "`retype_B'"
    if ( "`fcn'" == "percent"    ) return local retype = "`retype_B'"
    if ( "`fcn'" == "first"      ) return local retype = "`retype_A'"
    if ( "`fcn'" == "last"       ) return local retype = "`retype_A'"
    if ( "`fcn'" == "firstnm"    ) return local retype = "`retype_A'"
    if ( "`fcn'" == "lastnm"     ) return local retype = "`retype_A'"
    if ( "`fcn'" == "semean"     ) return local retype = "`retype_B'"
    if ( "`fcn'" == "sebinomial" ) return local retype = "`retype_B'"
    if ( "`fcn'" == "sepoisson"  ) return local retype = "`retype_B'"
    if ( "`fcn'" == "pctile"     ) return local retype = "`retype_B'"
    if ( "`fcn'" == "nunique"    ) return local retype = "`retype_C'"
    if ( "`fcn'" == "nmissing"   ) return local retype = "`retype_C'"
    if ( "`fcn'" == "skewness"   ) return local retype = "`retype_B'"
    if ( "`fcn'" == "kurtosis"   ) return local retype = "`retype_B'"
end

capture program drop encode_vartype
program encode_vartype, rclass
    args vtype
         if ( "`vtype'" == "byte"   ) return scalar typecode = 1
    else if ( "`vtype'" == "int"    ) return scalar typecode = 2
    else if ( "`vtype'" == "long"   ) return scalar typecode = 3
    else if ( "`vtype'" == "float"  ) return scalar typecode = 4
    else if ( "`vtype'" == "double" ) return scalar typecode = 5
    else                              return scalar typecode = 0
end

capture program drop collision_fallback
program collision_fallback
    local gtools_args HASHmethod(passthru)     ///
                      oncollision(passthru)    ///
                      Verbose                  ///
                      _subtract                ///
                      _CTOLerance(passthru)    ///
                      compress                 ///
                      forcestrl                ///
                      BENCHmark                ///
                      BENCHmarklevel(passthru) ///
                      gtools_capture(str)

    syntax, [`gtools_args' gtools_call(str) counts(str) fill(str) replace *]
    foreach opt in counts fill replace {
        if ( `"``opt''"' != "" ) {
            di as err "Cannot use fallback with option {opt `opt'}."
            exit 17000
        }
    }
    egen `gtools_call', `options'
end
