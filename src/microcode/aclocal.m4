# MIT_SCHEME_NATIVE_CODE(SPEC, HOST_CPU)
# ----------------------
AC_DEFUN([MIT_SCHEME_NATIVE_CODE],[
_mit_scheme_native_code_spec=$1
_mit_scheme_native_code_host_cpu=$2
case ${_mit_scheme_native_code_host_cpu} in
i?86)
    AC_CHECK_DECL([__x86_64__],[_mit_scheme_native_code_host_cpu=x86_64])
    ;;
esac
AC_MSG_CHECKING([for native-code support])
case ${_mit_scheme_native_code_spec} in
yes|YES|y|Y)
    case ${host_cpu} in
    alpha*)
	mit_scheme_native_code=alpha
	;;
    hppa*)
	mit_scheme_native_code=hppa
	;;
    i?86)
	mit_scheme_native_code=i386
	;;
    m68k|m680?0)
	mit_scheme_native_code=mc68k
	;;
    mips*)
	mit_scheme_native_code=mips
	;;
    vax)
	mit_scheme_native_code=vax
	;;
    x86_64)
	mit_scheme_native_code=x86-64
	;;
    *)
	AC_MSG_ERROR([unable to determine native-code type])
	;;
    esac
    ;;
c|C)
    mit_scheme_native_code=c
    ;;
svm|svm1)
    mit_scheme_native_code=svm1
    ;;
no|NO|none|NONE|n|N)
    mit_scheme_native_code=none
    ;;
i?86|x86)
    mit_scheme_native_code=i386
    ;;
x86-64|x86_64|amd64)
    mit_scheme_native_code=x86-64
    ;;
*)
    AC_MSG_ERROR([unknown native-code type: ${_mit_scheme_native_code_spec}])
    ;;
esac

case ${mit_scheme_native_code} in
none)
    AC_MSG_RESULT([no])
    ;;
c)
    AC_MSG_RESULT([yes, using portable C code])
    ;;
svm1)
    AC_MSG_RESULT([yes, using portable SVM code])
    ;;
*)
    AC_MSG_RESULT([yes, for ${mit_scheme_native_code}])
    ;;
esac
])
