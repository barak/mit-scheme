# MIT_SCHEME_NATIVE_CODE(SPEC, HOST_CPU)
# --------------------------------------
AC_DEFUN([MIT_SCHEME_NATIVE_CODE],[
_mit_scheme_native_code_spec=$1
_mit_scheme_native_code_host_cpu=$2

AC_CHECK_SIZEOF([unsigned long])
AC_C_BIGENDIAN(
    [mit_scheme_host_byteorder=be],
    [mit_scheme_host_byteorder=le],
    [AC_MSG_ERROR([unknown host byte order])])

AC_MSG_CHECKING([for native-code support])
MIT_SCHEME_ARCHITECTURE([${_mit_scheme_native_code_spec}])

case ${mit_scheme_architecture} in
yes)
    case ${_mit_scheme_native_code_host_cpu} in
    i?86)
	AC_CHECK_DECL([__x86_64__],
		      [_mit_scheme_native_code_host_cpu=x86_64],
		      [_mit_scheme_native_code_host_cpu=i386])
	;;
    esac
    ;;
esac

case ${mit_scheme_architecture} in
yes)
    case ${_mit_scheme_native_code_host_cpu} in
    i386)
	mit_scheme_native_code=i386
	;;
    x86_64)
	mit_scheme_native_code=x86-64
	;;
    *)
	AC_MSG_ERROR([unable to determine host architecture])
	;;
    esac
    ;;
*)
    mit_scheme_native_code=${mit_scheme_architecture}
    ;;
esac

case ${mit_scheme_native_code} in
none)
    AC_MSG_RESULT([no])
    ;;
c)
    AC_MSG_RESULT([yes, using portable C code])
    ;;
svm1|svm1-32|svm1-64|svm1-be|svm1-le|svm1-32be|svm1-32le|svm1-64be|svm1-64le)
    case ${mit_scheme_native_code} in
    svm1|svm1-be|svm1-le)
	case ${ac_cv_sizeof_unsigned_long} in
	4)
	    mit_scheme_svm_wordsize=32
	    ;;
	8)
	    mit_scheme_svm_wordsize=64
	    ;;
	*)
	    AC_MSG_ERROR([Unknown host word size])
	    ;;
	esac
	;;
    svm1-32le|svm1-32be)
	mit_scheme_svm_wordsize=32
	;;
    svm1-64le|svm1-64be)
	mit_scheme_svm_wordsize=64
	;;
    esac
    case ${mit_scheme_native_code} in
    svm1|svm1-32|svm1-64)
	mit_scheme_svm_byteorder="${mit_scheme_host_byteorder}"
	;;
    svm1-32be|svm1-64be)
	mit_scheme_svm_byteorder=be
	;;
    svm1-32le|svm1-64le)
	mit_scheme_svm_byteorder=le
	;;
    esac
    mit_scheme_native_code=svm1-${mit_scheme_svm_wordsize}${mit_scheme_svm_byteorder}
    AC_MSG_RESULT([yes, using portable SVM code])
    ;;
*)
    AC_MSG_RESULT([yes, for ${mit_scheme_native_code}])
    ;;
esac
])

# MIT_SCHEME_COMPILER_TARGET(SPEC)
# --------------------------------
AC_DEFUN([MIT_SCHEME_COMPILER_TARGET],[
_mit_scheme_compiler_target_spec=$1

AC_MSG_CHECKING([for compiler target])
MIT_SCHEME_ARCHITECTURE([${with_compiler_target}])

case ${mit_scheme_architecture} in
yes)
    mit_scheme_compiler_target=${mit_scheme_native_code}
    ;;
*)
    mit_scheme_compiler_target=${mit_scheme_architecture}
    ;;
esac

case ${mit_scheme_compiler_target} in
none)
    AC_MSG_RESULT([none])
    ;;
c)
    AC_MSG_RESULT([yes, using portable C code])
    ;;
svm1)
    AC_MSG_RESULT([yes, using portable SVM code])
    ;;
*)
    AC_MSG_RESULT([yes, for ${mit_scheme_compiler_target}])
    ;;
esac
])

# MIT_SCHEME_ARCHITECTURE(SPEC)
# -----------------------------
AC_DEFUN([MIT_SCHEME_ARCHITECTURE],[
_mit_scheme_architecture_spec=$1

case ${_mit_scheme_architecture_spec} in
yes|YES|y|Y)
    mit_scheme_architecture=yes
    ;;
c|C)
    mit_scheme_architecture=c
    ;;
svm1-32be|svm1-32le|svm1-64be|svm1-64le)
    mit_scheme_architecture=${_mit_scheme_architecture_spec}
    ;;
no|NO|none|NONE|n|N)
    mit_scheme_architecture=none
    ;;
i?86|x86)
    mit_scheme_architecture=i386
    ;;
x86-64|x86_64|amd64)
    mit_scheme_architecture=x86-64
    ;;
aarch64le|aarch64be)
    mit_scheme_architecture=aarch64
    ;;
*)
    AC_MSG_ERROR([unknown compiler architecture: ${_mit_scheme_architecture_spec}])
    ;;
esac
])
