# MIT_SCHEME_NATIVE_CODE(SPEC, HOST_CPU)
# --------------------------------------
AC_DEFUN([MIT_SCHEME_NATIVE_CODE],[
_mit_scheme_native_code_spec=$1
_mit_scheme_native_code_host_cpu=$2

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
svm1)
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
svm|svm1)
    mit_scheme_architecture=svm1
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
*)
    AC_MSG_ERROR([unknown compiler architecture: ${_mit_scheme_architecture_spec}])
    ;;
esac
])
