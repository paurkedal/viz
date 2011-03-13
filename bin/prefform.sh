#! /bin/sh

die()
{
    echo 1>&2 "$*"
    exit 69
}

[ x"$FFORM_SRCDIR" != x ] || die "Please run this script via a link to ffdev"

show_command=false
if [ x"$1" = x"-v" ]; then
    show_command=true
    shift
fi
case "$1" in
    ocamlc)
	libext=cma
	;;
    ocamlopt)
	libext=cmxa
	;;
    ocamldep)
	;;
    *)
	echo "Usage: prefform (ocamlc|ocamlopt|ocamldep) ARGUMENTS..."
	exit 64
	;;
esac
command="$1"
shift

grab_arg=false
pp_opts=
oc_args=
have_c_flag=false
seen_source=false
for arg in "$@"; do
    if [ x"$grab_arg" = true ]; then
	pp_opts="$pp_opts $arg"
	oc_args="$oc_args $arg"
	grab_arg=false
	continue
    fi
    case "$arg" in
	-I)
	    grab_arg=true
	    pp_opts="$pp_opts -I"
	    oc_args="$oc_args -I"
	    ;;
	*.ff)
	    seen_source=true
	    oc_args="$oc_args -impl $arg"
	    ;;
	-c)
	    have_c_flag=true
	    oc_args="$oc_args $arg"
	    ;;
	*)
	    oc_args="$oc_args $arg"
	    ;;
    esac
done

builddir=$FFORM_SRCDIR/_build
extra_includes="-I $builddir/fflib -I $builddir"
case "$command" in
    ocamlc)
	#oc_args="$oc_args -dllpath $builddir $builddir/fflib.cma"
	oc_args="-nopervasives stdlib.cma $builddir/fflib.cma $oc_args"
	;;
    ocamlopt)
	if [ $have_c_flag = true ]; then
	    oc_args="-nopervasives $oc_args"
	elif [ $seen_source = true ]; then
	    die "The -c flag is required for compiling Fform sources with" \
		"ocamlopt."
	fi
	oc_args="$builddir/fflib.cmxa $oc_args"
	;;
esac
[ x$show_command != xtrue ] || set -x
exec "$command" \
	-pp "$FFORM_SRCDIR/bin/ffoc1pp $extra_includes $pp_opts" \
	$extra_includes $oc_args
