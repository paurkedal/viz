#! /bin/sh

die()
{
    echo 1>&2 "$*"
    exit 69
}

[ x"$VIZ_SRCDIR" != x ] || die "Please run this script via a link to vizdev"

show_command=false
if [ x"$1" = x"-v" ]; then
    show_command=true
    shift
fi
case "$1" in
    ocamlc)
	libext=.cma
	;;
    ocamlopt)
	libext=.cmxa
	;;
    ocamldep)
	;;
    *)
	cat <<EOF

Usage:	camlviz --help
	camlviz [-v] (ocamlc|ocamlopt|ocamldep) ARGUMENTS...

The arguments are passed to ocamlfind and the corresponding subcommand.  For
compiling source files, the "-c" option is required, so you can't compile and
link with the same command line.

Examples:
	camlviz ocamlopt -c test.viz -package foo
	camlviz ocamlopt -o test.native test.cmx -package foo -linkpkg

EOF
	[ "$1" = "--help" ] && exit 0 || exit 64
	;;
esac
command="$1"
shift

grab_arg=false
pp_opts=
oc_args=
have_c_flag=false
seen_source=false
use_locations=false
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
	*.vz|*.viz|*.ff)
	    seen_source=true
	    oc_args="$oc_args -impl $arg"
	    ;;
	-c)
	    have_c_flag=true
	    oc_args="$oc_args $arg"
	    ;;
	--add-locations)
	    pp_opts="$pp_opts --add-locations"
	    use_loctations=true
	    ;;
	*)
	    oc_args="$oc_args $arg"
	    ;;
    esac
done

builddir=$VIZ_SRCDIR/_build
extra_includes="-I $builddir/vsl -I $builddir"
extra_packages="-package camomile"
case "$command" in
    ocamlopt|ocamlc)
	if [ $have_c_flag = true ]; then
	    oc_args="-nopervasives $oc_args"
	elif [ $seen_source = true ]; then
	    die "The -c flag is required for compiling Viz sources with" \
		"ocamlopt."
	fi
	oc_args="$builddir/vsl$libext $oc_args"
	;;
    ocamldep)
	extra_packages=
	;;
esac
[ x$show_command != xtrue ] || set -x
exec ocamlfind "$command" \
	-pp "$VIZ_SRCDIR/bin/camlvizpp $extra_includes $pp_opts" \
	$extra_includes $extra_packages $oc_args
