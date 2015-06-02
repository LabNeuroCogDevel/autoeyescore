# source me

# exit with error message
# if 2 arguments, only exit if first (a file) is not readable
function exiterr {
 [ -z "$2" -o ! -r "$1" ] && echo "$@" >&2 && exit 1
 return 0
}
# make it obvious that we are exiting with an error b/c of set -e
trap '[ "$?" -ne 0 ] && echo "$0 ended with error!"' EXIT
