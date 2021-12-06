#!/bin/bash --norc

#
## script moves duplicates from /var/cache/pacman/pkg to /home/backup/pkg-1, and
## then moves duplicates from /home/backup/pkg-1 to /home/backup/pkg-2 and so on.
## To change the directories (add/remove) entries from DIRLIST below.
#

usage() {
  [[ -n $1 ]] && echo -e "\n  ERROR: ${1}\n"
  echo -e "\n  Usage: ${0##/*} <parameter>\n"
  echo -e "    This wrapper script calls /usr/local/bin/fduppkg to search"
  echo -e "    /var/cache/pacman/pkg and move any dupicates as follows:\n"
  echo -e "      /var/cache/pacman/pkg -> /home/backup/pkg-1"
  echo -e "      /home/backup/pkg-1    -> /home/backup/pkg-2"
  echo -e "      /home/backup/pkg-2    -> /home/backup/pkg-3"
  echo -e "      /home/backup/pkg-3    -> /home/backup/pkg-del"
  echo -e "    options:"
  echo -e "      -f | --force     Ignore dir md5sum compare and force duplicate scan"
  echo -e "      -h | --help      Prints this help message and exits\n"
  echo -e "    To change the directories, edit the DIRLIST variable in this script."
  echo -e "\n  2nd script Usage: fduppkg <search dir> [ -d <dup_dir> (-l logfile) -q -s -v ]\n"
  echo -e "    Searches <search dir> for duplicate pkgs and moves duplicate files to [dup_dir]"
  echo -e "    or <search dir>/duplicates by default.\n"
  echo -e "      -d | --dupdir    Used to specify directory to hold duplicate rpms"
  echo -e "      -h | --help      Prints this help message and exits"
  echo -e "      -l | --logfile   Specify the log file name (default ./duplicates.log)"
  echo -e "      -q | --quiet     Suppress the individual package listing but log results"
  echo -e "      -s | --silent    Don't output anything to stdout, just log results"
  echo -e "      -v | --verbose   Output information showing duplicates found and moved"
  echo -e ""

  exit 1
}

## test for help
[[ "$@" =~ -h ]] && usage

## Check to make sure the script is run as root
ROOT_UID=0
E_NOTROOT=67

if [[ $UID -ne $ROOT_UID ]]; then
    if [[ $USER == david ]]; then
	pfx=sudo
    else
	echo -e "\nYou must be root to run this script.\nUser: $USER, UID: $UID can't! See below...\n"
	usage
	exit $E_NOTROOT
    fi
fi

## Define a few colors
ltblu='\e[1;34m'     # ${ltblu}
ltgrn='\e[1;32m'     # ${ltgrn}
nc='\e[0m'           # ${nc} (no color - disables previous color selection)

## script variables
FDUPPKG=/usr/local/bin/fduppkg
DATADIR=/home/backup/.data
LOGDIR=/home/backup/log
LOGBZ2=pkgdups.log.bz2
LOG=pkgdups.log
SUMFILE=${DATADIR}/pkgdir-md5.txt
TMPDIR=/tmp
TMPFILE=${TMPDIR}/tmppkgmd5.txt
CURDIR=$PWD
CNT=0
FORCESCAN=0             # flag to force scan even if no change in dir md5sums
declare -a DSIZE
declare -a DIRLIST
DIRLIST=( /var/cache/pacman/pkg /home/backup/pkg-1 /home/backup/pkg-2 /home/backup/pkg-del )
NUMDIRS=${#DIRLIST[@]}
PDIRS=$((NUMDIRS-1))
EGID=$(id -gn)          # get current user's default group for dir ownership

#check for forcescan
[[ "$@" =~ -f ]] && FORCESCAN=1

# check or create datadir
[[ -d $DATADIR ]] || $pfx mkdir -p $DATADIR
[[ -n $pfx ]] && $pfx chown $USER:$EGID $DATADIR
[[ -d $DATADIR ]] || echo -n "WARNING: unable to create $DATADIR - directory checksums cannot be saved."

## pkgdir md5sum functions
# create package directory md5sums and write to $SUMFILE
crpkgdirmd5() {
  [[ -w ${DATADIR} ]] || {
    echo "WARNING: checksum creation skipped due to unwritable '${DATADIR}'"
    return 1
  }
  [[ -w ${SUMFILE} ]] || $pfx chown $UID:$EGID ${SUMFILE}
  :>${SUMFILE}
  for((i=0;i<${#DIRLIST[@]};i++)); do
    sumline=$(ls -1 ${DIRLIST[${i}]} | md5sum)
    echo "${DIRLIST[${i}]} ${sumline%% *}" >> ${SUMFILE}
  done
}

# get the directory checksum for the directory passed as an argument to the function
getmd5sum() {
  [[ -z $1 ]] && {
    echo "ERROR: insufficient arguments passed to getmd5sum()" >&2
    return 2
  }
  if [[ -r ${SUMFILE} ]]; then
    sumline=$(grep ${1} ${SUMFILE})
    tmp=${sumline#* }
    tmp=${tmp% *}
    echo $tmp
    return 0
  else
    return 2
  fi
}

# update the directory checksum for the directory given as an argument (currently not used)
updtmd5sum() {
  [[ -z $1 ]] && {
    echo "ERROR: insufficient arguments passed to getmd5sum()" >&2
    return 2
  }
  if [[ -r ${SUMFILE} ]]; then
    # create tmp file w/o md5 of dir to update
    :>${TMPFILE}
    grep -v ${1} ${SUMFILE} > ${TMPFILE}
    sumline=$(ls -1 ${1} | md5sum)
    echo "${1}/ ${sumline%% *}" >> ${TMPFILE}
    [[ -r ${TMPFILE} ]] && cp ${TMPFILE} ${SUMFILE}
    [[ -r ${TMPFILE} ]] && rm -f ${TMPFILE}
    return 0
  else
    crpkgdirmd5
    return 2
  fi
}

# check the directory checksum for the directory given as an agrument
chkpkgmd5sum() {
  [[ -z $1 ]] && {
    echo "ERROR: insufficient arguments passed to chkpkgmd5sum()" >&2
    return 2
  }
  if [[ -r ${SUMFILE} ]]; then
    newsum=$(ls -1 ${1} | md5sum)
    newsum=${newsum%% *}
    oldsum=$(getmd5sum ${1})
    [[ $newsum == $oldsum ]] && return 1 || return 2
  else
    return 2
  fi
}

## check logdir and unip the log file
[[ -d $LOGDIR ]] || $pfx mkdir -p $LOGDIR
[[ -d $LOGDIR ]] || usage "Unable to create $LOGDIR"
  cd $LOGDIR
[[ -r $LOGBZ2 ]] && $pfx bzip2 -d -f $LOGBZ2

# check for old dir name and move if found (cleans up old directory names)
[[ -d /home/backup/pkg-old ]] && $pfx mv /home/backup/pkg-old /home/backup/pkg-1
[[ -d /home/backup/pkg-older ]] && $pfx mv /home/backup/pkg-older /home/backup/pkg-2
[[ -d /home/backup/pkg-oldest ]] && $pfx mv /home/backup/pkg-oldest /home/backup/pkg-3

#
## begin duplicate removal
#

echo ""
for ((i=0;i<$PDIRS;i++)); do

  SRCDIR=${DIRLIST[$i]}
  BUDIR=${DIRLIST[$((i+1))]}

  # check for budir name and create if necessary
  [[ -d ${BUDIR} ]] || $pfx mkdir -p ${BUDIR}
  [[ -d ${BUDIR} ]] || usage "unable to create ${BUDIR}, exiting..."

  # compare directory md5sum, skip dup removal if they match
  chkpkgmd5sum $SRCDIR
  if [[ $? -eq 1 ]] && [[ $FORCESCAN -eq 0 ]]; then
    printf "    ${ltblu}%-23s:${ltgrn}  md5 checksum matches -> skipping${nc}\n" ${SRCDIR}
  else
    echo -e "\n  ${ltblu}calling:${nc}  'fduppkg $SRCDIR -d $BUDIR -l $LOG'\n"
    case $1 in
      -v | --verbose ) $pfx $FDUPPKG $SRCDIR -d $BUDIR -l $LOG -v;;
      * ) $pfx $FDUPPKG $SRCDIR -d $BUDIR -l $LOG;;
    esac
  fi

  # get the size on disk of the directory
  DSIZE[$CNT]="$(du -hs $SRCDIR)"
  ((CNT+=1))

done

## summary
DSIZE[$CNT]="$(du -hs $BUDIR)"
((CNT+=1))
echo -e "\n${ltblu}  Package Disk Usage Summary${nc}\n"
for ((i=0;i<$CNT;i++)); do
  tmpln=${DSIZE[$i]}
  SIZE=${tmpln%%[[:blank:]]*}
  NAME=${tmpln##*[[:blank:]]}
  printf "    %-4s   %s\n" $SIZE $NAME
done
echo ""

## bzip2 the log file
[[ -r $LOG ]] && $pfx bzip2 -f $LOG
echo ""

## write new pkgdir checksums
crpkgdirmd5

## restore working dirctory
cd $CURDIR

exit 0
