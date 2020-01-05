#!/bin/bash

# 1001_Gauche_099_workaround.sh
# 2020-1-5 v1.01
#
# for Gauche 0.9.9 header file workaround
#

set -e

##### settings #####
GAUCHE_VERSION=`gosh -b -e'(display (gauche-version))' -Eexit`
HEADER_SRC_DIR=`gauche-config --sysincdir`
HEADER_COPY_DIR=header1000
GAUCHE_HEADER_FILE=gauche.h
GAUCHE_HEADER_SUBDIR=gauche
GAUCHE_MODIFY_FILES="number.h vector.h uvector.h"

##### functions #####
function usage {
    echo "Usage: 1001_Gauche_099_workaround.sh"
}

function do_check_file {
    if [ ! -f "$1" ]; then
        echo "File '$1' not found.  Aborting."; exit 1
    fi
}

function do_check_dir {
    if [ ! -d "$1" ]; then
        echo "Directory '$1' not found.  Aborting."; exit 1
    fi
}

function do_backup_file {
    local orig_file="$1"
    local bkup_file="$1.orig1000"
    if [ ! -f "$bkup_file" ]; then
        cp "$orig_file" "$bkup_file"
    fi
}

function do_patch_to_file {
    local patch_file="$1"
    local temp_file="$1.temp5001"

    # replace 'complex double' to 'double _Complex'
    if ! grep -q -e 'ScmDoubleComplex' $patch_file; then
        if ! grep -q -e 'double _Complex' $patch_file; then
            cp $patch_file $temp_file
            sed -e 's@complex double@double _Complex@' $temp_file > $patch_file
        fi
    fi

    # replace 'complex float' to 'float _Complex'
    if ! grep -q -e 'ScmFloatComplex' $patch_file; then
        if ! grep -q -e 'float _Complex' $patch_file; then
            cp $patch_file $temp_file
            sed -e 's@complex float@float _Complex@' $temp_file > $patch_file
        fi
    fi

    rm -f $temp_file
}

##### main #####

while [ "$#" -gt 0 ]; do
    case $1 in
        *) usage; exit 1;;
    esac
done

mkdir -p "$HEADER_COPY_DIR"

echo "Gauche version is $GAUCHE_VERSION ."
if [ "$GAUCHE_VERSION" != "0.9.9" ]; then
    echo "Nothing to do."; exit 0
fi

do_check_dir  "$HEADER_SRC_DIR"
do_check_file "$HEADER_SRC_DIR/$GAUCHE_HEADER_FILE"
do_check_dir  "$HEADER_SRC_DIR/$GAUCHE_HEADER_SUBDIR"

cp -f  "$HEADER_SRC_DIR/$GAUCHE_HEADER_FILE"   "$HEADER_COPY_DIR"
cp -rf "$HEADER_SRC_DIR/$GAUCHE_HEADER_SUBDIR" "$HEADER_COPY_DIR"
chmod -R u+w "$HEADER_COPY_DIR"

for file1 in $GAUCHE_MODIFY_FILES; do
    do_backup_file   "$HEADER_COPY_DIR/$GAUCHE_HEADER_SUBDIR/$file1"
    do_patch_to_file "$HEADER_COPY_DIR/$GAUCHE_HEADER_SUBDIR/$file1"
done

echo "Finished successfully."

