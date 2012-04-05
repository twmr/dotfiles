#!/bin/bash

CWD=`pwd`
FILESFOUND=/tmp/filesfound.txt
FILESSIZE=/tmp/filessize.txt
DUPLICATE_SETS_FOUND=/tmp/duplicate_sets_found.txt
DUPLICATES_TO_DELETE=/tmp/duplicates_to_delete.txt
DUPLICATE_DIR=~/duplicates
COUNT=0
F_COUNT=0
DIR_COUNT=0
EXT_COUNT=1

##################################################################
if [ ! -d $DUPLICATE_DIR ]; then
        mkdir -p $DUPLICATE_DIR
fi

##################################################################
# remove any previous output files
rm -rf $FILESSIZE
rm -rf $FILESFOUND
rm -rf $DUPLICATE_SETS_FOUND
rm -rf $DUPLICATES_TO_DELETE

##################################################################
echo
echo "Duplicate Image Finder"
echo
echo "Press enter for current directory"
echo "Or enter directory path to scan: "
read ANSWER
if [ "$ANSWER" == "" ]; then
        ANSWER="$CWD"
fi

##################################################################
# rename any directory name with spaces with an underscore
find $ANSWER -type d -iname '* *' -exec sh -c 'mv "$1" "${1// /_}"' -- {} \; 2> /dev/null
# rename any files name with spaces with an underscore
find $ANSWER -type f -iname '* *' -exec sh -c 'mv "$1" "${1// /_}"' -- {} \; 2> /dev/null

##################################################################
# find images
 for x in `find $ANSWER -type f -name "*.[Jj][Pp][Gg]"`; do
        COUNT=$(($COUNT + 1 ))
        ls -l "$x" | awk '{print $5,$9}' >> $FILESFOUND
        ls -l "$x" | awk '{print $5}' >> $FILESSIZE
done

# if no images files are found just exit script
if [ ! -e $FILESFOUND ] || [ ! -e $FILESSIZE ]; then
        echo "No image files found..........exiting"
        exit
fi

# find duplicate sets and remove one entry so as not to remove the original with subsequent duplicates
cat $FILESSIZE | sort | uniq -w 32 -d --all-repeated=separate | uniq > $DUPLICATE_SETS_FOUND
 for f in `cat $DUPLICATE_SETS_FOUND`; do
        grep "$f" "$FILESFOUND" | awk 'a ~ $1; {a=$1}' | awk '{print $2}' >> $DUPLICATES_TO_DELETE
done

#        if no duplicates are found exit script
if [ ! -e $DUPLICATES_TO_DELETE ]; then
        echo "Number of files scanned: $COUNT"
        echo "No duplicate files found"
        exit
fi

# instead of deleting move to the duplicate directory for inspection, just have to delete manually
for FILE in `cat $DUPLICATES_TO_DELETE`; do
        NAME=`basename $FILE`
        F_COUNT=$(($F_COUNT + 1 ))
                        if [ ! -e $DUPLICATE_DIR/$NAME ]; then # check to se if file name exist in duplicate directory before trying to move
                                mv $FILE $DUPLICATE_DIR
                        else
                                # if file exists strip the file extension so we can rename the file with a -1 to the end
                                ORG_NAME=`basename $FILE | cut -d "." -f 1` # get the name and strip off the file extension
                                FILE_EXT=`basename $FILE | cut -d "." -f 2` # get the file extension type
                                NEW_NAME="$ORG_NAME-$EXT_COUNT.$FILE_EXT"
                                        while [ -e $DUPLICATE_DIR/$NEW_NAME ]; do
                                                EXT_COUNT=$(($EXT_COUNT + 1 ))
                                                NEW_NAME="$ORG_NAME-$EXT_COUNT.$FILE_EXT"
                                        done
                                mv $FILE $DUPLICATE_DIR/$NEW_NAME
                        fi
done

##################################################################
# remove empty directories if they exist
 EMPTY_DIR=`find $ANSWER -depth -type d -empty`
        for EMPTY in $EMPTY_DIR; do
                DIR_COUNT=$(($DIR_COUNT + 1 ))
                rm -rf $EMPTY
        done

echo "Number of Files Checked: $COUNT"
echo "Number of duplicate files deleted/moved: $F_COUNT"
echo "Number of empty directories deleted: $DIR_COUNT "

##################################################################
