#!/bin/bash
# examples.sh
#   by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
#
# Sample use of musicdb application

bin=./dist/build/musicdb/musicdb

# Prepare relevant files
cabal build
rm -f music.db

# Initialize
$bin init

# Search online database
$bin search "the doors"       # Normal search
sleep 3
$bin search "ThE DoOrS" -l 8  # Increased result limit and case doesn't matter
sleep 3
$bin search "asdf"            # Result should be empty
sleep 3

# Fetch some data (this will take a while...)
$bin add "arctic monkeys"
sleep 3
$bin add "the strokes"
sleep 3
$bin add "radiohead"
sleep 3
$bin add "franz ferdinand"
sleep 3
$bin add "muse"
sleep 3
$bin add "the xx"
sleep 3
$bin add "alt-j"
sleep 3
$bin add "madonna"
sleep 3
$bin add "lady gaga"
sleep 3
$bin add "metallica"
sleep 3
$bin add "pantera"
sleep 3
$bin add "slayer"
sleep 3
$bin add "megadeth"
sleep 3
$bin add "anthrax"
sleep 3
$bin add "daft punk"
sleep 3

# Add Artist by ID
$bin add "541f16f5-ad7a-428e-af89-9fa1b16d3c9c" -i # Artist: Pantera
sleep 3

# Sample queries
$bin query "arctic monkeys" "am"
$bin query "aRcTiC MoNkEyS" "aM" # Again, case doesn't matter

# Show some recommendations
$bin recommend "metallica" "master of puppets"
$bin recommend "muse" "showbiz"

# All the commands that interact with the local database can have the option to
# use a different database file, as shown below

rm -f sample.db
$bin init -f sample.db
$bin add "the beatles" -f sample.db               # Add a band that's unique to the new file
sleep 3
$bin add "the rolling stones" -f sample.db
sleep 3
$bin add "arctic monkeys" -f sample.db
sleep 3
$bin query "the beatles" "revolver" -f sample.db  # Query on new file
$bin query "the beatles" "revolver"               # Query on default file
$bin recommend "the beatles" "revolver" -f sample.db # TODO
