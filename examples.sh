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
$bin search "ThE DoOrS" -l 8  # Increased result limit and case doesn't matter
$bin search "asdf"            # Result should be empty

# Fetch some data
$bin add "arctic monkeys"
$bin add "the strokes"
$bin add "madonna"
$bin add "lady gaga"
$bin add "pink floyd"
$bin add "metallica"
$bin add "daft punk"

# Add Artist by ID
$bin add "541f16f5-ad7a-428e-af89-9fa1b16d3c9c" -i

# Sample queries
$bin query "arctic monkeys" "am"
$bin query "aRcTiC MoNkEyS" "aM" # Again, case doesn't matter

# Show some playlist generation
#$bin genplaylist # TODO


# All the commands that interact with the local database can have the option to
# use a different database file, as shown below

$bin init -f sample.db
$bin add "the beatles" -f sample.db               # Add a band that's unique to the new file
$bin query "the beatles" "revolver" -f sample.db  # Query on new file
$bin query "the beatles" "revolver"               # Query on default file
#$bin genplaylist -f sample.db # TODO

rm sample.db
