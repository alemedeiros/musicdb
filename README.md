#musicdb

A small project for artist/music information database using MusicBrainz as a
source of information (Individual Project for QMUL's ECS713 - Functional
Programming Module).

Author: alemedeiros (Alexandre Medeiros)

##How to build

If you don't want to install the dependencies on the whole system, initialize a
cabal sandbox then install the dependencies

    cabal sandbox init
    cabal install --only-dependencies

Then configure and build the project with

    cabal configure
    cabal build

##Running

The binary is `dist/build/musicdb/musicdb`, you can either add it to your `PATH`
variable with the command `export PATH=$PATH:./dist/build/musicdb` or you can
run it directly with `./dist/build/musicdb/musicdb`.

The commands are passed via command line arguments. There is a help command that
sums the usage of the program.

##Documentation

To generate the documentation for the whole project, run the command

    cabal haddock --all

The documentation can be viewed with your favourite browser, just open the file
`dist/doc/html/musicdb/musicdb/index.html`.
