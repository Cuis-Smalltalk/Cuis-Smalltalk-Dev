Additional Packages for Cuis
================================

The Cuis base image includes only kernel functionailty, very basic libraries, and development tools. Optional functionality, that can be loaded as needed, is stored in separate code Packages. The Cuis community developes and maintains several dozens of such Packages.

The main Cuis GitHub repository, at https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev includes a 'Packages' folder. Packages here include basic libraries and system extensions that are used by many other packages and applications. They are maintained and kept up to date in sync with the base Cuis image.

You can load them like this:

    Feature require: 'Sound'.
    Feature require: 'WebClient'.
    Feature require: 'JSON'.

The Cuis Smalltalk GitHub organization, at at https://github.com/Cuis-Smalltalk include over 30 additional repositories with community developed and maintained code. Repos in the Cuis Smalltalk organization are meant for wide use. Most of them are of very high quality, well maintained and really useful. Explore them!

Some Cuis developers may prefer to host their packages in personal repos. This is usually best for code that is not yet ready for wide adoption. You're welcome to explore them and contact authors if you have questions.

Some of the repos owned by community members, and including packages for Cuis are:
(Note: Repos whose name starts with 'Cuis' are intended for Cuis Smalltalk)

- https://github.com/hernanwilkinson/LiveTyping
- https://github.com/hernanwilkinson/Cuis-Smalltalk-Refactoring
- https://github.com/hernanwilkinson/Cuis-Smalltalk-DenotativeObject
- https://github.com/len/Signals
- https://github.com/len/Arrows
- https://github.com/len/Graphs
- https://github.com/len/RayTracer
- https://github.com/len/Cuis-Smalltalk-DWM
- https://github.com/KenDickey?tab=repositories
- https://github.com/hhzl?tab=repositories
- https://github.com/dtlewis290?tab=repositories
- https://github.com/davidgraham?tab=repositories
- https://github.com/muspellsson?tab=repositories
- https://github.com/garduino?tab=repositories
- https://github.com/bpieber?tab=repositories
- https://github.com/pmon?tab=repositories
- https://github.com/mumez?tab=repositories
- https://github.com/TakuriTakahashi?tab=repositories
- https://github.com/pbella?tab=repositories
- https://github.com/dhnorton?tab=repositories
- https://bitbucket.org/mmontone/cuis-smalltalk-preferencebrowser
- https://gitlab.com/klgcuisstuff/futures

(if your repo with Cuis stuff is missing, please email the Cuis mail list to add it here)

Don't forget to check them when searching for useful packages for Cuis!
