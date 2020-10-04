Additional Packages for Cuis
================================

The Cuis base image includes only kernel functionailty, very basic libraries, and development tools. Optional functionality, that can be loaded as needed, is stored in separate code Packages. The Cuis community developes and maintains several dozens of such Packages.

Some of them are included in the main Cuis GitHub repository, at https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev in the 'Packages' folder. Loading 'Core-Packages.pck.st' loads them all.

To load only some packages execute in a Workspace for example

    Feature require: 'Sound'.
    Feature require: 'WebClient'.
    Feature require: 'JSON'.

Many additional packages are being developed by members of the Cuis community, and stored in their own repo. Most of them are of very high quality, well maintained and really useful.

Some of the repos owned by community members, and including packages for Cuis are:
(Note: Repos whose name starts with 'Cuis' are intended for Cuis Smalltalk)

- https://github.com/Cuis-Smalltalk/Learning-Cuis
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
- https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-StyledTextEditor
- https://github.com/hernanwilkinson/Cuis-Smalltalk-Aconcagua
- https://github.com/hernanwilkinson/Cuis-Smalltalk-Chalten
- https://github.com/hernanwilkinson/Cuis-Smalltalk-Refactoring
- https://github.com/hernanwilkinson/Cuis-Smalltalk-DenotativeObject
- https://github.com/len/Mathematics
- https://github.com/len/RayTracer
- https://github.com/len/Cuis-Smalltalk-DWM
- https://gitlab.com/klgcuisstuff/futures
- https://github.com/Phantasus/Cuis-Smalltalk-CBOR

(if your repo with Cuis stuff is missing, please email the Cuis mail list)

Don't forget to check them when searching for useful packages for Cuis!
