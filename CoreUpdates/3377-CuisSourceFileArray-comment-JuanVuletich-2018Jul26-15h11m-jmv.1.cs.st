'From Cuis 5.0 of 7 November 2016 [latest update: #3375] on 26 July 2018 at 3:17:53 pm'!

!CuisSourceFileArray commentStamp: '<historical>' prior: 0!
Cuis source code access mechanism.
Uses the range for sourcePointer in CompiledMethods (16r1000000 to 16r4FFFFFF) quite differently than StandardSourceFileArray (the older way, inherited from Squeak). First half is for Sources, second half is for Changes. The actual offset in the file is the sourcePointer minus 16r1000000 (or minus 16r3000000) multiplied by a scaling factor. This scaling factor is (right now) 32, raising the limit to 1Gb.

See the class comment at MigratingSourceFileArray to see how to activate this.!

