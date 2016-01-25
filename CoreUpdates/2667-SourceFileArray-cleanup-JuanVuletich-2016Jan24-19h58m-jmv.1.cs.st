'From Cuis 4.2 of 25 July 2013 [latest update: #2665] on 24 January 2016 at 8:01:18.767656 pm'!
!classDefinition: #CuisSourceFileArray category: #'System-Files'!
SequenceableCollection subclass: #CuisSourceFileArray
	instanceVariableNames: 'files pointerScale '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Files'!

!CuisSourceFileArray commentStamp: 'jmv 1/23/2016 23:18' prior: 0!
Cuis source code access mechanism.
Uses the range for sourcePointer in CompiledMethods (16r1000000 to 16r4FFFFFF) quite differently than StandardSourceFileArray (the older way, inherited from Cuis). First half is for Sources, second half is for Changes. The actual offset in the file is the sourcePointer minus 16r1000000 (or minus 16r3000000) multiplied by a scaling factor. This scaling factor is (right now) 32, raising the limit to 1Gb.

See the class comment at MigratingSourceFileArray to see how to activate this.!

!classDefinition: #CuisSourceFileArray category: #'System-Files'!
SequenceableCollection subclass: #CuisSourceFileArray
	instanceVariableNames: 'files pointerScale'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Files'!
!classRemoval: #MigratingSourceFileArray!
Smalltalk removeClassNamed: #MigratingSourceFileArray!
!classRemoval: #SourceFileArray!
Smalltalk removeClassNamed: #SourceFileArray!
!classRemoval: #StandardSourceFileArray!
Smalltalk removeClassNamed: #StandardSourceFileArray!
