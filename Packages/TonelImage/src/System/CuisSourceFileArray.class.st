"
Cuis source code access mechanism.
Uses the range for sourcePointer in CompiledMethods (16r1000000 to 16r4FFFFFF) quite differently than StandardSourceFileArray (the older way, inherited from Squeak). First half is for Sources, second half is for Changes. The actual offset in the file is the sourcePointer minus 16r1000000 (or minus 16r3000000) multiplied by a scaling factor. This scaling factor is (right now) 32, raising the limit to 1Gb.

See the class comment at MigratingSourceFileArray to see how to activate this.
"
Class {
	#name : 'CuisSourceFileArray',
	#superclass : 'SequenceableCollection',
	#instVars : [
		'files',
		'pointerScale'
	],
	#category : 'System-Files',
	#package : 'System',
	#tag : 'Files'
}

{ #category : 'instance creation' }
CuisSourceFileArray class >> install [
	"Replace SourceFiles by an instance of me with the standard sources and changes files.
	If migrating, use MigratingSourceFileArray, see that class' comment"

	"CuisSourceFileArray install"

	SourceFiles := self new
]

{ #category : 'instance creation' }
CuisSourceFileArray class >> new: nFiles [
	^self new initialize: nFiles.
]

{ #category : 'accessing' }
CuisSourceFileArray >> at: index [
	^files at: index
]

{ #category : 'accessing' }
CuisSourceFileArray >> at: index put: aFile [
	files at: index put: aFile
]

{ #category : 'sourcePointer conversion' }
CuisSourceFileArray >> checkOKToAdd: size at: filePosition in: fileIndex [
	"Issue several warnings as the end of the changes file approaches its limit,
	and finally halt with an error when the end is reached."

	| fileSizeLimit margin |
	fileSizeLimit := 16r2000000 * pointerScale.
	3 to: 1 by: -1 do:
		[:i | margin := i*100000.
		(filePosition + size + margin) > fileSizeLimit
			ifTrue: [(filePosition + margin) > fileSizeLimit ifFalse:
						[self inform: 'WARNING: your changes file is within
' , margin printString , ' characters of its size limit.
You should take action soon to reduce its size.
You may proceed.']]
			ifFalse: [^ self]].
	(filePosition + size > fileSizeLimit) ifFalse: [^ self].
	self error: 'You have reached the size limit of the changes file.
You must take action now to reduce it.
Close this error.  Do not attempt to proceed.'
]

{ #category : 'sourcePointer conversion' }
CuisSourceFileArray >> fileIndexFromSourcePointer: anInteger [
	"Return the index of the source file which contains the source chunk addressed by anInteger"
	"No source pointer => no fileIndex"
	anInteger = 0 ifTrue: [ ^0 ].
	^(anInteger - 16r1000000 anyMask: 16r2000000)
		ifTrue: [2]
		ifFalse: [1]
]

{ #category : 'sourcePointer conversion' }
CuisSourceFileArray >> filePositionFromSourcePointer: anInteger [
	"Return the position of the source chunk addressed by anInteger"

	| unscaledPosition |
	"No source pointer => no filePosition"
	anInteger = 0 ifTrue: [ ^0 ].
	unscaledPosition := (anInteger - 16r1000000) bitAnd: 16r1FFFFFF.
	^unscaledPosition * pointerScale
]

{ #category : 'initialization' }
CuisSourceFileArray >> initialize [
	files := Array new: 2.
	files at: 1 put: (SourceFiles at: 1).
	files at: 2 put: (SourceFiles at: 2).
	pointerScale := 32	"To change the pointerScale (and therefore the size limit for Sources and Changes, change this value, and use a MigratingSourceFileArray to create the new Sources and Changes files. See class comment there."
]

{ #category : 'initialization' }
CuisSourceFileArray >> initialize: nFiles [
	files := Array new: nFiles.
	pointerScale := 32
]

{ #category : 'accessing' }
CuisSourceFileArray >> pointerScaleForWriting [
	^pointerScale
]

{ #category : 'accessing' }
CuisSourceFileArray >> size [
	^files size
]

{ #category : 'sourcePointer conversion' }
CuisSourceFileArray >> sourcePointerFromFileIndex: index andPosition: position [
	"Return a sourcePointer encoding the given file index and position"
	| answer changesFlag |
	((index between: 1 and: 2) and: [position >= 0])
		ifFalse: [self error: 'invalid source code pointer'].
	changesFlag := index = 2 ifTrue: [ 16r2000000 ] ifFalse: [ 0 ].
	answer := (position // pointerScale bitOr: changesFlag) + 16r1000000.
	^answer
]
