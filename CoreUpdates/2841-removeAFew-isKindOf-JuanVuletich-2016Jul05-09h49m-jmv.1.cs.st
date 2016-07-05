'From Cuis 4.2 of 25 July 2013 [latest update: #2839] on 5 July 2016 at 9:52:17.400253 am'!

!PositionableStream methodsFor: 'testing' stamp: 'jmv 7/4/2016 22:38'!
isFileStream
	^false! !


!FileStream methodsFor: 'testing' stamp: 'jmv 7/4/2016 22:38'!
isFileStream
	^true! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 7/4/2016 22:04'!
fileOutOn: stream 
	"Write out all the changes the receiver knows about"

	| classList |
	self isEmpty
		ifTrue: [ self inform: 'Warning: no changes to file out' ].
	classList _ Array streamContents: [ :strm |
		Smalltalk hierarchySorted: self changedClasses do: [ :cls | strm nextPut: cls ]].

	"First put out rename, max classDef and comment changes."
	classList do: [ :aClass | self fileOutClassDefinition: aClass on: stream ].

	"Then put out all the method additions"
	classList do: [ :aClass | self fileOutMethodAdditionsFor: aClass on: stream ].

	"Then put out all the method changes"
	classList do: [ :aClass | self fileOutMethodChangesFor: aClass on: stream ].

	"Finally put out removals, final class defs and reorganization if any"
	classList reverseDo: [ :aClass |
		self fileOutMethodRemovalsFor: aClass on: stream.
		self fileOutMethodMovedToOtherPackagesFor: aClass on: stream ].
	"Class initialization goes in hierarchy order (superclass first)"
	classList do: [ :aClass |
		self fileOutPSFor: aClass on: stream ].

	self classRemoves sort do: [ :aClassName |
		stream nextPut: $!!; nextChunkPut: ('classRemoval: #', aClassName); newLine.
		stream nextChunkPut: 'Smalltalk removeClassNamed: #', aClassName; newLine ].

	self classMovesToOtherPackage sort do: [ :aClassName |
		stream nextPut: $!!; nextChunkPut: ('classMoveToSomePackage: #', aClassName); newLine.
		stream nextChunkPut: 'Smalltalk removeClassNamedIfInBaseSystem: #', aClassName; newLine ]! !


!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 7/4/2016 22:39'!
currentChangesName
	"Answer the full path to the version-stable source code currently in use.
	Answer nil if not a file (i.e. if internalized with #internalizeChangeLog or #internalizeSources, or closed with #closeSourceFiles)"
	| changes |
	changes _ SourceFiles second.
	^changes isFileStream
		ifTrue: [ changes name ]! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 7/4/2016 22:39'!
currentSourcesName
	"Answer the full path to the version-stable source code currently in use
	Answer nil if not a file (i.e. if internalized with #internalizeSources, or closed with #closeSourceFiles)"
	| sources |
	sources _ SourceFiles first.
	^sources isFileStream
		ifTrue: [ sources name ]! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 7/4/2016 22:38'!
closeSourceFiles
	"Shut down the source files if appropriate.  1/29/96 sw: changed so that the closing and nilification only take place if the entry was a FileStream, thus allowing stringified sources to remain in the saved image file"

	1 to: 2 do: [ :i |
		(SourceFiles at: i) ifNotNil: [ :strm |
			strm isFileStream
				ifTrue: [
					strm close.
					SourceFiles at: i put: nil ]]]! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 7/4/2016 22:39'!
forceChangesToDisk
	"Ensure that the changes file has been fully written to disk by closing and re-opening it. This makes the system more robust in the face of a power failure or hard-reboot."

	| changesFile |
	changesFile _ SourceFiles at: 2.
	changesFile isFileStream ifTrue: [
		changesFile flush.
		changesFile close.
		changesFile open: changesFile name forWrite: true.
		changesFile setToEnd.
	].
! !

