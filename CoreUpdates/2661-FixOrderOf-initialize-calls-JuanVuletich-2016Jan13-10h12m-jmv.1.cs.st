'From Cuis 4.2 of 25 July 2013 [latest update: #2658] on 13 January 2016 at 10:13:32.721318 am'!

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 1/13/2016 10:13'!
fileOutOn: stream 
	"Write out all the changes the receiver knows about"

	| classList |
	(self isEmpty and: [ stream isKindOf: FileStream ])
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


!CodePackage methodsFor: 'saving' stamp: 'jmv 1/13/2016 10:08'!
write: classes initializersOn: aStream

	Smalltalk hierarchySorted: classes do: [ :class |
		(class class includesSelector: #initialize) ifTrue: [
			aStream nextChunkPut: class name, ' initialize'; newLine ]]! !

