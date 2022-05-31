'From Cuis 6.0 [latest update: #5209] on 31 May 2022 at 4:12:55 pm'!
!classDefinition: #ArrayOfCharactersAndCodePoints category: #'System-Text'!
Array variableSubclass: #ArrayOfCharactersAndCodePoints
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!Object methodsFor: 'testing' stamp: 'jmv 5/31/2022 15:34:04'!
isUnicodeCodePoint
	"Overridden to return true in UnicodeCodePoint, natch"
	^ false! !


!SequenceableCollection methodsFor: 'converting' stamp: 'jmv 5/31/2022 15:14:15'!
asStreamResult
	^self! !

!SequenceableCollection methodsFor: 'testing' stamp: 'jmv 5/31/2022 15:37:21'!
canHoldCodePoints
	"Why not?"
	^true! !


!String methodsFor: 'converting' stamp: 'jmv 5/31/2022 15:39:11'!
thatCanHoldCodePoints
	^ArrayOfCharactersAndCodePoints withAll: self! !

!String methodsFor: 'testing' stamp: 'jmv 5/31/2022 15:38:15'!
canHoldCodePoints
	"Nope. Needs something like Utf8String or Utf32String for that. Even an Array is better than us."
	^false! !


!ArrayOfCharactersAndCodePoints methodsFor: 'accessing' stamp: 'jmv 5/31/2022 15:59:57'!
at: index
	"Full compatibility with String"
	^(super at: index) ifNil: [ `Character null` ].! !

!ArrayOfCharactersAndCodePoints methodsFor: 'converting' stamp: 'jmv 5/31/2022 15:14:28'!
asStreamResult
	^Utf8String newFrom: self! !


!Text methodsFor: 'converting' stamp: 'jmv 5/31/2022 15:15:58'!
asStreamResult
	self setString: string asStreamResult setRuns: runs.
	^self! !

!Text methodsFor: 'converting' stamp: 'jmv 5/31/2022 15:39:54'!
thatCanHoldCodePoints
	string _ string thatCanHoldCodePoints! !

!Text methodsFor: 'testing' stamp: 'jmv 5/31/2022 15:37:39'!
canHoldCodePoints
	"Ask our string"
	^string canHoldCodePoints! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 5/31/2022 15:34:13'!
isUnicodeCodePoint
	^true! !


!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 5/31/2022 15:50:25'!
new: sizeRequested
	^self withBytes: (ByteArray new: sizeRequested)! !


!SequenceableCollection class methodsFor: 'stream creation' stamp: 'jmv 5/31/2022 15:27:41'!
streamContents: blockWithArg estimatedSize: estimatedSize

	| stream originalContents |
	
	stream _ WriteStream on: (self ofSize: estimatedSize).
	blockWithArg value: stream.
	originalContents _ stream originalContents.
	
	^stream position = originalContents size
		ifTrue: [ originalContents asStreamResult ]
		ifFalse: [ stream contents ]! !


!PositionableStream methodsFor: 'accessing' stamp: 'jmv 5/31/2022 15:28:46'!
contents
	"Answer with a copy of my collection from 1 to readLimit."

	^ (collection copyFrom: 1 to: readLimit) asStreamResult! !


!WriteStream methodsFor: 'accessing' stamp: 'jmv 5/31/2022 15:27:30'!
contents
	readLimit _ readLimit max: position.
	^(collection copyFrom: 1 to: position) asStreamResult! !

!WriteStream methodsFor: 'accessing' stamp: 'jmv 5/31/2022 15:40:16'!
nextPut: anObject 
	"Insert the argument at the next position in the Stream represented by the receiver."

	(anObject isUnicodeCodePoint and: [collection canHoldCodePoints not]) ifTrue: [
		collection _ collection thatCanHoldCodePoints ].
	position >= writeLimit
		ifTrue: [^ self pastEndPut: anObject]
		ifFalse: [
			position _ position + 1.
			^collection at: position put: anObject]! !

!WriteStream methodsFor: 'accessing' stamp: 'jmv 5/31/2022 15:43:32'!
nextPutAll: aCollection

	| newEnd |
	(aCollection canHoldCodePoints and: [collection canHoldCodePoints not]) ifTrue: [
		collection _ collection thatCanHoldCodePoints ].
	(self isCompatibleWithContents: aCollection)
		ifFalse: [ ^ super nextPutAll: aCollection ].

	newEnd _ position + aCollection size.
	newEnd > writeLimit ifTrue: [
		self growTo: newEnd + 10].

	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: 1.
	position _ newEnd.! !


!ReadWriteStream methodsFor: 'accessing' stamp: 'jmv 5/31/2022 15:28:17'!
contents
	"Answer with a copy of my collection from 1 to readLimit."

	readLimit _ readLimit max: position.
	^ (collection copyFrom: 1 to: readLimit) asStreamResult! !

