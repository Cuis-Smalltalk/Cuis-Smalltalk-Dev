'From Cuis6.3 [latest update: #6171] on 8 January 2024 at 11:58:37 am'!

!Collection methodsFor: 'testing' stamp: 'jmv 1/8/2024 11:52:21'!
canHoldAnyCodePoint
	"Why not?"
	^true! !


!String methodsFor: 'converting' stamp: 'jmv 1/8/2024 11:54:45'!
thatCanHoldAnyCodePoint
	"See senders.
	Also see #asStreamResult"
	^ArrayOfCharactersAndCodePoints withAll: self! !

!String methodsFor: 'testing' stamp: 'jmv 1/8/2024 11:53:48'!
canHoldAnyCodePoint
	"Nope.
	We can only hold Code Points less than 256.
	Needs something like UnicodeString for others. Even an Array is better than us."
	^false! !


!Text methodsFor: 'converting' stamp: 'jmv 1/8/2024 11:54:54'!
thatCanHoldAnyCodePoint
	"See senders.
	Also see #asStreamResult	"
	string := string thatCanHoldAnyCodePoint! !

!Text methodsFor: 'testing' stamp: 'jmv 1/8/2024 11:52:30'!
canHoldAnyCodePoint
	"Ask our string"
	^string canHoldAnyCodePoint! !


!String methodsFor: 'copying' stamp: 'jmv 1/8/2024 11:51:02'!
copyWith: newElement
	newElement codePoint > 255 ifFalse: [
		^self asUnicodeString copyWith: newElement ].
	^super copyWith: newElement! !


!WriteStream methodsFor: 'accessing' stamp: 'jmv 1/8/2024 11:58:03'!
nextPut: anObject 
	"Insert the argument at the next position in the Stream represented by the receiver."

	(anObject isCharacter and: [anObject codePoint > 255 and: [collection canHoldAnyCodePoint not]]) ifTrue: [
		collection := collection thatCanHoldAnyCodePoint ].
	position >= writeLimit
		ifTrue: [^ self pastEndPut: anObject]
		ifFalse: [
			position := position + 1.
			^collection at: position put: anObject].! !

!WriteStream methodsFor: 'accessing' stamp: 'jmv 1/8/2024 11:55:05'!
nextPutAll: aCollection

	| newEnd |
	(aCollection canHoldAnyCodePoint and: [collection canHoldAnyCodePoint not]) ifTrue: [
		collection := collection thatCanHoldAnyCodePoint ].
	(self isCompatibleWithContents: aCollection)
		ifFalse: [ ^ super nextPutAll: aCollection ].

	newEnd := position + aCollection size.
	newEnd > writeLimit ifTrue: [
		self growTo: newEnd + 10].

	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: 1.
	position := newEnd.! !

!WriteStream methodsFor: 'accessing' stamp: 'jmv 1/8/2024 11:55:08'!
nextPutAllString: aString withAttributes: attributesArray

	| newEnd |
	(aString canHoldAnyCodePoint and: [collection canHoldAnyCodePoint not]) ifTrue: [
		collection := collection thatCanHoldAnyCodePoint ].
	(self isCompatibleWithContents: aString)
		ifFalse: [ ^ self nextPutAll: aString ].

	newEnd := position + aString size.
	newEnd > writeLimit ifTrue: [
		self growTo: newEnd + 10].

	collection
		replaceFrom: position+1
		to: newEnd 
		withString: aString
		attributes: attributesArray
		startingAt: 1.
	position := newEnd! !

!methodRemoval: Text #thatCanHoldCodePoints stamp: 'jmv 1/8/2024 11:55:13'!
Text removeSelector: #thatCanHoldCodePoints!
!methodRemoval: Text #canHoldCodePoints stamp: 'jmv 1/8/2024 11:54:22'!
Text removeSelector: #canHoldCodePoints!
!methodRemoval: String #thatCanHoldCodePoints stamp: 'jmv 1/8/2024 11:55:23'!
String removeSelector: #thatCanHoldCodePoints!
!methodRemoval: String #canHoldCodePoints stamp: 'jmv 1/8/2024 11:54:21'!
String removeSelector: #canHoldCodePoints!
!methodRemoval: Collection #canHoldCodePoints stamp: 'jmv 1/8/2024 11:54:21'!
Collection removeSelector: #canHoldCodePoints!
!methodRemoval: UnicodeCodePoint #isUnicodeCodePoint stamp: 'jmv 1/8/2024 11:58:12'!
UnicodeCodePoint removeSelector: #isUnicodeCodePoint!
!methodRemoval: Object #isUnicodeCodePoint stamp: 'jmv 1/8/2024 11:58:10'!
Object removeSelector: #isUnicodeCodePoint!
