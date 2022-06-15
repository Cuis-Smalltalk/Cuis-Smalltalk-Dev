'From Cuis 6.0 [latest update: #5234] on 8 June 2022 at 10:10:32 am'!
!classDefinition: #CharacterSequence category: #'Kernel-Text'!
SequenceableCollection subclass: #CharacterSequence
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Text'!

!CharacterSequence commentStamp: '<historical>' prior: 0!
Superclass for String classes!


!CharacterSequence methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
size
	"Answer the number of indexable fields in the receiver. This value is the 
	same as the largest legal subscript. Primitive is specified here to override 
	SequenceableCollection size. Essential. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 62>
	^self basicSize! !

!CharacterSequence methodsFor: 'adding' stamp: 'ST-80 5/31/1983 9:10:35'!
add: newObject
	self shouldNotImplement! !

!CharacterSequence methodsFor: 'copying' stamp: 'KenD 9/4/2016 18:22:51'!
appendToText: aText

	| textStringSize |
	textStringSize := aText string size.
	^ aText replaceFrom: textStringSize + 1
			 to: textStringSize 
			 with: self asText! !

!CharacterSequence methodsFor: 'comparing' stamp: 'jmv 6/23/2011 09:53'!
commonPartWith: aString startAt: startIndex stopAt: stopIndexRequested applying: aBlock
	"Return the size of the longest common subsequence with aString, only between startIndex and stopIndex.
	Apply aBlock to each character before comparing.
	Do a character-by-character comparison between the receiver and aString.  Return the index of the final character that matched exactly."
	| stopIndex |
	stopIndex _ stopIndexRequested min: aString size.
	startIndex
		to: stopIndex
		do: [ :i |
			(aBlock value: (self at: i)) = (aBlock value: (aString at: i)) ifFalse: [ ^ i - 1 ]].
	^ stopIndex! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/8/2022 09:32:35'!
asByteString
	"Answer an instance of the Byte oriented String class"

	^String fromUtf8Bytes: self asUtf8Bytes! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 4/21/2022 11:43:33'!
asPlainString
	"Answer the receiver itself.
	Not a Symbol, not a Text."
	^self! !

!CharacterSequence methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asString
	"Answer the receiver itself."
	^self! !

!CharacterSequence methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asText
	"Answer a Text whose string is the receiver."
	^Text fromString: self! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/8/2022 09:32:54'!
asUtf8Bytes
	^self subclassResponsibility! !

!CharacterSequence methodsFor: 'converting' stamp: 'di 1/16/98 16:33'!
contractTo: smallSize
	"return myself or a copy shortened by ellipsis to smallSize"
	| leftSize |
	self size <= smallSize
		ifTrue: [^ self].  "short enough"
	smallSize < 5
		ifTrue: [^ self copyFrom: 1 to: smallSize].    "First N characters"
	leftSize _ smallSize-2//2.
	^ self copyReplaceFrom: leftSize+1		"First N/2 ... last N/2"
		to: self size - (smallSize - leftSize - 3)
		with: '...'
"
	'A clear but rather long-winded summary' contractTo: 18
	(Utf8String withBytes: 'A clear but rather long-winded summary' asUtf8) contractTo: 18
"! !

!CharacterSequence methodsFor: 'enumerating' stamp: 'jmv 6/8/2022 09:46:46'!
collect: aBlock
	"Refer to the comment in Collection >> #collect:."
	| array |
	array _ self collectInArray: aBlock.
	(array allSatisfy: [ :c | c isCharacter ]) ifFalse: [
		^array ].
	^self class newFrom: array! !

!CharacterSequence methodsFor: 'enumerating' stamp: 'jmv 6/8/2022 09:47:25'!
collectInArray: aBlock
	"Refer to the comment in Collection >> #collect:."
	| result i |
	result _ Array new: self size.
	i _ 1.
	self do: [ :codePoint |
		result at: i put: (aBlock value: codePoint).
		i _ i + 1 ].
	^ result.! !

!CharacterSequence methodsFor: 'testing' stamp: 'jmv 6/8/2022 08:41:33'!
isEmpty
	^self size = 0! !

!CharacterSequence methodsFor: 'testing' stamp: 'jmv 6/6/2022 10:54:40'!
isString
	^ true! !

!CharacterSequence methodsFor: 'user interface' stamp: 'jmv 1/2/2013 15:26'!
edit

	self editLabel: self class name, ' Text Editor'! !

!CharacterSequence methodsFor: 'user interface' stamp: 'jmv 5/11/2022 10:20:22'!
editLabel: labelString

	(TextModel withText: self) openLabel: labelString! !


!CharacterSequence class methodsFor: 'instance creation' stamp: 'jmv 9/29/2019 12:52:40'!
new
	"Answer a new instance of me, with size = 0, unles a specific size is used."

	^self new: self numElements! !

!CharacterSequence class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
new: size withAll: value 
	"Answer a new instance of me, whose every element is equal to the argument,
	value."

	^(self new: size) atAllPut: value! !

!CharacterSequence class methodsFor: 'instance creation' stamp: 'jmv 3/7/2015 22:23'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."
	| newArray index |
	newArray _ self new: aCollection size.
	index _ 0.
	aCollection do: [ :each | newArray at: (index _ index + 1) put: each].
	^ newArray

"
	Array newFrom: {1. 2. 3}
	{1. 2. 3} as: Array
	{1. 2. 3} as: ByteArray
	{$c. $h. $r} as: String
	{$c. $h. $r} as: Text
"! !

!CharacterSequence class methodsFor: 'instance creation' stamp: 'jmv 9/29/2019 12:47:53'!
numElements
	"Some subclasses create instances of a specific size, and a non-zero number"
	^0! !

!CharacterSequence class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
with: anObject 
	"Answer a new instance of me, containing only anObject."

	| newCollection |
	newCollection _ self new: 1.
	newCollection at: 1 put: anObject.
	^newCollection! !

!CharacterSequence class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
with: firstObject with: secondObject 
	"Answer a new instance of me, containing firstObject and secondObject."

	| newCollection |
	newCollection _ self new: 2.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	^newCollection! !

!CharacterSequence class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
with: firstObject with: secondObject with: thirdObject 
	"Answer a new instance of me, containing only these three objects."

	| newCollection |
	newCollection _ self new: 3.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	^newCollection! !

!CharacterSequence class methodsFor: 'instance creation' stamp: 'jmv 3/7/2015 23:10'!
withAll: aSequenceableCollection
	"Create a new collection containing all the elements from aSequenceableCollection."

	^ (self new: aSequenceableCollection size)
		replaceFrom: 1
		to: aSequenceableCollection size
		with: aSequenceableCollection! !

!CharacterSequence class methodsFor: 'stream creation' stamp: 'jmv 5/22/2022 21:07:37'!
writeStream
	^ WriteStream on: (self new: 100)! !


!CharacterSequence reorganize!
('accessing' size)
('adding' add:)
('copying' appendToText:)
('comparing' commonPartWith:startAt:stopAt:applying:)
('converting' asByteString asPlainString asString asText asUtf8Bytes contractTo:)
('enumerating' collect: collectInArray:)
('testing' isEmpty isString)
('user interface' edit editLabel:)
!

