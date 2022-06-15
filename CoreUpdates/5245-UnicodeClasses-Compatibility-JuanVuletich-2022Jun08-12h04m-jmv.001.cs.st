'From Cuis 6.0 [latest update: #5238] on 8 June 2022 at 12:26:23 pm'!

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/6/2022 17:16:35'!
canBeGlobalVarInitial
	^self isUppercase! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/6/2022 17:16:54'!
isUppercase
	"Answer whether the receiver is an uppercase letter."
self flag: #unicodeNeedsImprovement.
	self asCharacter ifNotNil: [ :ch | ^ch isUppercase ].
	^false! !


!CharacterSequence methodsFor: 'accessing' stamp: 'sw 8/12/97 12:37'!
includesSubString: subString
	^ (self findString: subString startingAt: 1) > 0! !

!CharacterSequence methodsFor: 'comparing' stamp: 'jmv 2/22/2013 07:41'!
beginsWith: prefix
	"Answer whether the receiver begins with the given prefix string.
	The comparison is case-sensitive."

	^self is: prefix substringAt: 1! !

!CharacterSequence methodsFor: 'comparing' stamp: 'jmv 2/22/2013 07:41'!
endsWith: suffix
	"Answer whether the tail end of the receiver is the same as suffix.
	The comparison is case-sensitive."

	^self is: suffix substringAt: self size - suffix size + 1
"
  'Elvis' endsWith: 'vis'
  'Elvis' asUtf8String endsWith: 'vis'
  'Elvis' endsWith: 'vis' asUtf8String
"! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/6/2022 15:36:10'!
afterBlanksEndsWith: aTail

	^(self endsWith: aTail) and: [ self firstNonSeparator = (self size - aTail size + 1) ]
! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 4/21/2022 14:43:55'!
asLowercase
	"Answer a String made up from the receiver whose characters are all 
	lowercase."

	^ self copy asPlainString translateToLowercase! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/6/2022 15:36:06'!
firstNonSeparator

	^self findFirst: [:aChar | aChar isSeparator not ]! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/6/2022 15:36:54'!
lastNonSeparator

	^ self findLast: [:aChar | aChar isSeparator not].
	! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/6/2022 15:36:59'!
withBlanksTrimmed
	"Return a copy of the receiver from which leading and trailing blanks have been trimmed."

	| first |
	
	first _ self firstNonSeparator.
	first = 0 ifTrue: [^ ''].  "no non-separator character"
	
	^ self copyFrom: first to: self lastNonSeparator 

	" ' abc  d   ' withBlanksTrimmed"
! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/6/2022 15:36:16'!
withoutLeadingBlanks
	
	"Return a copy of the receiver from which leading blanks have been trimmed."
	
	| first |
	
	first := self firstNonSeparator.
	first = 0 ifTrue: [^ ''].  
	first = 1 ifTrue: [^ self ].
	
	"no non-separator character"	
	^ self copyFrom: first to: self size
		
	" '    abc  d  ' withoutLeadingBlanks"
! !


!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/6/2022 14:54:20'!
findString: subString startingAt: start 
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	^ '' findString: subString asUtf8Bytes in: bytes startingAt: start! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/6/2022 15:26:19'!
findString: key startingAt: start caseSensitive: caseSensitive
	"Answer the index in this String at which the substring key first occurs, at or beyond start.  The match can be case-sensitive or not.  If no match is found, zero will be returned."

"caseSensitive = false is very expensive. Do better."

	^caseSensitive
		ifTrue: [ self findString: key startingAt: start ]
		ifFalse: [ self asLowercase findString: key asLowercase startingAt: start ]! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/6/2022 15:06:02'!
indexOfSubCollection: subString startingAt: start ifAbsent: exceptionBlock
	| ndex |
	ndex _ '' findString: subString asUtf8Bytes in: bytes startingAt: start.
	ndex = 0 ifTrue: [^ exceptionBlock value].
	^ ndex! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 15:05:05'!
is: subString substringAt: ndex
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."
	| sequenceSize subStringBytes |
	ndex < 1 ifTrue: [ ^false ].
	sequenceSize _ subString size.
	sequenceSize = 0 ifTrue: [ ^true].
	self size - ndex + 1 < sequenceSize ifTrue: [ ^false ].
	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	subStringBytes _ subString asUtf8Bytes.
	(bytes at: ndex) = (subStringBytes at: 1) ifFalse: [ ^false ].
	^('' findString: subStringBytes in: bytes startingAt: ndex) = ndex! !


!Utf8Symbol methodsFor: 'testing' stamp: 'jmv 6/6/2022 15:47:19'!
isInitializePvtSelector
	"Answer whether the receiver is a private instance initialization message selector, that is,
	begins with 'initializePvt' (followed or not by additional stuff, as a unary message, or as keyword with arguments)"

	^ bytes beginsWith: `'initializePvt' asUtf8Bytes`! !

!Utf8Symbol methodsFor: 'testing' stamp: 'jmv 6/6/2022 15:46:51'!
isPvtSelector
	"Answer whether the receiver is a private message selector, that is,
	begins with 'pvt' followed by an uppercase letter, e.g. pvtStringhash."

	^ (bytes beginsWith: `'pvt' asUtf8Bytes` ) and: [bytes size >= 4 and: [(self at: 4) isUppercase]]! !


!String methodsFor: 'accessing' stamp: 'di 11/15/1998 16:43'!
findString: subString in: body startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	^ self findSubstring: subString in: body startingAt: start matchTable: CaseSensitiveOrder.! !

!String methodsFor: 'accessing' stamp: 'di 11/15/1998 16:43'!
findString: subString startingAt: start 
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	subString isByteString ifFalse: [ ^self asUtf8String findString: subString startingAt: start ].
	^ self findString: subString in: self startingAt: start! !

!String methodsFor: 'accessing' stamp: 'di 11/15/1998 17:02'!
indexOfSubCollection: subString startingAt: start ifAbsent: exceptionBlock
	| index |
	subString isByteString ifFalse: [ ^self asUtf8String indexOfSubCollection: subString startingAt: start ifAbsent: exceptionBlock ].
	index _ self findString: subString in: self startingAt: start.
	index = 0 ifTrue: [^ exceptionBlock value].
	^ index! !

!String methodsFor: 'comparing' stamp: 'len 11/15/2015 04:23'!
is: subString substringAt: index
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."
	| sequenceSize |
	index < 1 ifTrue: [ ^false ].
	sequenceSize _ subString size.
	sequenceSize = 0 ifTrue: [ ^true].
	subString isByteString ifFalse: [ ^self asUtf8String is: subString substringAt: index ].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].
	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	(self basicAt: index) = (subString basicAt: 1) ifFalse: [ ^false ].
	^(self findString: subString in: self startingAt: index) = index! !


!WriteStream methodsFor: 'accessing' stamp: 'jmv 6/6/2022 17:06:41'!
nextPutAllString: aString withAttributes: attributesArray

	| newEnd |
	(aString canHoldCodePoints and: [collection canHoldCodePoints not]) ifTrue: [
		collection _ collection thatCanHoldCodePoints ].
	(self isCompatibleWithContents: aString)
		ifFalse: [ ^ self nextPutAll: aString ].

	newEnd _ position + aString size.
	newEnd > writeLimit ifTrue: [
		self growTo: newEnd + 10].

	collection
		replaceFrom: position+1
		to: newEnd 
		withString: aString
		attributes: attributesArray
		startingAt: 1.
	position _ newEnd! !


!DifferenceFinder class methodsFor: 'compatibility' stamp: 'jmv 6/1/2022 11:24:19'!
wordsDisplayPatchFrom: srcString to: dstString
	| finder answer src1 dst1 changedCount |

	srcString class == dstString class ifFalse: [
		^self wordsDisplayPatchFrom: srcString asUtf8String to: dstString asUtf8String ].

	finder _ self base: srcString case: dstString.
	finder compareLines; compute.
	answer _ srcString class new asText.
	src1 _ srcString class writeStream.
	dst1 _ srcString class writeStream.
	changedCount _ 0.
	finder differences sort first do: [:item :condition |
		condition caseOf: {
			[ #unchanged ] -> [
				changedCount > 0 ifTrue: [
					"If the sequence of changed lines is large, comparing words gets too slow and less useful"
					changedCount > 30 ifTrue: [
						^nil ].
					"Compare the just ended sequence of changed lines"
					finder base: src1 contents case: dst1 contents.
					finder compareWords; compute: true.
					finder differences ifNil: [ ^nil ].
					answer _ answer append:  finder differences anyOne asText.
					src1 resetToStart.
					dst1 resetToStart.
					changedCount _ 0.
				].
				"This line hasn't changed. Just add it to the result in plain text."
				answer append: item ].
			[ #removed ] -> [
				"A removed line belongs in the source"
				src1 nextPutAll: item.
				changedCount _ changedCount + 1 ].
			[ #inserted ] -> [
				"An added line belongs in the destination"
				dst1 nextPutAll: item.
				changedCount _ changedCount + 1  ].
			}.
		].
	"If the sequence of changed lines is large, comparing words gets too slow and less useful"
	changedCount > 30 ifTrue: [
		^nil ].
	finder base: src1 contents case: dst1 contents.
	finder compareWords; compute: true.
	finder differences ifNil: [ ^nil ].
	answer _ answer append: finder differences anyOne asText.

	^answer! !

!methodRemoval: String #lastNonSeparator stamp: 'jmv 6/8/2022 12:17:37'!
String removeSelector: #lastNonSeparator!
!methodRemoval: String #withoutLeadingBlanks stamp: 'jmv 6/8/2022 12:17:52'!
String removeSelector: #withoutLeadingBlanks!
!methodRemoval: String #includesSubString: stamp: 'jmv 6/8/2022 12:10:00'!
String removeSelector: #includesSubString:!
!methodRemoval: String #afterBlanksEndsWith: stamp: 'jmv 6/8/2022 12:17:15'!
String removeSelector: #afterBlanksEndsWith:!
!methodRemoval: String #endsWith: stamp: 'jmv 6/8/2022 12:16:56'!
String removeSelector: #endsWith:!
!methodRemoval: String #withBlanksTrimmed stamp: 'jmv 6/8/2022 12:17:44'!
String removeSelector: #withBlanksTrimmed!
!methodRemoval: String #firstNonSeparator stamp: 'jmv 6/8/2022 12:17:31'!
String removeSelector: #firstNonSeparator!
!methodRemoval: String #asLowercase stamp: 'jmv 6/8/2022 12:17:23'!
String removeSelector: #asLowercase!
!methodRemoval: String #beginsWith: stamp: 'jmv 6/8/2022 12:15:21'!
String removeSelector: #beginsWith:!
