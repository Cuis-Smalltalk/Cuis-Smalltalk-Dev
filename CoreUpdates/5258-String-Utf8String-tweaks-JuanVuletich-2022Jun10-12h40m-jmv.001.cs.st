'From Cuis 6.0 [latest update: #5258] on 10 June 2022 at 12:55:45 pm'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/10/2022 12:21:38'!
asUtf8BytesOrByteString
	"Similar to #asUtf8Bytes.
	May answer an instance of String as an optimization.
	See senders"
	^self asUtf8Bytes! !


!String methodsFor: 'converting' stamp: 'jmv 6/10/2022 12:21:55'!
asUtf8BytesOrByteString
	"Similar to #asUtf8Bytes.
	May answer an instance of String as an optimization.
	See senders"

	| inStream nextChar codePoint |
	"Fast lane."
	self isAscii ifTrue: [ ^self ].

	"Need to do UTF-8 encoding."
	^ByteArray streamContents: [ :outStream |
		inStream _ self readStream.
		[ inStream atEnd ] whileFalse: [
			nextChar _ inStream next.
			codePoint _ nextChar codePoint.
			Character
				evaluate: [ :byte | outStream nextPut: byte ]
				withUtf8BytesOfUnicodeCodePoint: codePoint ]]! !


!String methodsFor: 'comparing' stamp: 'jmv 6/10/2022 12:05:07'!
< aString
	"Answer whether the receiver sorts before aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString > self ].

	^ (self compare: self with: aString collated: CaseSensitiveOrder) = 1! !

!String methodsFor: 'comparing' stamp: 'jmv 6/10/2022 12:05:43'!
<= aString
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString >= self ].

	^ (self compare: self with: aString collated: CaseSensitiveOrder) <= 2! !

!String methodsFor: 'comparing' stamp: 'jmv 6/10/2022 12:05:20'!
> aString
	"Answer whether the receiver sorts after aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString < self ].

	^ (self compare: self with: aString collated: CaseSensitiveOrder) = 3! !

!String methodsFor: 'comparing' stamp: 'jmv 6/10/2022 12:05:32'!
>= aString
	"Answer whether the receiver sorts after or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString <= self ].

	^ (self compare: self with: aString collated: CaseSensitiveOrder) >= 2! !


!Utf32String methodsFor: 'comparing' stamp: 'jmv 6/10/2022 12:54:15'!
= aString
	"Answer whether the receiver sorts equally as anUtf32String.
	This means same characters in same order."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].

	self hash = aString hash ifFalse: [
		^false ].

	"Expensive. Assume it is extremely unlikely."
self flag: #slowUnicode.
	aString isUtf32String ifFalse: [
		^self asUtf8String = aString ].

	^ codePoints = aString words! !


!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/10/2022 12:26:56'!
findString: subString startingAt: start 
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	^ '' findString: subString asUtf8BytesOrByteString in: bytes startingAt: start! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/10/2022 12:48:18'!
findString: key startingAt: start caseSensitive: caseSensitive
	"Answer the index in this String at which the substring key first occurs, at or beyond start.  The match can be case-sensitive or not.  If no match is found, zero will be returned."

"caseSensitive = false is very expensive. Do better."
self flag: #slowUnicode.

	^caseSensitive
		ifTrue: [ self findString: key startingAt: start ]
		ifFalse: [ self asLowercase findString: key asLowercase startingAt: start ]! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/10/2022 12:27:05'!
indexOfSubCollection: subString startingAt: start ifAbsent: exceptionBlock
	| index |
	index _ '' findString: subString asUtf8BytesOrByteString in: bytes startingAt: start.
	index = 0 ifTrue: [^ exceptionBlock value].
	^ index! !

!Utf8String methodsFor: 'copying' stamp: 'jmv 6/10/2022 12:22:26'!
, aString
	^Utf8String fromUtf8Bytes: bytes, aString asUtf8BytesOrByteString codePointCount: self size + aString size.! !

!Utf8String methodsFor: 'copying' stamp: 'jmv 6/10/2022 12:25:40'!
copyReplaceFrom: startIndex to: endIndex with: replacement
	"Answer a copy of the receiver satisfying the following conditions: 
	+ stop is less than start, then this is an insertion; stop should be exactly start-1, 
	+ start = 1 means insert before the first character,
	+ start = size+1 means append after last character. 
	+ Otherwise, this is a replacement; start and stop have to be within the receiver's bounds."

	| epilogFirstIndex prologLastIndex newBytes replacementBytes |
	prologLastIndex _ (self byteIndexAt: startIndex)-1.
	epilogFirstIndex _ self byteIndexAt: endIndex+1.
	replacementBytes _ replacement asUtf8BytesOrByteString.
	newBytes _ (bytes copyFrom: 1 to: prologLastIndex),
		replacementBytes,
		(bytes
			copyFrom: epilogFirstIndex
			to: bytes size).
	^Utf8String fromUtf8Bytes: newBytes codePointCount: (startIndex-1) + replacement size + (self size-endIndex).! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/10/2022 12:22:34'!
< aString
	"Answer whether the receiver sorts before aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	^ ('' compare: bytes with: aString asUtf8BytesOrByteString) = 1! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/10/2022 12:23:07'!
<= aString
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	^ ('' compare: bytes with: aString asUtf8BytesOrByteString) <= 2! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/10/2022 12:24:20'!
= aString
	"Answer whether the receiver is equal to aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, Utf8String, Utf8Symbol or Utf32String."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].

	self hash = aString hash ifFalse: [
		^false ].

	(self isAscii and: [	aString isByteString ]) ifTrue: [
		"Fast lane."
		^'' is: bytes sameAs: aString ].

	"May need to do UTF-8 encoding."
	^ '' is: bytes sameAs: aString asUtf8Bytes.! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/10/2022 12:24:26'!
> aString
	"Answer whether the receiver sorts after aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	^ ('' compare: bytes with: aString asUtf8BytesOrByteString) = 3! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/10/2022 12:24:29'!
>= aString
	"Answer whether the receiver sorts after or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	^ ('' compare: bytes with: aString asUtf8BytesOrByteString) >= 2! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/10/2022 12:48:25'!
sameAs: aString
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

self flag: #slowUnicode.
	^self asLowercase = aString asLowercase! !

