'From Cuis 6.0 [latest update: #5238] on 8 June 2022 at 1:29:47 pm'!

!UnicodeCodePoint methodsFor: 'fileman-testing' stamp: 'jmv 6/7/2022 20:47:28'!
isDriveLetter
	^'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz' includes: self! !

!UnicodeCodePoint methodsFor: 'fileman-testing' stamp: 'jmv 6/7/2022 20:46:31'!
isDriveSeparator
	^self = $:! !

!UnicodeCodePoint methodsFor: 'fileman-testing' stamp: 'jmv 6/7/2022 20:46:25'!
isPathSeparator
	self = $\ ifTrue: [^true].
	self = $: ifTrue: [^true].
	self = $/ ifTrue: [^true].
	^false! !


!CharacterSequence methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
findDelimiters: delimiters startingAt: start
	"Answer the index of the character within the receiver, starting at start, that matches one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1."

	start to: self size do: [:i |
		delimiters do: [:delim | delim = (self at: i) ifTrue: [^ i]]].
	^ self size + 1! !

!CharacterSequence methodsFor: 'accessing' stamp: 'tk 8/10/2000 09:14'!
findTokens: delimiters
	"Answer the collection of tokens that result from parsing self.  Return strings between the delimiters.  Any character in the Collection delimiters marks a border.  Several delimiters in a row are considered as just one separation.  Also, allow delimiters to be a single character."

	| tokens keyStart keyStop separators |

	tokens _ OrderedCollection new.
	separators _ delimiters isCharacter
		ifTrue: [Array with: delimiters]
		ifFalse: [delimiters].
	keyStop _ 1.
	[keyStop <= self size] whileTrue:
		[keyStart _ self skipDelimiters: separators startingAt: keyStop.
		keyStop _ self findDelimiters: separators startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens! !

!CharacterSequence methodsFor: 'accessing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
skipDelimiters: delimiters startingAt: start
	"Answer the index of the character within the receiver, starting at start, that does NOT match one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1.  Assumes the delimiters to be a non-empty string."

	start to: self size do: [:i |
		delimiters detect: [:delim | delim = (self at: i)]
				ifNone: [^ i]].
	^ self size + 1! !

!CharacterSequence methodsFor: 'fileman-converting' stamp: 'mu 3/4/2007 11:04'!
asPathComponents

	| tokens |
	self isRelativePathName ifTrue: [self error: 'relative form is invaild!!'].
	tokens _ self asPathTokens.
	^tokens! !

!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 6/6/2015 23:56'!
asPathTokens

	^self beginsWithWindowsDriveName
		ifTrue: [
			(OrderedCollection with: (self copyFrom: 1 to: 2)), 
				((self copyFrom: 3 to: self size) findTokens: String pathSeparators) ]
		ifFalse: [
			self findTokens: String pathSeparators ]! !

!CharacterSequence methodsFor: 'fileman-testing' stamp: 'jmv 9/17/2015 10:36'!
beginsWithWindowsDriveName
	"Answer doesn't depend on running on Windows or not"
	self size < 2 ifTrue: [ ^false ].
	^self first isDriveLetter
		and: [ self second isDriveSeparator
			and: [ self size = 2 or: [ self third isPathSeparator ]]]! !

!CharacterSequence methodsFor: 'fileman-testing' stamp: 'mu 3/2/2007 22:05'!
isPathSeparator
	self = '\' ifTrue: [^true].
	self = ':' ifTrue: [^true].
	self = '/' ifTrue: [^true].
	^false! !

!CharacterSequence methodsFor: 'fileman-testing' stamp: 'mu 6/11/2006 22:41'!
isRelativeMark

	self = '.' ifTrue: [^true].
	self = '..' ifTrue: [^true].

	^false! !

!CharacterSequence methodsFor: 'fileman-testing' stamp: 'mu 3/4/2007 10:46'!
isRelativePathName
	self ifEmpty: [^false].
	self isRelativeMark ifTrue: [^true].
	^#('.' '..') includes: self upToFirstPathSeparator! !

!CharacterSequence methodsFor: 'fileman-accessing' stamp: 'jmv 4/2/2016 23:07'!
indexOfFirstPathSeparator
	self
		withIndexDo: [:each :idx | each isPathSeparator
				ifTrue: [^ idx]].
	^ 0! !

!CharacterSequence methodsFor: 'fileman-accessing' stamp: 'mu 3/4/2007 10:45'!
upToFirstPathSeparator
	| index |
	self ifEmpty: [^self].
	index _ self indexOfFirstPathSeparator.
	index = 0 ifTrue: [^self].
	^self copyFrom: 1 to: index - 1! !


!String methodsFor: 'converting' stamp: 'jmv 6/7/2022 20:26:14'!
substrings: aString
	"Answer an array of the substrings that compose the receiver."
	| end beginning |
	end _ 0.
	^Array streamContents: [ :result |
		"find one substring each time through this loop"
		[ 
			"find the beginning of the next substring"
			beginning _ aString indexOfAnyOf: CSNonSeparators startingAt: end+1 ifAbsent: nil.
			beginning notNil ] 
		whileTrue: [
			"find the end"
			end _ aString indexOfAnyOf: CSSeparators startingAt: beginning ifAbsent: [ aString size + 1 ].
			end _ end - 1.
			result nextPut: (aString copyFrom: beginning to: end).
		]
	].! !


!Utf8String methodsFor: 'converting' stamp: 'jmv 6/7/2022 20:34:31'!
substrings
	"
	'  Hello World!! Some  		1234 more  ' substrings
	'  Hello World!! Some  		1234 more  ' asUtf8String substrings
	"
	^ ('' substrings: bytes) collect: [ :eachBytes | Utf8String withBytes: eachBytes ].! !


!ByteArray methodsFor: 'accessing' stamp: 'jmv 6/7/2022 20:33:24'!
indexOfAnyOf: aCharacterSet  startingAt: start ifAbsent: aBlock
	"returns the index of the first character in the given set, starting from start"

	| answer |
	answer _ String findFirstInString: self inSet: aCharacterSet byteArrayMap startingAt: start.

	^answer = 0 
		ifTrue: [ aBlock value ]
		ifFalse: [ answer]! !


!String methodsFor: 'converting' stamp: 'jmv 6/7/2022 20:26:38'!
substrings
	"Answer an array of the substrings that compose the receiver."
	^self substrings: self! !


!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 6/8/2022 12:35:38'!
withBytes: aByteArray

	^self basicNew bytes: aByteArray ! !


!DifferenceFinder class methodsFor: 'compatibility' stamp: 'jmv 6/8/2022 13:17:15'!
wordsDisplayPatchFrom: srcStringOrText to: dstStringOrText

	| finder answer src1 dst1 changedCount dstString srcString |
	srcString _ srcStringOrText asPlainString.
	dstString _ dstStringOrText asPlainString.
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

!methodRemoval: String #asPathComponents stamp: 'jmv 6/8/2022 13:21:09'!
String removeSelector: #asPathComponents!
!methodRemoval: String #indexOfFirstPathSeparator stamp: 'jmv 6/8/2022 13:24:34'!
String removeSelector: #indexOfFirstPathSeparator!
!methodRemoval: String #isRelativeMark stamp: 'jmv 6/8/2022 13:22:13'!
String removeSelector: #isRelativeMark!
!methodRemoval: String #skipDelimiters:startingAt: stamp: 'jmv 6/8/2022 13:20:09'!
String removeSelector: #skipDelimiters:startingAt:!
!methodRemoval: String #asPathTokens stamp: 'jmv 6/8/2022 13:27:21'!
String removeSelector: #asPathTokens!
!methodRemoval: String #beginsWithWindowsDriveName stamp: 'jmv 6/8/2022 13:26:46'!
String removeSelector: #beginsWithWindowsDriveName!
!methodRemoval: String #isRelativePathName stamp: 'jmv 6/8/2022 13:25:20'!
String removeSelector: #isRelativePathName!
!methodRemoval: String #upToFirstPathSeparator stamp: 'jmv 6/8/2022 13:23:42'!
String removeSelector: #upToFirstPathSeparator!
!methodRemoval: String #findTokens: stamp: 'jmv 6/8/2022 13:18:56'!
String removeSelector: #findTokens:!
!methodRemoval: String #isPathSeparator stamp: 'jmv 6/8/2022 13:26:05'!
String removeSelector: #isPathSeparator!
!methodRemoval: String #findDelimiters:startingAt: stamp: 'jmv 6/8/2022 12:32:58'!
String removeSelector: #findDelimiters:startingAt:!
