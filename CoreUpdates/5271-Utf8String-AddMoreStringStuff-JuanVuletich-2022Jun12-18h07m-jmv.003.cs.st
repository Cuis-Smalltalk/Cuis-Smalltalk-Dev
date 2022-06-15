'From Cuis 6.0 [latest update: #5267] on 12 June 2022 at 7:11:56 pm'!

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/12/2022 18:09:59'!
isAccented
	"Answer whether the receiver is a letter."
self flag: #unicodeNeedsImprovement.
	self asCharacter ifNotNil: [ :ch | ^ch isAccented ].
	^false! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/12/2022 18:21:34'!
isLowercase
	"Answer whether the receiver is a letter."
self flag: #unicodeNeedsImprovement.
	self asCharacter ifNotNil: [ :ch | ^ch isLowercase ].
	^false! !


!CharacterSequence methodsFor: 'converting' stamp: 'jmv 5/12/2017 11:24:09'!
asMonth
	"Many allowed forms, see Month>>#readFrom:
		'July 1998' asMonth.
		'July 1998' asUtf8String asMonth.
		'1998/7' asMonth.
		'1998/7' asUtf8String asMonth.
	"

	^ Month fromString: self! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/2/2011 08:51'!
asNumber 
	"Answer the Number created by interpreting the receiver as the string 
	representation of a number."
	^Number readFrom: self readStream! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/12/2022 18:10:15'!
asUnaccented
	"
	'Hello' asUnaccented
	'Hello' asUtf8String asUnaccented
	'αιξτσό' asUnaccented
	'αιξτσό' asUtf8String asUnaccented
	"
	^(self anySatisfy: [ :c | c isAccented])
		ifFalse: [ self ]
		ifTrue: [ self collect: [ :c | c asUnaccented ]]! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 5/12/2017 16:39:13'!
asWeek
	"
		'2008-W52' asUtf8String asWeek.
		'2008-W53' asUtf8String asWeek. 'Invalid format!!'.
		'2009-W01' asUtf8String asWeek
		'2009-W02' asUtf8String asWeek
		'2009-W53' asUtf8String asWeek
		'2010-W01' asUtf8String asWeek
		'2010-W02' asUtf8String asWeek
		'2008-W52' asUtf8String asWeek start.
		'2009-W01' asUtf8String asWeek start
		'2009-W02' asUtf8String asWeek start
		'2009-W53' asUtf8String asWeek start
		'2010-W01' asUtf8String asWeek start
		'2010-W02' asUtf8String asWeek start
	"

	^ Week fromString: self! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 5/12/2017 16:37:23'!
asYear
	"
		'2008' asUtf8String asYear.
		'2008' asUtf8String asYear start.
	"

	^ Year fromString: self! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/12/2022 18:24:05'!
capitalized
	"Answer an object like the receiver but with first character downshifted if necesary
	'mElViN' asUtf8String capitalized
	'MElViN' asUtf8String capitalized
	('will', Random next mantissaPart printString) asUtf8String asSymbol capitalized
	('Will', Random next mantissaPart printString) asUtf8String asSymbol capitalized
	"
	self isEmpty ifTrue: [^ self].
	self first isUppercase ifTrue: [ ^self ].
	^self class streamContents: [ :strm | | isFirst corrected |
		isFirst _ true.
		self do: [ :codePoint |
			isFirst
				ifTrue: [
					corrected _ codePoint asUppercase.
					isFirst _ false ]
				ifFalse: [corrected _ codePoint].
			strm nextPut: corrected ]]! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/12/2022 19:09:42'!
findPositiveInteger 
	"Answer the Integer created by interpreting the receiver as the string representation of an integer.
	Answer nil if no digits, else find the first digit and then all consecutive digits after that"

	| startPosition tail endPosition |
	startPosition _ self findFirst: [:ch | ch isDigit].
	startPosition = 0 ifTrue: [^ nil].
	tail _ self copyFrom: startPosition to: self size.
	endPosition _ tail findFirst: [:ch | ch isDigit not].
	endPosition = 0 ifTrue: [endPosition _ tail size + 1].
	^ Number readFrom: (tail copyFrom: 1 to: endPosition - 1) readStream

"
'1796exportFixes-tkMX' findPositiveInteger
'1796exportFixes-tkMX' asUtf8String findPositiveInteger
'1848recentLogFile-sw' findPositiveInteger
'1848recentLogFile-sw' asUtf8String findPositiveInteger
'donald' findPositiveInteger
'donald' asUtf8String findPositiveInteger
'abc234def567' findPositiveInteger
'abc234def567' asUtf8String findPositiveInteger
"! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/12/2022 19:11:26'!
initialIntegerOrNil
	"Answer the integer represented by the leading digits of the receiver, or nil if the receiver does not begin with a digit"
	| firstNonDigit |
	(self isEmpty or: [self first isDigit not]) ifTrue: [^ nil].
	firstNonDigit _ (self findFirst: [:m | m isDigit not]).
	firstNonDigit = 0 ifTrue: [firstNonDigit _ self size + 1].
	^ (self copyFrom: 1  to: (firstNonDigit - 1)) asNumber
"
'234Whoopie' initialIntegerOrNil
'234Whoopie' asUtf8String initialIntegerOrNil
'wimpy' initialIntegerOrNil
'wimpy' asUtf8String initialIntegerOrNil
'234' initialIntegerOrNil
'234' asUtf8String initialIntegerOrNil
'2N' initialIntegerOrNil
'2N' asUtf8String initialIntegerOrNil
'2' initialIntegerOrNil
'2' asUtf8String initialIntegerOrNil
'  89Ten ' initialIntegerOrNil
'  89Ten ' asUtf8String initialIntegerOrNil
'78 92' initialIntegerOrNil
'78 92' asUtf8String initialIntegerOrNil
"
! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/12/2022 18:18:59'!
uncapitalized
	"Answer an object like the receiver but with first character downshifted if necesary
	'MElViN' asUtf8String uncapitalized
	'mElViN' asUtf8String uncapitalized
	('Will', Random next mantissaPart printString) asUtf8String asSymbol uncapitalized
	('will', Random next mantissaPart printString) asUtf8String asSymbol uncapitalized
	"
	self isEmpty ifTrue: [^ self].
	self first isLowercase ifTrue: [ ^self ].
	^self class streamContents: [ :strm | | isFirst corrected |
		isFirst _ true.
		self do: [ :codePoint |
			isFirst
				ifTrue: [
					corrected _ codePoint asLowercase.
					isFirst _ false ]
				ifFalse: [corrected _ codePoint].
			strm nextPut: corrected ]]! !

!CharacterSequence methodsFor: 'testing' stamp: 'len 11/26/2016 09:05:35'!
isAlphaNumeric
	"Answer true if the receiver contains only letters or digits."
	^ self allSatisfy: [:each| each isAlphaNumeric]! !


!Utf8Symbol methodsFor: 'converting' stamp: 'jmv 6/12/2022 18:11:42'!
capitalized
	^ self asPlainString capitalized asSymbol! !

!Utf8Symbol methodsFor: 'converting' stamp: 'jmv 6/12/2022 18:11:29'!
uncapitalized
	^ self asPlainString uncapitalized asSymbol! !


!String methodsFor: 'converting' stamp: 'jmv 6/12/2022 18:19:34'!
uncapitalized
	"Answer an object like the receiver but with first character downshifted if necesary
	'MElViN' uncapitalized
	'mElViN' uncapitalized
	('Will', Random next mantissaPart printString) asSymbol uncapitalized
	('will', Random next mantissaPart printString) asSymbol uncapitalized
	"
	| answer |
	self isEmpty ifTrue: [^ self].
	self first isLowercase ifTrue: [ ^self ].
	answer _ self asPlainString.
	answer == self ifTrue: ["don't modify receiver"
		answer _ self copy ].
	answer at: 1 put: (answer at: 1) asLowercase.
	^ answer! !

!methodRemoval: String #findPositiveInteger stamp: 'jmv 6/12/2022 19:09:03'!
String removeSelector: #findPositiveInteger!
!methodRemoval: String #asNumber stamp: 'jmv 6/12/2022 19:08:18'!
String removeSelector: #asNumber!
!methodRemoval: String #asUnaccented stamp: 'jmv 6/12/2022 18:08:51'!
String removeSelector: #asUnaccented!
!methodRemoval: String #asYear stamp: 'jmv 6/12/2022 19:06:14'!
String removeSelector: #asYear!
!methodRemoval: String #isAlphaNumeric stamp: 'jmv 6/12/2022 18:58:49'!
String removeSelector: #isAlphaNumeric!
!methodRemoval: String #initialIntegerOrNil stamp: 'jmv 6/12/2022 19:10:46'!
String removeSelector: #initialIntegerOrNil!
!methodRemoval: String #asMonth stamp: 'jmv 6/12/2022 19:01:36'!
String removeSelector: #asMonth!
!methodRemoval: String #asWeek stamp: 'jmv 6/12/2022 19:06:42'!
String removeSelector: #asWeek!
