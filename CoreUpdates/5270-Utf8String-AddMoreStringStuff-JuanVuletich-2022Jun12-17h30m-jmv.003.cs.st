'From Cuis 6.0 [latest update: #5267] on 12 June 2022 at 6:07:25 pm'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/12/2022 17:41:46'!
fixLineEndings
	"Assume that CR, LF, and CRLF are all valid line endings. Replace each occurence with a single Lf.
	If possible, modify and answer the same instance. Create a new instance only if needed.
	Be quick and don't use extra memory if already using Character lf!!
	('Hello ', String crlfString, 'World') fixLineEndings
	('Hello ', String crString, 'World') fixLineEndings
	('Hello ', String lfString, 'World') fixLineEndings
	('Hello ' asUtf8String, String crlfString, 'World') fixLineEndings
	('Hello ' asUtf8String, String crString, 'World') fixLineEndings
	('Hello ' asUtf8String, String lfString, 'World') fixLineEndings
	"
	(self includes: `Character cr`) ifFalse: [ ^self ].
	^ self withLineEndings: `String newLineString`! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/12/2022 18:01:37'!
withBlanksCondensed
	"Return a copy of the receiver with leading/trailing blanks removed
	 and consecutive white spaces condensed as a single space."

	| trimmed lastWasBlank |
	trimmed _ self withBlanksTrimmed.
	^self class streamContents: [ :stream |
		lastWasBlank _ false.
		trimmed do: [ :c |
			c isSeparator
				ifTrue: [ lastWasBlank ifFalse: [ stream space ]]
				ifFalse: [ stream nextPut: c ].
			lastWasBlank _ c isSeparator ]].

	"
	' abc  d   ' withBlanksCondensed
	' abc  d   ' asUtf8String withBlanksCondensed
	' abc  d
	s
	as   zz 	q 			q' withBlanksCondensed
	' abc  d
	s
	as   zz 	q 			q' asUtf8String withBlanksCondensed
	"! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/12/2022 17:36:52'!
withCuisLineEndings
	"Assume that CR, LF, and CRLF are all valid line endings.
	Answer a new instance, replacing each occurence with a single Lf.
	('aLine', String crlfString, 'anotherOne') withCuisLineEndings
	('aLine' asUtf8String, String crlfString, 'anotherOne') withCuisLineEndings
	"
	^ self withLineEndings: `String newLineString`.! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/12/2022 17:31:05'!
withDescriptiveLineEndings
	"
	('Hello', String newLineString, 'World') withDescriptiveLineEndings
	('Hello' asUtf8String, String newLineString, 'World') withDescriptiveLineEndings
	"
	^ self withLineEndings: '[enter]'! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/12/2022 17:58:20'!
withoutSeparators
	
	^self reject: [ :aCharacter | aCharacter isSeparator ]

	"
	' abc  d   ' withoutSeparators
	' abc  d   ' asUtf8String withoutSeparators
	"! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/12/2022 17:56:59'!
withoutTrailingBlanks
	"Return a copy of the receiver from which trailing blanks have been trimmed."

	| last |
	
	last _ self lastNonSeparator.
	last = 0 ifTrue: [^ ''].  "no non-separator character"
	last = self size ifTrue: [ ^self ].
	
	^ self copyFrom: 1 to: last

	"
	' abc  d   ' withoutTrailingBlanks
	' abc  d   ' asUtf8String withoutTrailingBlanks
	"
! !

!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 6/12/2022 17:59:27'!
withoutWindowsDriveName
	^self beginsWithWindowsDriveName 
		ifFalse: [ self ]
		ifTrue: [ self copyFrom: 3 to: self size ]! !


!String methodsFor: 'converting' stamp: 'jmv 6/12/2022 17:39:24'!
fixLineEndings
	"Assume that CR, LF, and CRLF are all valid line endings. Replace each occurence with a single Lf.
	If possible, modify and answer the same instance. Create a new instance only if needed.
	Be quick and don't use extra memory if already using Character lf!!
	('Hello ', String crlfString, 'World') fixLineEndings
	('Hello ', String crString, 'World') fixLineEndings
	('Hello ', String lfString, 'World') fixLineEndings
	"
	(self includes: `Character cr`) ifFalse: [ ^self ].
	(self includesSubString: `String crlfString`) ifTrue: [ "A new instance is needed"
		^ self withLineEndings: `String newLineString` ].
	self replaceAll: `Character cr` with: `Character lf`.! !


!Utf8String methodsFor: 'converting' stamp: 'jmv 6/12/2022 17:36:10'!
withLineEndings: lineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"

	| correctedBytes |
	correctedBytes _ '' string: bytes withLineEndings: lineEndingString asByteArray.
	"The codePointCount trick can be done because the line endings considered are all single byte UTF-8.
	If we ever consider unicode line endings taking more than one byte, this is no longer valid (pass nil instead)."
	^  Utf8String fromUtf8Bytes: correctedBytes codePointCount: self size - bytes size + correctedBytes size.! !

!methodRemoval: String #withBlanksCondensed stamp: 'jmv 6/12/2022 18:00:55'!
String removeSelector: #withBlanksCondensed!
!methodRemoval: String #withoutSeparators stamp: 'jmv 6/12/2022 17:58:20'!
String removeSelector: #withoutSeparators!
!methodRemoval: String #withCuisLineEndings stamp: 'jmv 6/12/2022 17:30:39'!
String removeSelector: #withCuisLineEndings!
!methodRemoval: String #withoutTrailingBlanks stamp: 'jmv 6/12/2022 17:56:31'!
String removeSelector: #withoutTrailingBlanks!
!methodRemoval: String #withoutWindowsDriveName stamp: 'jmv 6/12/2022 17:59:27'!
String removeSelector: #withoutWindowsDriveName!
!methodRemoval: String #withDescriptiveLineEndings stamp: 'jmv 6/12/2022 17:30:46'!
String removeSelector: #withDescriptiveLineEndings!
