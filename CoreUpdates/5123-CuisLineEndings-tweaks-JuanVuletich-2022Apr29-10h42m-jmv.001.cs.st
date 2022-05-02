'From Cuis 6.0 [latest update: #5118] on 29 April 2022 at 10:44:57 am'!

!String methodsFor: 'converting' stamp: 'jmv 4/29/2022 09:52:23'!
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


!Text methodsFor: 'converting' stamp: 'jmv 4/29/2022 10:01:15'!
fixLineEndings
	"Answer a copy of myself in which all sequences of <CR><LF> or <CF> have been changed to <LF>.
	If possible, modify and answer the same instance. Create a new instance only if needed.
	Be quick and don't use extra memory if already using Character lf!!
	('Hello ', String crlfString, 'World') asText fixLineEndings
	('Hello ', String crString, 'World') asText fixLineEndings
	('Hello ', String lfString, 'World') asText fixLineEndings
	"

	| newText |
	(string includes: `Character cr`) ifFalse: [ ^self ].
	newText _ self.
	(string includesSubString: `String crlfString`) ifTrue: [ "A new instance is needed"
		newText _ self copyReplaceAll: `String crlfString` with: `String newLineString` asTokens: false ].
	newText string replaceAll: `Character cr` with: `Character lf`.
	^newText! !


!String methodsFor: 'converting' stamp: 'jmv 4/29/2022 10:07:10'!
withCuisLineEndings
	"Assume that CR, LF, and CRLF are all valid line endings.
	Answer a new instance, replacing each occurence with a single Lf.
	('aLine', String crlfString, 'anotherOne') withCuisLineEndings
	"

	^ self withLineEndings: `String newLineString`.! !


!Text methodsFor: 'converting' stamp: 'jmv 4/29/2022 10:07:03'!
withCuisLineEndings
	"Answer a copy of myself in which all sequences of <CR><LF> or <CF> have been changed to <LF>"

	| newText wrongLineEnd |
	wrongLineEnd _ `String crlfString detect: [ :char | (char = Character newLineCharacter) not ]`.
	(string includes: wrongLineEnd) ifFalse: [ ^self copy ].
	newText _ self copyReplaceAll: `String crlfString` with: `String newLineString` asTokens: false.
	newText string replaceAll: wrongLineEnd with: `Character newLineCharacter`.
	^newText! !

