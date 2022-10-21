'From Cuis 6.0 [latest update: #5504] on 21 October 2022 at 11:12:28 am'!

!TextModel class methodsFor: 'handling of Unicode' stamp: 'jmv 10/21/2022 11:03:20'!
arrowStringForAssignment
	^ UnicodeContentsHandling = #useUtf8Editors
		ifTrue: [ "Some examples to try.
			2190 - 	Leftwards Arrow
			21D0 - 	Leftwards Double Arrow
			21E0	 - Leftwards Dashed Arrow
			21E6 - Leftwards White Arrow
			21FD	 - Leftwards Open-Headed Arrow
			27F5 - Long Leftwards Arrow
			2B05	 - Leftwards Black Arrow	"
			`(UnicodeCodePoint codePoint: 16r2190) asString` ]
		ifFalse: [ `(Character numericValue: 28) asString` ]! !

!TextModel class methodsFor: 'handling of Unicode' stamp: 'jmv 10/21/2022 11:11:30'!
arrowStringForReturn
	^ UnicodeContentsHandling = #useUtf8Editors
		ifTrue: [ "Some examples to try.
			2191 - 	Upwards Arrow
			21D1 - 	Upwards Double Arrow
			21E1	 - Upwards Dashed Arrow
			21E7 - Upwards White Arrow
			2B06	 - Upwards Black Arrow	"
			`(UnicodeCodePoint codePoint: 16r2191) asString` ]
		ifFalse: [ `(Character numericValue: 30) asString` ]! !


!CodeProvider methodsFor: 'shout styling' stamp: 'jmv 10/21/2022 11:04:23'!
normalizeCode: aTextOrString
	"Adapt edited code to be suitable for compilation or logging.
	Answer is a plain text.
	Additionally, if code was modified for better look and edition experience, normalize it.
	This may include reverting to ':=' and/or '_' for assignment, and other replacements of
	ASCII by Unicode symbols done by Shout."

	| answer |
	answer _ aTextOrString asPlainString.
	answer _ answer copyReplaceAll: TextModel arrowStringForAssignment with: '_'.
	answer _ answer copyReplaceAll: TextModel arrowStringForReturn with: '^'.
	^answer! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 10/21/2022 11:06:06'!
doItSourceCodeFor: aSourceCode in: anEvaluationContext

	^self stringClassToUse streamContents: [ :stream | | normalizedCode |
		normalizedCode _ aSourceCode asPlainString.
		normalizedCode _ normalizedCode copyReplaceAll: TextModel arrowStringForAssignment with: '_'.
		normalizedCode _ normalizedCode copyReplaceAll: TextModel arrowStringForReturn with: '^'.
		Scanner doItSourceCodeHeaderWithContext: anEvaluationContext notNil into: stream.
		stream nextPutAll: normalizedCode		]! !


!SHParserST80 methodsFor: 'token testing' stamp: 'jmv 10/21/2022 11:00:36'!
isAssignment
	^ currentToken = ':=' or: [ currentToken = '_' or: [ currentToken = TextModel arrowStringForAssignment ]]! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 10/21/2022 11:08:26'!
parseStatement
	currentTokenFirst = $^ ifTrue: [self scanPast: #return].
	currentTokenFirst = TextModel arrowStringForReturn first ifTrue: [self scanPast: #return].
	self parseExpression! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 10/21/2022 11:11:24'!
privateFormatAndConvert

	"Turn := into an arrow, only if requested"
	(Preferences at: #syntaxHighlightingAsYouTypeLeftArrowAssignment) ifTrue: [
		self replaceStringForRangesWithType: #ansiAssignment with: TextModel arrowStringForAssignment ].

	"Turn $_ either into an arrow or into := "
	(Preferences at: #syntaxHighlightingAsYouTypeAnsiAssignment)
		ifTrue: [ self replaceStringForRangesWithType: #assignment with: ':=' ]
		ifFalse: [ self replaceStringForRangesWithType: #assignment with: TextModel arrowStringForAssignment ].

	self replaceStringForRangesWithType: #return with: TextModel arrowStringForReturn.! !

!methodRemoval: TextModel class #assignmentArrowString stamp: 'jmv 10/21/2022 11:00:50'!
TextModel class removeSelector: #assignmentArrowString!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

SHTextStylerST80 initialize!

