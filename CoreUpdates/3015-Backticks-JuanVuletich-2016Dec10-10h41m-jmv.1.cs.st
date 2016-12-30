'From Cuis 5.0 of 7 November 2016 [latest update: #3000] on 10 December 2016 at 10:41:46 am'!

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 12/10/2016 01:38:21'!
xBacktick
	"Smalltalk code evaluated at compile time as a literal."

	self step.
	buffer reset.
	[hereChar == $`
		and: [aheadChar == $`
				ifTrue: [self step. false]
				ifFalse: [true]]]
		whileFalse: [
			buffer nextPut: self step.
			(hereChar = Scanner doItCharacterValue asCharacter and: [source atEnd])
				ifTrue: [^self offEnd: 'Unmatched back quote']].
	self step.
	token _ Compiler evaluate: buffer contents.
	tokenType _ #literal! !


!Character methodsFor: 'testing' stamp: 'jmv 12/10/2016 01:26:44'!
isValidInIdentifiers
	"Can c be part of an identifier? (unary or keyword selector, or variable name)"

	^self isAlphaNumeric or: [ #( $_ ) statePointsTo: self ]! !


!CompiledMethod methodsFor: 'comparing' stamp: 'jmv 12/10/2016 01:27:06'!
= method
	| numLits lit1 lit2 |

	"Any object is equal to itself"
	self == method ifTrue: [ ^ true ].

	"Answer whether the receiver implements the same code as the 
	argument, method."
	(method is: #CompiledMethod) ifFalse: [ ^false ].
	self size = method size ifFalse: [ ^false ].
	self header = method header ifFalse: [ ^false ].
	self initialPC to: self endPC do: [ :i |
		(self at: i) = (method at: i) ifFalse: [ ^false ]].
	(numLits _ self numLiterals) ~= method numLiterals ifTrue: [ ^false ].

	"Dont bother checking FFI and named primitives''
	jmv: Does this make any sense?
	 (#(117 120) includes: self primitive) ifTrue: [^ true]."

	 "properties"
	(self properties analogousCodeTo: method properties) ifFalse: [
		^false ].

	"#penultimateLiteral is selector (or properties, just compared, above)
	Last literal is #methodClass.
	Don't compare them. Two methods might be equal even if they have different selector (or none at all)
	or are installed in different classes (or none at all)"
	1 to: numLits-2 do: [ :i |
		lit1 _ self literalAt: i.
		lit2 _ method literalAt: i.
		lit1 = lit2 ifFalse: [
			(i = 1 and: [ #(117 120) includes: self primitive ])
				ifTrue: [
					lit1 isArray
						ifTrue: [
							(lit2 isArray and: [ lit1 allButLast = lit2 allButLast ]) ifFalse: [
								^false ]]
						ifFalse: [ "ExternalLibraryFunction"
							(lit1 analogousCodeTo: lit2) ifFalse: [
								^false ]]]
				ifFalse: [
					 lit1 isFloat
						ifTrue: [
							"Floats match if values are close, due to roundoff error."
							(lit1 closeTo: lit2) ifFalse: [ ^false ].
							self flag: 'just checking'. self halt ]
						ifFalse: [
							"any other discrepancy is a failure"
							^ false ]]]].
	^true! !


!Scanner class methodsFor: 'cached class state' stamp: 'jmv 12/10/2016 01:26:17'!
initTypeTable
	| newTable |
	newTable := Array new: 256 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"

	1 to: 255
		do: [:index |
			(Character numericValue: index) isValidInIdentifiers
				ifTrue: [
					"Digits and $_ are overwritten later"
					newTable at: index put: #xLetter ]].

	newTable atAll: ($0 numericValue to: $9 numericValue) put: #xDigit.
	newTable atAll:  '!!%&+-*/\±◊˜¨≠Ø,<=>îïñóòôöõúù´ª?@~ÄÅÇÉéèêëíìûü∞∑' asByteArray put: #xBinary.

	newTable at: Scanner doItCharacterValue put: #doIt.
	newTable at: $" numericValue put: #xDoubleQuote.
	newTable at: $# numericValue put: #xLitQuote.
	newTable at: $$ numericValue put: #xDollar.
	newTable at: $' numericValue put: #xSingleQuote.
	newTable at: $` numericValue put: #xBacktick.
	newTable at: $: numericValue put: #xColon.
	newTable at: $( numericValue put: #leftParenthesis.
	newTable at: $) numericValue put: #rightParenthesis.
	newTable at: $. numericValue put: #period.
	newTable at: $; numericValue put: #semicolon.
	newTable at: $[ numericValue put: #leftBracket.
	newTable at: $] numericValue put: #rightBracket.
	newTable at: ${ numericValue put: #leftBrace.
	newTable at: $} numericValue put: #rightBrace.
	newTable at: $^ numericValue put: #upArrow.
	newTable at: $_ numericValue put: #xUnderscore.
	newTable at: $| numericValue put: #verticalBar.
	TypeTable := newTable "bon voyage!!"

	"
	Scanner initTypeTable
	"! !


!SHParserST80 methodsFor: 'scan' stamp: 'jmv 12/10/2016 01:42:02'!
scanWhitespace
	| c |
	
	[c := self currentChar.
	c notNil and: [c isSeparator or: [c == $`]]] 
		whileTrue: [sourcePosition := sourcePosition + 1].
	c == $" ifTrue: [self scanComment]! !

!SHParserST80 methodsFor: 'character testing' stamp: 'jmv 12/10/2016 10:24:38'!
isBinarySelectorCharacter: aCharacter

	aCharacter isValidInIdentifiers ifTrue: [^false].
	aCharacter isSeparator ifTrue: [^false].

	('"#$'':().;[]{}_`'  includes: aCharacter) 
		ifTrue:[^false].
	aCharacter numericValue = Scanner doItCharacterValue ifTrue: [^false "the doIt char"].
	aCharacter numericValue = 0 ifTrue: [^false].
	"Any other char is ok as a binary selector char."
	^true! !

!methodRemoval: Scanner #scanStringStruct!
Scanner removeSelector: #scanStringStruct!
!methodRemoval: Scanner #scanStringStruct:!
Scanner removeSelector: #scanStringStruct:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Scanner initTypeTable!

