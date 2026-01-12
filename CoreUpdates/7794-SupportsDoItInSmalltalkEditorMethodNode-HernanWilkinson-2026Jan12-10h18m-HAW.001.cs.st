'From Cuis7.7 [latest update: #7793] on 12 January 2026 at 10:23:10 am'!

!Parser methodsFor: 'private' stamp: 'HAW 9/Jan/2026 12:33:44'!
messagePatternExpectedErrorMessage

	^ self class messagePatternExpectedErrorMessage! !

!Parser methodsFor: 'error handling' stamp: 'HAW 9/Jan/2026 12:16:25'!
expectedErrorMessageFor: aString

	^ self class expectedErrorMessageFor: aString ! !


!Parser class methodsFor: 'error messages' stamp: 'HAW 9/Jan/2026 12:16:09'!
expectedErrorMessageFor: aString

	^ aString , ' expected'! !

!Parser class methodsFor: 'error messages' stamp: 'HAW 9/Jan/2026 12:33:44'!
messagePatternExpectedErrorMessage

	^ 'Message pattern'! !


!Parser methodsFor: 'private' stamp: 'HAW 9/Jan/2026 12:33:44'!
privateReadSelector
	| args selector |
	doitFlag := false.

	hereType == #word ifTrue: [
		^ here asSymbol ].

	self transformVerticalBarAndUpArrowIntoABinarySelector.

	hereType == #binary ifTrue: [
		^ here asSymbol ].

	hereType == #keyword ifTrue: [
		selector := source collectionSpecies writeStream.
		args := OrderedCollection new.
		[hereType == #keyword] whileTrue: [
			selector nextPutAll: self advance.
			args addLast: (encoder bindArg: self argumentName).
		].
		^ selector contents asSymbol ].

	^self expected: self messagePatternExpectedErrorMessage! !

!Parser methodsFor: 'error handling' stamp: 'HAW 9/Jan/2026 12:15:50'!
expected: aString 
	"Notify a problem at token 'here'."

	tokenType == #doIt ifTrue: [hereMark := hereMark + 1].
	hereType == #doIt ifTrue: [hereMark := hereMark + 1].
	^ self notify: (self expectedErrorMessageFor: aString) at: hereMark + requestorOffset! !

!Parser methodsFor: 'expression types' stamp: 'HAW 9/Jan/2026 12:33:44'!
pattern: fromDoit inContext: ctxt
	" unarySelector | binarySelector arg | keyword arg {keyword arg} =>
	{selector, arguments, precedence}."
	
	doitFlag := fromDoit.
	
	fromDoit ifTrue: [^self doitPatternInContext: ctxt ].
	hereType == #word ifTrue: [^self unaryPattern ].
	self transformVerticalBarAndUpArrowIntoABinarySelector.
	hereType == #binary ifTrue: [^self binaryPattern ].
	hereType == #keyword ifTrue: [^self keywordPattern ]. 
	
	^self expected: self messagePatternExpectedErrorMessage
! !


!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 9/Jan/2026 12:37:07'!
withMethodNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

	| selectedClass methodNode noPattern |

	selectedClass := self codeProvider selectedClassOrMetaClass.
	noPattern := false.
	"When debugging a do it there is no message pattern (that is, method name). 
	I try to parse first with a message pattern and if it fails because there is no message pattern I retry parsing
	without message pattern - Hernan"
	[ methodNode := selectedClass methodNodeFor: model actualContents string noPattern: noPattern ]
		on: UndeclaredVariableWarning 
		do: [ :ex | ex resume ]
		on: SyntaxErrorNotification 
		do: [:aSyntaxError |
			"I check for noPattern not to be really sure that I do not enter an infinite loop with the retry.
			I should not happen becuase I only do a retry if we get a messagePattern expected error, 
			but things can change ... - Hernan"
			(noPattern not and: [ aSyntaxError errorMessage = (Parser expectedErrorMessageFor: Parser messagePatternExpectedErrorMessage) ])
				ifTrue: [ 
					noPattern := true.
					aSyntaxError retry ]
				ifFalse: [ aSyntaxError pass ]]
		on: Error 
		do: [ :anError | ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: selectedClass.! !

