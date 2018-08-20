'From Cuis 5.0 of 7 November 2016 [latest update: #3403] on 11 August 2018 at 11:32:56 pm'!

!Parser methodsFor: 'expression types' stamp: 'HAW 8/11/2018 23:20:34'!
primaryExpression 
	hereType == #word 
		ifTrue: [
			parseNode _ self variable.
			
			"
			(parseNode isUndefTemp and: [self interactive] and: [
				((requestor isKindOf: Editor) or: [ requestor is: #Morph ])])
			"
			"If the requestor is of an exotic kind (like a telnet server) we might not be
			 allowed to open a PupUpMenu for querying the user"
				
			(parseNode isUndefTemp and: [ self interactive ])
				ifTrue: [self queryUndefined].
			parseNode nowHasRef.
			^ true].
	hereType == #leftBracket
		ifTrue: [
			self advance.
			self blockExpression.
			^true].
	hereType == #leftBrace
		ifTrue: [
			self braceExpression.
			^true].
	hereType == #leftParenthesis
		ifTrue: [
			self advance.
			self expression ifFalse: [^self expected: 'expression'].
			(self match: #rightParenthesis)
				ifFalse: [^self expected: 'right parenthesis'].
			^true].
	(hereType == #string or: [hereType == #number or: [hereType == #literal]])
		ifTrue: [ 
			parseNode := self advanceWithRangeDo: [ :lexema :range | encoder encodeLiteral: lexema range: range ].
			^true].
	(here == #- and: [tokenType == #number])
		ifTrue: [
			self advanceWithRangeDo: [ :minusChar :minusRange |
				self advanceWithRangeDo: [ :number :numberRange |
					parseNode := encoder encodeLiteral: number negated range: (minusRange first to: numberRange last)]].
			^true].
	^false! !

!Parser methodsFor: 'expression types' stamp: 'HAW 8/11/2018 23:23:08'!
temporaries
	" [ '|' (variable)* '|' ]"
	| vars theActualText |
	(self match: #verticalBar) ifFalse: 
		["no temps"
		doitFlag ifTrue:
			[tempsMark := self interactive
								ifTrue: [requestor selectionInterval first]
								ifFalse: [1].
			^ #()].
		tempsMark := hereMark	"formerly --> prevMark + prevToken".
		tempsMark > 0 ifTrue:
			[theActualText := source contents.
			[tempsMark < theActualText size and: [(theActualText at: tempsMark) isSeparator]]
				whileTrue: [tempsMark := tempsMark + 1]].
			^ #()].
	vars := OrderedCollection new.
	[hereType == #word]
		whileTrue: [
			self advanceWithRangeDo: [ :variableName :range |
				vars addLast: (encoder bindTemp: variableName range: range)]].
	(self match: #verticalBar) ifTrue: 
		[tempsMark := prevMark.
		^ vars].
	^ self expected: 'Vertical bar'
! !

!Parser methodsFor: 'expression types' stamp: 'HAW 8/11/2018 23:30:36'!
variable
	
	^self advanceWithRangeDo: [ :variableName :range | | varName result |
		varName := variableName.
		[result _ encoder encodeVariable: varName sourceRange: range ifUnknown: [ nil ].
		result ifNil: [
			result _ (UndeclaredVariableReference new)
				parser: self;
				varName: varName;
				varStart: range first;
				varEnd: range last;
				signal ].
		result isString ] whileTrue: [ varName _ result].
		encoder addMultiRange: range for: result ].
	! !

