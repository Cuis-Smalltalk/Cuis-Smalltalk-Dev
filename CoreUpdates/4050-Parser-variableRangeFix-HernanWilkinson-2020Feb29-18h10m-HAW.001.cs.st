'From Cuis 5.0 [latest update: #4049] on 29 February 2020 at 6:14:13 pm'!

!Parser methodsFor: 'expression types' stamp: 'HAW 2/29/2020 18:10:55'!
variable
	
	^self advanceWithRangeDo: [ :variableName :range | | varName result rightRange |
		varName := variableName.
		
		"See ParserTest>>#testRangesAreOkWhenReturningAVariableWithoutSpaceAfterThat
		There is a very difficult bug to fix. It happends when the source code ends with a return of a variable of 
		one char an no other char after that, for example: '^a' 
		In that case the range includes the ^ that is incorrect and makes the rename temporary fail.
		I do this fix becuase changing how the range is calculated is almost imposible due to the coupling 
		and complexity of the parser. This change applies only to variables and therefore it assures no
		unexpected behavior. I'm not cheching for size = 1 because it is redundant - Hernan"
		rightRange := varName size = range size
			ifTrue: [ range ]
			ifFalse: [ range last - varName size + 1 to: range last ].
		
		[result _ encoder encodeVariable: varName sourceRange: rightRange ifUnknown: [ nil ].
		result ifNil: [
			result _ (UndeclaredVariableReference new)
				parser: self;
				varName: varName;
				varStart: rightRange first;
				varEnd: rightRange last;
				signal ].
		result isString ] whileTrue: [ varName _ result].
		encoder addMultiRange: rightRange for: result ].
	! !

