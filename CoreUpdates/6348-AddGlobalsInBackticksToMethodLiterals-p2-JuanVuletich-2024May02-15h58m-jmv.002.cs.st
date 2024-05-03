'From Cuis6.3 [latest update: #6340] on 2 May 2024 at 4:01:46 pm'!

!Encoder methodsFor: 'results' stamp: 'jmv 5/2/2024 15:53:02'!
allLiterals
	| newLiterals |
	addedSelectorAndMethodClassLiterals ifFalse: [
		addedSelectorAndMethodClassLiterals := true.
		"Put the optimized selectors and backtick introduced bindings in literals so they are shown in senders and references."
		optimizedSelectors := optimizedSelectors reject: [ :e | literalStream originalContents hasLiteral: e ].
		bindingsInBacktickLiterals := bindingsInBacktickLiterals reject: [ :e | literalStream originalContents hasLiteral: e ].
		newLiterals := optimizedSelectors asArray sort, bindingsInBacktickLiterals asArray.
		newLiterals isEmpty ifFalse: [
			"Use one entry per literal if enough room, else make anArray"
			literalStream position + newLiterals size + 2 < self maxNumLiterals
				ifTrue: [ newLiterals do: [ :e | self litIndex: e ]]
				ifFalse: [ self litIndex: newLiterals ]].
		"Add a slot for selector or MethodProperties"
		self litIndex: nil.
		self litIndex: self associationForClass ].
	^literalStream contents! !

!methodRemoval: BacktickNode #addSymbolsOfExpressionToParser stamp: 'jmv 5/2/2024 15:59:11'!
BacktickNode removeSelector: #addSymbolsOfExpressionToParser!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Compiler recompileAll!

