'From Cuis7.5 [latest update: #7450] on 27 August 2025 at 12:02:33 pm'!

!TemporaryVariableDeclarationRemover methodsFor: 'source text generation' stamp: 'jmv 8/27/2025 10:17:43'!
sourceTextWithoutTemporaryDeclarationLineInBlockNode: aBlockNode
	
	| sourceTextUpToEndTemps endTempsMark startTempsMark |
	endTempsMark := aBlockNode tempsMark.
	sourceTextUpToEndTemps := methodNode sourceText copyFrom: 1 to: endTempsMark - 1.
	startTempsMark := sourceTextUpToEndTemps findLast: [ :c | c = $| ].
	^self sourceTextWithoutTemporaryDeclarationLineFrom: startTempsMark to: endTempsMark! !

