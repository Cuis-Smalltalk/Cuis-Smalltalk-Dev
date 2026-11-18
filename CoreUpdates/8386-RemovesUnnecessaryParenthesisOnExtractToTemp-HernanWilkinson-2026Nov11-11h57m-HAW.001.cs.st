'From Cuis7.9 [latest update: #8340] on 11 November 2026 at 6:04:51 pm'!

!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'HAW 11/Nov/2026, 5:11:27 pm (UTC)'!
editToNewVariableOf: anIntervalToExtract

	| replacement |

	replacement := ExpressionReplacement
		of: anIntervalToExtract
		in: methodNodeToRefactor
		by: newVariableName
		withPrecedence: ParseNode atomicExpressionPrecedence.

	^replacement intervalToReplace -> replacement sourceCodeToWrite! !


!ExtractMultipleToTemporary methodsFor: 'private - applying steps' stamp: 'HAW 11/Nov/2026, 5:11:27 pm (UTC)'!
editsToNewVariable

	^collectionOfIntervalsToExtract collect: [ :anIntervalToExtract |
		self editToNewVariableOf: anIntervalToExtract ]! !


!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'HAW 11/Nov/2026, 5:11:27 pm (UTC)'!
replaceExtractedCodeWithNewTemporaryVariable

	updatedSourceCode := updatedSourceCode
		copyReplacing: { self editToNewVariableOf: intervalToExtract }! !


!ExtractMultipleToTemporary methodsFor: 'private - applying steps' stamp: 'HAW 11/Nov/2026, 5:11:27 pm (UTC)'!
replaceAllCodeIntervalsWithNewVariable

	updatedSourceCode := updatedSourceCode copyReplacing: self editsToNewVariable! !

