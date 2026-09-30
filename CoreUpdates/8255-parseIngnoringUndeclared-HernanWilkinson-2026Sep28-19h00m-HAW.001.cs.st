'From Cuis7.9 [latest update: #8184] on 29 September 2026 at 9:55:17 am'!

!AddParameter class methodsFor: 'pre-conditions' stamp: 'HAW 29/Sep/2026, 12:13:02 pm (UTC)'!
assertNewParameterValueCompiles: trimmedNewParameterValue

	^ [
		Parser
			parseIgnoringUndeclared: trimmedNewParameterValue
			class: self
			noPattern: true ]
		on: SyntaxErrorNotification 
		do: [ :error | self signalNewParameterValueDoesNotCompile ].! !


!SourceCodeOfTemporaryToBeExtractedPrecondition methodsFor: 'private' stamp: 'HAW 29/Sep/2026, 12:08:43 pm (UTC)'!
initializeParseNodeToExtract

	parseNodeToExtract := [
		Parser
			parseIgnoringUndeclared: sourceCodeToExtract
			class: method methodClass
			noPattern: true ]
		on: SyntaxErrorNotification
		do: [ :anError | self signalCollaborationToExtractHasSyntaxError: anError ]! !


!SourceCodeOfTemporaryToBeExtractedPrecondition reorganize!
('evaluating' value)
('exceptions' signalCollaborationToExtractHasSyntaxError: signalCollaborationToExtractHasToBeOneStatementError signalSourceCodeIncludesAReturnStatementError)
('initialization' initializeFor:of:)
('pre-conditions' assertSourceCodeContainValidNodes assertSourceCodeDoesNotIncludeReturnExpressions assertSourceCodeHasOneStatement assertSourceCodeIsACompleteExpression)
('private' anySourceRangeMatchesExactlyTheIntervalToExtract initializeParseNodeToExtract parseNodeUnderIntervalToExtractCanBeExtractedToAVariable thereAreReturnExpressions)
!

