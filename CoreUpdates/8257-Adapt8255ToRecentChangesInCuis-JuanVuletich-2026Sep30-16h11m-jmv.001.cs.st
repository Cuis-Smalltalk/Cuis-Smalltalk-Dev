'From Cuis7.9 [latest update: #8256] on 30 September 2026 at 4:15:12 pm'!

!AddParameter class methodsFor: 'pre-conditions' stamp: 'jmv 30/Sep/2026, 7:13:23 pm (UTC)'!
assertNewParameterValueCompiles: trimmedNewParameterValue

	^ [
		[ Parser
			parse: trimmedNewParameterValue
			class: self
			noPattern: true ]
				on: UndeclaredVariableWarning
				do: [ :warning | warning resume: false ]
	]
		on: SyntaxErrorNotification 
		do: [ :error | self signalNewParameterValueDoesNotCompile ]! !


!SourceCodeOfTemporaryToBeExtractedPrecondition methodsFor: 'private' stamp: 'jmv 30/Sep/2026, 7:11:36 pm (UTC)'!
initializeParseNodeToExtract

	parseNodeToExtract := [
		[ Parser
			parse: sourceCodeToExtract
			class: method methodClass
			noPattern: true ]
				on: UndeclaredVariableWarning
				do: [ :warning | warning resume: false ]
	]
		on: SyntaxErrorNotification
		do: [ :anError | self signalCollaborationToExtractHasSyntaxError: anError ].! !

