'From Cuis7.9 [latest update: #8098] on 29 July 2026 at 2:40:43 pm'!

!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'JEC 8/Jan/2026, 12:58:10 am (UTC)'!
initializeNewSourceCode

	| shouldReturnAnyVariable |
	
	shouldReturnAnyVariable := extractMethodProgrammer shouldReturnAnyVariable.

	newSourceCode := String streamContents: [ :stream |
		stream
			nextPutAll: self newMessageString;
			nextPutAll: self startingMethodIdentation.
		extractMethodProgrammer shouldDeclareAnyVariable ifTrue: [
			stream nextPutAll: extractMethodProgrammer declareTempVarsCode ].
		shouldReturnAnyVariable ifFalse: [
			stream nextPutAll: self returnCharacterIfNeeded ].
		stream
			nextPutAll: sourceCodeToExtract.
		shouldReturnAnyVariable ifTrue: [
			stream
				newLineTab:1;
				nextPutAll: extractMethodProgrammer returnTempVarsCode ].
	].
! !

!methodRemoval: Transcript class #nextPutAll:when: stamp: 'jmv 29/Jul/2026, 5:40:31 pm (UTC)'!
Transcript class removeSelector: #nextPutAll:when:!
!methodRemoval: WriteStream #newLineTab:when: stamp: 'jmv 29/Jul/2026, 5:40:32 pm (UTC)'!
WriteStream removeSelector: #newLineTab:when:!
!methodRemoval: Stream #nextPutAllValueOf:when: stamp: 'jmv 29/Jul/2026, 5:40:33 pm (UTC)'!
Stream removeSelector: #nextPutAllValueOf:when:!
!methodRemoval: Stream #nextPutAll:when: stamp: 'jmv 29/Jul/2026, 5:40:34 pm (UTC)'!
Stream removeSelector: #nextPutAll:when:!
