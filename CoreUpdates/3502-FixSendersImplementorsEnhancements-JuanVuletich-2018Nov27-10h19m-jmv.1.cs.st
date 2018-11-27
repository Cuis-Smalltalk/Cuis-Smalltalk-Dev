'From Cuis 5.0 of 7 November 2016 [latest update: #3501] on 27 November 2018 at 10:20:03 am'!

!Workspace methodsFor: 'accessing' stamp: 'jmv 11/27/2018 10:19:24'!
methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

	^[ UndefinedObject methodNodeFor: aSourceCode noPattern: true ] on: Error, UndeclaredVariableReference do: aParsingErrorBlock
! !


!CodeProvider methodsFor: 'contents' stamp: 'jmv 11/27/2018 10:19:13'!
methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

	^[ self selectedClassOrMetaClass methodNodeFor: aSourceCode ] on: Error, UndeclaredVariableReference do: aParsingErrorBlock
	! !


!Inspector methodsFor: 'contents' stamp: 'jmv 11/27/2018 10:19:17'!
methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

	^[ self selectedClassOrMetaClass methodNodeFor: aSourceCode noPattern: true ] on: Error, UndeclaredVariableReference do:  aParsingErrorBlock ! !

