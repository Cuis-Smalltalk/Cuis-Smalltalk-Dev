'From Cuis 5.0 [latest update: #4129] on 2 May 2020 at 12:49:24 pm'!

!NewInstanceVariablePrecondition methodsFor: 'pre-conditions' stamp: 'HAW 5/2/2020 12:48:57'!
assertIsNotAlreadyDefinedInSuperclasses

	^ (classToAddInstVar classThatDefinesInstanceVariable: instVarName) 
		ifNotNil: [ :definingClass | self signalAlreadyDefinedInAll: {definingClass} ]
		! !

