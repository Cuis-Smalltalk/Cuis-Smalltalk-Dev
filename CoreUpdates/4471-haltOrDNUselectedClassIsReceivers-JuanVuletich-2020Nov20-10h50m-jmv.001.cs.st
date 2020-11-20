'From Cuis 5.0 [latest update: #4469] on 20 November 2020 at 10:58:54 am'!

!Debugger methodsFor: 'class list' stamp: 'jmv 11/20/2020 10:56:52'!
selectedClass
	"Answer the class in which the currently selected context's method was found."

	^self selectedContext ifNotNil: [ :ctx |
		(#(doesNotUnderstand: halt halt:) statePointsTo: ctx selector)
			ifTrue: [ctx receiver class]
			ifFalse: [ctx methodClass ]]! !

