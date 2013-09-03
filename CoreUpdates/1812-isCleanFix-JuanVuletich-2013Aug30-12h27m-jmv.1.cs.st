'From Cuis 4.2 of 25 July 2013 [latest update: #1811] on 30 August 2013 at 12:33:54.422955 pm'!

!BlockClosure methodsFor: 'testing' stamp: 'jmv 8/30/2013 12:29'!
isClean
	"A clean closure is one that doesn't really need the home context because:
		- It doesn't send messages to self or super
		- It doesn't access any instance variable
		- It doesn't access any outer temp
		- It doesn't do ^ return
	"

	| recreated source |
	source _ self decompile decompileString.

	"This catches any acess to outer context!!"
	recreated _ [ Compiler evaluate: source ] on: UndeclaredVariableWarning do: [ :ex | ^ false ].

	"Fail if returns from outer context, or uses self"
	Smalltalk
		eliotsClosureMeasurementsOn: recreated outerContext method
		over: [ :closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesUAR :anyClosureUsesSelf |
			anyClosureDoesUAR ifTrue: [ ^ false ].
			anyClosureUsesSelf ifTrue: [ ^ false ]].

	"Ok."
	^true! !

