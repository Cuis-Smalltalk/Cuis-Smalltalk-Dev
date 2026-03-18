'From Cuis7.7 [latest update: #7840] on 18 March 2026 at 03:34:21 PM'!

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 18/Mar/2026 15:20:16'!
debug: aCompiledMethod receiver: anObject in: evalContext

	Debugger
		openDebugging: [
			aCompiledMethod
				valueWithReceiver: anObject
				arguments: (evalContext isNil ifTrue: [ #() ] ifFalse: [ { evalContext } ])]
				"Avoid the use of #ifNil:ifNotNil: here.
				If preference #avoidIfNilOptimization has changed, but the method has not been recompiled,
				the debugger could break rather badly."
		to: aCompiledMethod
		label: 'Debug it'.! !

