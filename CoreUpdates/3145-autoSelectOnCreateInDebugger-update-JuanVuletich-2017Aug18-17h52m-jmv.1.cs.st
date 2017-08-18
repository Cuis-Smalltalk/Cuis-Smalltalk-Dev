'From Cuis 5.0 of 7 November 2016 [latest update: #3144] on 18 August 2017 at 6:03:22 pm'!

!Debugger methodsFor: 'code pane' stamp: 'jmv 8/18/2017 18:02:55'!
contentsSelection

	"If we are just creating the method in the debugger, help the user by selecting the method body."
	| sendInterval |
	currentCompiledMethod ifNotNil: [
		((currentCompiledMethod hasLiteral: #shouldBeImplemented) and: [
			(currentCompiledMethod abstractPCForConcretePC: self selectedContext pc) = 1]) ifTrue: [
				sendInterval _ (self selectedContext debuggerMap abstractSourceMap at: 2 ifAbsent: [nil]).
				sendInterval ifNotNil: [ ^ sendInterval first - 5 to: sendInterval last + 1 ]]].

	^self pcRange! !

