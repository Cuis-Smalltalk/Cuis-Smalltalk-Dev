'From Cuis 5.0 of 7 November 2016 [latest update: #3138] on 18 August 2017 at 3:36:55 pm'!

!Debugger methodsFor: 'code pane' stamp: 'jmv 8/18/2017 15:36:20'!
contentsSelection

	"If we are just creating the method in the debugger, help the user by selecting the method body."
	currentCompiledMethod ifNotNil: [
		((currentCompiledMethod hasLiteral: #shouldBeImplemented) and: [
			(self selectedContext debuggerMap method abstractPCForConcretePC: self selectedContext pc) = 1]) ifTrue: [
				^currentCompiledMethod selector size + 3  to: currentCompiledMethod getSource size ]].

	^self pcRange! !

