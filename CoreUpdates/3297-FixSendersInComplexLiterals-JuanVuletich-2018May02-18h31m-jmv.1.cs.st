'From Cuis 5.0 of 7 November 2016 [latest update: #3296] on 2 May 2018 at 6:33:04 pm'!

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 5/2/2018 18:28:52'!
xBacktick
	"Smalltalk code evaluated at compile time as a literal."

	self step.
	buffer reset.
	[hereChar == $`
		and: [aheadChar == $`
				ifTrue: [self step. false]
				ifFalse: [true]]]
		whileFalse: [
			buffer nextPut: self step.
			(hereChar = Scanner doItCharacterValue asCharacter and: [source atEnd])
				ifTrue: [^self offEnd: 'Unmatched back quote']].
	self step.
	[ | compiler method methodNode |
		compiler _ Compiler new.
		methodNode _ compiler compileNoPattern: buffer contents in: UndefinedObject context: nil notifying: nil ifFail: [].
		method _ methodNode generate.
		"Grab all messages sent while evaluating literal, so the main method will be shown as senders of them"
		sentInLiterals addAll: method messages.
		method literalsDo: [ :literal | literal isSymbol ifTrue: [ sentInLiterals add: literal ]].
		"Evaluate now."
		token _ nil withArgs: #() executeMethod: method.
	] 
		on: SyntaxErrorNotification, UndeclaredVariableReference, Error
		do: [ :ex |
			ex class caseOf: {
				[ SyntaxErrorNotification ] -> [
						self notify: 'Can not compile: ', ex errorMessage at: mark].
				[ UndeclaredVariableReference ] -> [ 
						self notify: 'Can not compile: Variable ''', ex varName, ''' is not declared' at: mark ]
			} otherwise: [
						self notify: 'Can not evaluate code: ', ex description at: mark ]].
	tokenType _ #literal! !

