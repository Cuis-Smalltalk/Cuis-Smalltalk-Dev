'From Cuis 5.0 of 7 November 2016 [latest update: #3016] on 30 December 2016 at 10:57:00 am'!

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 12/30/2016 10:29:16'!
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
	token _ [ Compiler evaluate: buffer contents ] 
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

