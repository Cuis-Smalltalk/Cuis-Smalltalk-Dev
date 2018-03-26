'From Cuis 5.0 of 7 November 2016 [latest update: #3269] on 12 March 2018 at 3:44:27 pm'!
!classDefinition: #Scanner category: #'Compiler-Kernel'!
Object subclass: #Scanner
	instanceVariableNames: 'source mark hereChar aheadChar token tokenType currentComment buffer typeTable isForTokenization sentInLiterals '
	classVariableNames: 'TypeTable '
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 3/12/2018 15:37:29'!
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

!Scanner methodsFor: 'initialization' stamp: 'jmv 3/12/2018 15:05:15'!
initScanner

	buffer := WriteStream on: (String new: 40).
	typeTable := Scanner typeTable.
	isForTokenization := false.
	sentInLiterals := Set new! !


!Parser methodsFor: 'public access' stamp: 'jmv 3/12/2018 15:39:43'!
parse: sourceStream class: class category: aCategory noPattern: noPattern context: ctxt notifying: req ifFail: aBlock
	"Answer a MethodNode for the argument, sourceStream, that is the root of
	 a parse tree. Parsing is done with respect to the argument, class, to find
	 instance, class, and pool variables; and with respect to the argument,
	 ctxt, to find temporary variables. Errors in parsing are reported to the
	 argument, req, if not nil; otherwise aBlock is evaluated. The argument
	 noPattern is a Boolean that is true if the the sourceStream does not
	 contain a method header (i.e., for DoIts)."
	| methNode repeatNeeded myStream s p |
	category _ aCategory.
	myStream _ sourceStream.
	[
		repeatNeeded _ false.
		p _ myStream position.
		s _ myStream upToEnd.
		myStream position: p.
		self encoder init: class context: ctxt notifying: self.
		self init: myStream notifying: req failBlock: [
			^ aBlock value ].
		doitFlag _ noPattern.
		failBlock _ aBlock.
		[ methNode _ self method: noPattern context: ctxt ]
			on: ReparseAfterSourceEditing
			do: [ :ex |
				repeatNeeded _ true.
				myStream _ sourceStreamGetter notNil 	"Cuis specific. Do not remove!!"
					ifTrue: [ requestor perform: sourceStreamGetter ]
					ifFalse: [ ReadStream on: requestor text string ]].
		repeatNeeded
	] whileTrue: [
		encoder _ self encoder class new ].
	methNode sourceText: s.
	"See #xBacktick"
	sentInLiterals do: [ :sym | encoder noteOptimizedSelector: sym ].
	^ methNode! !

!methodRemoval: Parser #method:context:encoder:!
Parser removeSelector: #method:context:encoder:!
!classDefinition: #Scanner category: #'Compiler-Kernel'!
Object subclass: #Scanner
	instanceVariableNames: 'source mark hereChar aheadChar token tokenType currentComment buffer typeTable isForTokenization sentInLiterals'
	classVariableNames: 'TypeTable'
	poolDictionaries: ''
	category: 'Compiler-Kernel'!
