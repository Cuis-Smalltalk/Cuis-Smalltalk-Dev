'From Cuis 5.0 of 7 November 2016 [latest update: #3403] on 12 August 2018 at 11:35:10 pm'!

!CompiledMethod methodsFor: 'comparing' stamp: 'jmv 8/12/2018 23:31:38'!
= method
	| numLits lit1 lit2 firstLitIndex |

	"Any object is equal to itself"
	self == method ifTrue: [ ^ true ].

	"Answer whether the receiver implements the same code as the 
	argument, method."
	(method is: #CompiledMethod) ifFalse: [ ^false ].
	self size = method size ifFalse: [ ^false ].
	self header = method header ifFalse: [ ^false ].
	self initialPC to: self endPC do: [ :i |
		(self at: i) = (method at: i) ifFalse: [ ^false ]].
	(numLits _ self numLiterals) ~= method numLiterals ifTrue: [ ^false ].

	"Dont bother checking FFI and named primitives''
	jmv: Does this make any sense?
	 (#(117 120) includes: self primitive) ifTrue: [^ true]."

	 "properties"
	(self properties analogousCodeTo: method properties) ifFalse: [
		^false  ].

	firstLitIndex _ 1.
	(#(117 120) includes: self primitive) ifTrue: [
		lit1 _ self literalAt: firstLitIndex.
		lit2 _ method literalAt: firstLitIndex.
			lit1 isArray
				ifTrue: [
					(lit2 isArray and: [ lit1 first = lit2 first and: [lit1 second = lit2 second]]) ifFalse: [
						^false ]]
				ifFalse: [ "ExternalLibraryFunction"
					(lit1 analogousCodeTo: lit2) ifFalse: [
						^false ]].
		firstLitIndex _ 2 ].

	"#penultimateLiteral is selector (or properties, just compared, above)
	Last literal is #methodClass.
	Don't compare them. Two methods might be equal even if they have different selector (or none at all)
	or are installed in different classes (or none at all)"
	firstLitIndex to: numLits-2 do: [ :i |
		lit1 _ self literalAt: i.
		lit2 _ method literalAt: i.
		lit1 = lit2 ifFalse: [
			"any other discrepancy is a failure"
			^ false ]].
	^true! !


!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 8/12/2018 21:36:15'!
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
		method literalsDo: [ :literal | literal isSymbol ifTrue: [ literal = #DoIt ifFalse: [sentInLiterals add: literal ]]].
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


!Encoder methodsFor: 'results' stamp: 'jmv 8/12/2018 22:28:05'!
allLiterals
	addedSelectorAndMethodClassLiterals ifFalse:
		[addedSelectorAndMethodClassLiterals := true.
		"Put the optimized selectors in literals so as to browse senders more easily"
		optimizedSelectors := optimizedSelectors reject: [:e| literalStream originalContents hasLiteral: e].
		optimizedSelectors isEmpty ifFalse: [
			"Use one entry per literal if enough room, else make anArray"
			literalStream position + optimizedSelectors size + 2 >= self maxNumLiterals
				ifTrue: [self litIndex: optimizedSelectors asArray sort]
				ifFalse: [optimizedSelectors sorted do: [:e | self litIndex: e]]].
		"Add a slot for selector or MethodProperties"
		self litIndex: nil.
		self litIndex: self associationForClass].
	^literalStream contents! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
ProtoObject withAllSubclasses do: [:c | c compileAll ].!

