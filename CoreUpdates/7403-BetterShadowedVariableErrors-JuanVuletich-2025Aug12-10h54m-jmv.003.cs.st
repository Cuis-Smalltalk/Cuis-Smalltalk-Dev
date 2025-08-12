'From Cuis7.5 [latest update: #7402] on 12 August 2025 at 11:01:01 am'!

!Encoder methodsFor: 'private' stamp: 'jmv 8/12/2025 10:48:26'!
warnAboutShadowed: aVariableNode evenIfDuringFileIn: aBoolean

	| msg transcriptMsg |
	msg := 'There already exists ', aVariableNode descriptiveString withArticle, '. Please use a different name.'.
	requestor addWarning: msg.
	transcriptMsg := msg, ' (In definition of: ', class name.
	selector
		ifNotNil: [
			transcriptMsg := transcriptMsg, '>>', selector printString ].
	(transcriptMsg, ')') print.
	
	aBoolean
		ifTrue: [self notify: msg]
		ifFalse: [
			InMidstOfFileinNotification signal ifFalse: [
					requestor interactive
						ifTrue: [
							self notify: msg ]
						ifFalse: [
							(RecompilationFailure class: class messageText: msg) signal ]]]! !


!VariableNode methodsFor: 'printing' stamp: 'jmv 8/12/2025 10:03:12'!
descriptiveString

	^self class name, ' named: ', name printString! !


!InstanceVariableNode methodsFor: 'printing' stamp: 'jmv 8/12/2025 10:02:45'!
descriptiveString

	^'Instance Variable named: ', name printString! !


!TempVariableNode methodsFor: 'printing' stamp: 'jmv 8/12/2025 10:12:07'!
descriptiveString
	self isMethodArg
		ifTrue: [ ^'Method Argument named: ', name printString ].
	self isBlockArg
		ifTrue: [ ^'Block Argument named: ', name printString ].
	^'Temporary Variable named: ', name printString! !


!Encoder methodsFor: 'temps' stamp: 'jmv 8/12/2025 10:59:18'!
autoBind: name 
	"Declare a block argument as a temp if not already declared."
	| node |
	node := scopeTable 
			at: name
			ifAbsent: 
				[(self lookupInPools: name ifFound: [:assoc | assoc])
					ifTrue: [self warnAboutShadowed: name evenIfDuringFileIn: false].
				^ (self reallyBind: name) nowHasDef nowHasRef scope: 1].
	node isTemp
		ifTrue: [node scope >= 0 ifTrue:
					[^self warnAboutShadowed: node evenIfDuringFileIn: true].
				node nowHasDef nowHasRef scope: 1]
		ifFalse: [^self warnAboutShadowed: node evenIfDuringFileIn: true].
	^node! !

!Encoder methodsFor: 'temps' stamp: 'jmv 8/12/2025 10:57:45'!
bindBlockTemp: name 
	"Declare a temporary block variable; complain if it's not a field or class variable."

	| node |
	node := scopeTable at: name ifAbsent: [^self reallyBind: name].
	node isTemp
		ifTrue: [
			node scope >= 0 ifTrue: [^self warnAboutShadowed: node evenIfDuringFileIn: true].
			node scope: 0]
		ifFalse: [^self warnAboutShadowed: node evenIfDuringFileIn: true].
	^node
! !

!Encoder methodsFor: 'temps' stamp: 'jmv 8/12/2025 10:58:47'!
bindTemp: name 
	"Declare a temporary; error not if a field or class variable."
	scopeTable at: name ifPresent:[:node|
		"When non-interactive raise the error only if its a duplicate"
		node isTemp
			ifTrue:[^self warnAboutShadowed: node evenIfDuringFileIn: true]
			ifFalse:[self warnAboutShadowed: name evenIfDuringFileIn: false]].
	^self reallyBind: name! !


!BytecodeEncoder methodsFor: 'temps' stamp: 'jmv 8/12/2025 11:00:22'!
bindTemp: name
	"Declare a temporary; error not if a field or class variable or out-of-scope temp."

	scopeTable
		at: name
		ifPresent: [ :node |
			"When non-interactive raise the error only if it is a duplicate"
			node isTemp
				ifTrue: [ node scope >= 0 ifTrue: [
						^self warnAboutShadowed: node evenIfDuringFileIn: true ]]
				ifFalse: [ 
					self warnAboutShadowed: node evenIfDuringFileIn: false]].
	^ self reallyBind: name.! !

!methodRemoval: Encoder #warnAboutShadowed: stamp: 'jmv 8/12/2025 11:00:29'!
Encoder removeSelector: #warnAboutShadowed:!
