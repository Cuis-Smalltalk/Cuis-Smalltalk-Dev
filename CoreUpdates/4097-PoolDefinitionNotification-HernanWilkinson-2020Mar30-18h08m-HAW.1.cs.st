'From Cuis 5.0 [latest update: #4024] on 30 March 2020 at 6:10:32 pm'!
!classDefinition: #PoolDefinitionNotification category: #'Kernel-Classes'!
Notification subclass: #PoolDefinitionNotification
	instanceVariableNames: 'name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Classes'!

!Class methodsFor: 'initialization' stamp: 'HAW 3/30/2020 18:09:32'!
sharing: poolString 
	"Set up sharedPools. Answer whether recompilation is advisable."
	| oldPools |
	oldPools _ self sharedPools.
	sharedPools _ OrderedCollection new.
	(Scanner new scanFieldNames: poolString) do: 
		[:poolName | 
		sharedPools add: (Smalltalk at: poolName asSymbol ifAbsent:[
			(PoolDefinitionNotification signalNamed: poolName)
				ifTrue:[Smalltalk at: poolName asSymbol put: Dictionary new]
				ifFalse:[^self error: poolName,' does not exist']])].
	sharedPools isEmpty ifTrue: [sharedPools _ nil].
	^oldPools anySatisfy: [ :pool |
		self sharedPools noneSatisfy: [ :p | p == pool ]]! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'HAW 3/30/2020 18:09:14'!
compileNextChunkHandlingExceptions 

	[ self compileNextChunk ]
		on: InMidstOfFileinNotification, UndeclaredVariableWarning, PoolDefinitionNotification 
		do: [ :ex | ex resume: true ]! !


!PoolDefinitionNotification methodsFor: 'exception handling' stamp: 'HAW 3/30/2020 18:10:04'!
defaultAction

	| shouldResume |
	
	shouldResume := PopUpMenu confirm: 'The pool dictionary ', name,' does not exist.',
						'\Do you want it automatically created?' withNewLines.
						
	^ self resume: shouldResume ! !

!PoolDefinitionNotification methodsFor: 'initialization' stamp: 'HAW 3/30/2020 18:10:08'!
initializeNamed: aName

	name := aName ! !


!PoolDefinitionNotification class methodsFor: 'signaling' stamp: 'HAW 3/30/2020 18:09:58'!
signalNamed: aName

	^(self named: aName) signal! !

!PoolDefinitionNotification class methodsFor: 'instance creation' stamp: 'HAW 3/30/2020 18:09:54'!
named: aName

	^self new initializeNamed: aName! !

