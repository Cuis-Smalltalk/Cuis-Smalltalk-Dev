Class {
	#name : 'PushDownMethod',
	#superclass : 'TransferMethod',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
PushDownMethod class >> addShadowedInstVarsOf: subclass in: aMethodToPushDown to: subclassesWithShadowedInstVarNames [

	| shadowedInstVarNames |

	shadowedInstVarNames := subclass instVarNames select: [:instVarName |
		(aMethodToPushDown hasArgumentOrTemporaryNamed: instVarName) ].

	 shadowedInstVarNames ifNotEmpty: [
		subclassesWithShadowedInstVarNames at: subclass put: shadowedInstVarNames ]
]

{ #category : 'pre-conditions' }
PushDownMethod class >> assert: aMethodToPushDown hasNoShadowedInstVarsIn: aSubclasses  [

	| subclassesWithShadowedInstVarNames |

	subclassesWithShadowedInstVarNames := Dictionary new.

	aSubclasses do: [:subclass |
		self addShadowedInstVarsOf: subclass in: aMethodToPushDown to: subclassesWithShadowedInstVarNames ].

	subclassesWithShadowedInstVarNames ifNotEmpty: [
		self signalMethodCannotShadowInstVarOfSubclasses: subclassesWithShadowedInstVarNames]
]

{ #category : 'exceptions' }
PushDownMethod class >> errorMessageCanNotPushDownWithShadowedInstVarsOf: subclassesWithShadowedInstVarNames [

	^String streamContents: [ :stream |
		stream
			nextPutAll: 'Can not push down method because the following subclasses';
			newLine;
			nextPutAll: 'would have shadowed instance variables:'.

		subclassesWithShadowedInstVarNames
			keysAndValuesDo: [ :class :instVarNames |
				stream
					newLine;
					print: class;
					nextPutAll: ': '.
				instVarNames asCommaSeparated: [:instVarName | stream nextPutAll: instVarName ] on: stream ]].



]

{ #category : 'pre-conditions' }
PushDownMethod class >> is: subclass equivalentlyImplementedIn: aMethodToPushDown [

	^ subclass 
		compiledMethodAt: aMethodToPushDown selector
		ifPresent: [ :methodInSubclass | (methodInSubclass equivalentTo: aMethodToPushDown) not ]
		ifAbsent: [ false ]
]

{ #category : 'exceptions' }
PushDownMethod class >> signalMethodCannotShadowInstVarOfSubclasses: subclassesWithShadowedInstVarNames [

	self refactoringError: (
		self errorMessageCanNotPushDownWithShadowedInstVarsOf: subclassesWithShadowedInstVarNames)
]

{ #category : 'pre-conditions' }
PushDownMethod class >> warnIf: aMethodToPushDown existsOn: aSubclasses  [

	| subclassesImplementingMessage |

	subclassesImplementingMessage := aSubclasses select: [:subclass |
		self is: subclass equivalentlyImplementedIn: aMethodToPushDown ].
		
	subclassesImplementingMessage ifNotEmpty: [
		self warnMessageAlreadyImplementedIn: subclassesImplementingMessage ]
]

{ #category : 'warnings' }
PushDownMethod class >> warnMessageAlreadyImplementedIn: subclassesImplementingMessage [

	self refactoringWarning:
		(self warningMesssageForMessageAlreadyImplementedIn: subclassesImplementingMessage)
]

{ #category : 'warnings' }
PushDownMethod class >> warningMesssageForMessageAlreadyImplementedIn: subclassesImplementingMessage [

	^String streamContents: [ :stream |
		stream
			nextPutAll: 'Method already exist in the following subclasses:';
			newLine.

		subclassesImplementingMessage asCommaSeparated: [ :aClass | stream print: aClass ] on: stream.

		stream
			newLine;
			nextPutAll: 'If you continue they will be overwritten' ].
]

{ #category : 'applying - private' }
PushDownMethod >> implementSubclassResponsibility [

	^ self implementSubclassResponsibilityIn: method methodClass
	
]
