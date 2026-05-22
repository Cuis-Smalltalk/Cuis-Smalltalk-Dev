Class {
	#name : 'BehaviorTest',
	#superclass : 'TestCase',
	#instVars : [
		'unreferenced',
		'referenced'
	],
	#category : 'BaseImageTests-Kernel-Classes',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Classes'
}

{ #category : 'instance variables tests' }
BehaviorTest >> testAllAccessToInstVarReturnsAccessReceiversBranch [

	| references |

	"Just in case the hierarchy changes... not much of a chance but... - Hernan"
	self deny: (IdentitySet includesBehavior: Dictionary).
	self deny: (Dictionary includesBehavior: IdentitySet).
	self assert: (IdentitySet includesBehavior: Set).
	self assert: (Dictionary includesBehavior: Set).
	self assert: (IdentityDictionary includesBehavior: Dictionary).

	references := IdentitySet allAccessesTo: 'array'.

	self assert: (references noneSatisfy: [ :aReference | aReference methodClass = IdentityDictionary ])
]

{ #category : 'instance variables tests' }
BehaviorTest >> testAllAccessToIsEmptyForVariableNotDefinedInClass [

	self assert: (Object allAccessesTo: 'iv1') isEmpty
]

{ #category : 'instance variables tests' }
BehaviorTest >> testAllBroadAccessToReturnsAccessInAllHierarchy [

	| references |

	"Just in case the hierarchy changes... not much of a chance but... - Hernan"
	self deny: (IdentitySet includesBehavior: Dictionary).
	self deny: (Dictionary includesBehavior: IdentitySet).
	self assert: (IdentitySet includesBehavior: Set).
	self assert: (Dictionary includesBehavior: Set).

	references := IdentitySet allBroadAccessesTo: 'array'.

	self assert: (references anySatisfy: [ :aReference | aReference methodClass = Dictionary ])
]

{ #category : 'class hierarchy tests' }
BehaviorTest >> testAllSuperclassesUpToFailsWhenWantsToStopOnInvalidSuperclass [

	| invalidSuperclass |
	
	invalidSuperclass := Collection.
	self
		should: [ SmallInteger allSuperclassesUpTo: invalidSuperclass ]
		raise: Error
		withExceptionDo: [:anError | 
			self assert: (nil superclassNotValidErrorDescriptionFor: invalidSuperclass) equals: anError messageText ].
]

{ #category : 'class hierarchy tests' }
BehaviorTest >> testAllSuperclassesUpToPutsFirstSuperclassesFirst [

	| superclasses |
	
	superclasses := SmallInteger allSuperclassesUpTo: Number.
	
	self assert: superclasses size equals: 2.
	self assert: superclasses first equals: Integer.
	self assert: superclasses second equals: Number.
]

{ #category : 'class hierarchy tests' }
BehaviorTest >> testAllSuperclassesUpToStopsOnRightClass [

	| superclasses |
	
	superclasses := SmallInteger allSuperclassesUpTo: Integer.
	
	self assert: superclasses size equals: 1.
	self assert: (superclasses includes: Integer).
]

{ #category : 'instance variables tests' }
BehaviorTest >> testAllUnreferencedInstanceVariablesReturnsOnlyUnreferencedVariables [

	| unreferencedVariables |
	
	unreferencedVariables := BehaviorTestSubclass allUnreferencedInstanceVariables.
	
	self assert: unreferencedVariables size equals: 1.
	self assert: (unreferencedVariables includes: 'unreferenced').
]

{ #category : 'instance variables tests' }
BehaviorTest >> testHasReferencesToInstanceVariableNamedLooksInClassOnly [

	self deny: (self class hasReferencesToInstanceVariableNamed: 'referenced').
	self assert: (BehaviorTestSubclass hasReferencesToInstanceVariableNamed: 'referenced').
	
]

{ #category : 'class hierarchy tests' }
BehaviorTest >> testHighestClassImplementingReturnsReceiverWhenNoOtherSuperclassImplementsSelector [

	| higestClass |
	
	higestClass := OrderedCollection highestClassImplementing: #addFirst: ifNone: [ self fail ].
	
	self assert: higestClass equals: OrderedCollection.
]

{ #category : 'class hierarchy tests' }
BehaviorTest >> testHighestClassImplementingReturnsTheTopSuperclassImplementingSelector [

	| higestClass |
	
	higestClass := OrderedCollection highestClassImplementing: #select: ifNone: [ self fail ].
	
	self assert: higestClass equals: Collection.
]

{ #category : 'class hierarchy tests' }
BehaviorTest >> testHighestClassImplementingValuesIfNoneBlockWhenSelectorIsNotImplemented [

	OrderedCollection highestClassImplementing: #x ifNone: [ ^self  ].
	
	self fail
]

{ #category : 'instance variables tests' }
BehaviorTest >> testIsInstanceVariableNamedReferencedInHierarchyLooksReferencesInHierarchy [

	self assert: (self class isInstanceVariableNamedReferencedInHierarchy: 'referenced').
	self deny: (self class isInstanceVariableNamedReferencedInHierarchy: 'unreferenced').
	
]

{ #category : 'instance variables tests' }
BehaviorTest >> testProtoObjectAllRegularInstVarNamesDoesNotFail [

	self
		shouldnt: [ ProtoObject allRegularInstVarNames ]
		raise: Error
]

{ #category : 'instance variables tests' }
BehaviorTest >> testUnreferencedInstanceVariablesReturnsOnlyUnreferencedVariables [

	| unreferencedVariables |
	
	unreferencedVariables := self class unreferencedInstanceVariables.
	
	self assert: unreferencedVariables size equals: 1.
	self assert: (unreferencedVariables includes: 'unreferenced').
]

{ #category : 'class hierarchy tests' }
BehaviorTest >> testWithAllSuperclassesUpToIncludesReceiverClass [

	| superclasses |
	
	superclasses := SmallInteger withAllSuperclassesUpTo: Number.
	
	self assert: superclasses size equals: 3.
	self assert: superclasses first equals: SmallInteger.
	self assert: superclasses second equals: Integer.
	self assert: superclasses third equals: Number.
]
