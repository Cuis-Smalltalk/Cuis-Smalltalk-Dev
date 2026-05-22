Class {
	#name : 'SafelyRemoveClass',
	#superclass : 'Refactoring',
	#instVars : [
		'classToRemove'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
SafelyRemoveClass class >> assertNoReferencesTo: aClassToSafelyRemove [
	
	| referencesCollector |
	
	referencesCollector := ClassReferencesCollector valueOf: aClassToSafelyRemove.
	
	referencesCollector hasReferencesToClass ifTrue: [ 
		^self 
			signalCanNotRemove: aClassToSafelyRemove 
			dueToReferences: referencesCollector referencesToClass 
			toAll: referencesCollector referencedAsClass ].
	
	referencesCollector hasReferencesToName ifTrue: [
		^self 
			warnAboutRemoveOf: aClassToSafelyRemove 
			dueToNameReferences: referencesCollector referencesToName 
			toAll: referencesCollector referencedAsName ].

]

{ #category : 'exceptions' }
SafelyRemoveClass class >> errorMessageForCanNotRemove: aClassToSafelyRemove dueToReferencesToAll: referenced [

	^String streamContents: [ :stream |
		stream 
			nextPutAll: aClassToSafelyRemove name asPlainString;
			nextPutAll: ' can not be removed '.
	
		referenced size = 1 
			ifTrue: [ (referenced includes: aClassToSafelyRemove) 
				ifTrue: [ stream nextPutAll: 'because it has references' ]
				ifFalse: [ stream 
					nextPutAll: 'because it subclass, ';
					nextPutAll: referenced anyOne name asPlainString;
					nextPutAll: ', has references' ]]
			ifFalse: [ (referenced includes: aClassToSafelyRemove)
				ifTrue: [ stream 
					nextPutAll: 'due to references to: ';
					nextPutAll: referenced asCommaStringAnd ]
				ifFalse: [ stream
					nextPutAll: 'due to references to its subclasses: ';
					nextPutAll: referenced asCommaStringAnd ]]]
]

{ #category : 'instance creation' }
SafelyRemoveClass class >> of: aClassToSafelyRemove [

	| theNonMetaclassToRemove |
	
	theNonMetaclassToRemove := aClassToSafelyRemove theNonMetaClass.	
	self assertNoReferencesTo: theNonMetaclassToRemove.	
	self warnIfHasSubclasses: theNonMetaclassToRemove.
	
	^self new initializeOf: theNonMetaclassToRemove 
]

{ #category : 'exceptions' }
SafelyRemoveClass class >> signalCanNotRemove: aClassToSafelyRemove dueToReferences: references toAll: allReferenced [

	self 
		canNotRefactorDueToReferencesError: (self errorMessageForCanNotRemove: aClassToSafelyRemove dueToReferencesToAll: allReferenced)
		references: references 
		to: aClassToSafelyRemove 
	
]

{ #category : 'warnings' }
SafelyRemoveClass class >> warn: aClassToSafelyRemove hasSubclasses: allSubclasses [

	self refactoringWarning: (self warningMessageFor: aClassToSafelyRemove hasSubclasses: allSubclasses)
]

{ #category : 'warnings' }
SafelyRemoveClass class >> warnAboutRemoveOf: aClassToSafelyRemove dueToNameReferences: references toAll: allReferenced  [
	
	self 
		warnAboutReferences: references
		with: (self warningMessageForReferencesToNames: allReferenced)
		of: aClassToSafelyRemove 
		toAll: allReferenced 
	
]

{ #category : 'pre-conditions' }
SafelyRemoveClass class >> warnIfHasSubclasses: aClassToSafelyRemove [

	| allSubclasses |
	
	allSubclasses := aClassToSafelyRemove allSubclasses.
	allSubclasses isEmpty ifFalse: [ self warn: aClassToSafelyRemove hasSubclasses: allSubclasses ]
]

{ #category : 'warnings' }
SafelyRemoveClass class >> warningMessageFor: aClassToSafelyRemove hasSubclasses: allSubclasses [

	^String streamContents: [ :stream |
		stream nextPutAll: aClassToSafelyRemove name asPlainString.
		allSubclasses size = 1
			ifTrue: [ stream nextPutAll: ' has a subclass' ]
			ifFalse: [ stream 
					nextPutAll: ' has ';
					print:  allSubclasses size;
					nextPutAll: ' subclasses' ].
		stream nextPutAll: ' that will be removed'].
		
	
]

{ #category : 'warnings' }
SafelyRemoveClass class >> warningMessageForReferencesToNames: referenced [

	^'There are references to the name of ', referenced asCommaStringAnd 
]

{ #category : 'applying' }
SafelyRemoveClass >> apply [
	
	self removeWithAllSubclasses: classToRemove.
	
]

{ #category : 'initialization' }
SafelyRemoveClass >> initializeOf: aClassToSafetelyRemove  [
	
	classToRemove := aClassToSafetelyRemove 
]

{ #category : 'applying - private' }
SafelyRemoveClass >> removeWithAllSubclasses: aClassToRemove [

	"I have to do 'subclasses do:' and not 'subclassesDo:' because removing a class modifies parent's subclasses collection.
	#subclasses returns a copy of superclass' subclasses collection -Hernan"
	aClassToRemove subclasses do: [ :aSubclassToRemove | self removeWithAllSubclasses: aSubclassToRemove ].
	aClassToRemove removeFromSystem.
	
]
