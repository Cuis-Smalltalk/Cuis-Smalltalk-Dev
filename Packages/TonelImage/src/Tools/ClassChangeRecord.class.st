"
A ClassChangeRecorder keeps track of most substantive changes premissible in a project, isolated or not.

Structure:
inForce		a boolean
			Tells whether these changes are in effect.
			true for all changeSets in and above the current project.
			It should be sufficient only to record this for the changeSet
			as a whole, but this redundancy could help in error recovery.
classIsLocal	a boolean
			True if and only if this class is defined in this layer of the
			project structure.
changeTypes an identitySet
			Summarizes which changes have been made in this class.
			Values include #comment, #reorganize, #rename,
			and the four more summarized below.
thisName	a string
			Retains the class name for this layer.
priorName	a string
			Preserves the prior name.
thisComment	a text
			Retains the class comment for this layer.
priorComment	a text
			Preserves the prior comment.
thisOrganization	a classOrganizer
			Retains the class organization for this layer.
priorOrganization	a classOrganizer
			Preserves the prior organization.
thisMD	a methodDictionary
			Used to prepare changes for nearly atomic invocation
			of this layer (see below).
priorMD	a methodDictionary
			Preserves the state of an altered class as it exists in the next
			outer layer of the project structure.
methodChanges		a dictionary of classChangeRecords
			Retains all the method changes for this layer.

Four of the possible changeTypes are maintained in a mutually exclusive set, analogously to MethodChangeRecords.  Here is a simple summary of the relationship between these four changeType symbols and the recording of prior state
			|	prior == nil			|	prior not nil	
	---------	|----------------------------	|--------------------
	add		|	add					|	change
	---------	|----------------------------	|--------------------
	remove	|	addedThenRemoved	|	remove

A classChangeRecorder is notified of changes by the method
		noteMethodChange: <ClassChangeRecord>.
ClassChangeRecorders are designed to invoke a set of changes relative to the definition of a class in an prior layer.  It is important that both invocation and revocation of these changes take place in a nearly atomic fashion so that interdependent changes will be adopted as a whole, and so that only one flush of the method cache should be necessary.  A further reason for revocation to be simple is that it may be requested as an attempt to recover from an error in a project that is failing.
"
Class {
	#name : 'ClassChangeRecord',
	#superclass : 'Object',
	#instVars : [
		'inForce',
		'changeTypes',
		'priorDefinition',
		'thisName',
		'priorName',
		'methodChanges',
		'stamp'
	],
	#category : 'Tools-Changes',
	#package : 'Tools',
	#tag : 'Changes'
}

{ #category : 'all changes' }
ClassChangeRecord >> allChangeTypes [

	| chgs |
	(priorName notNil and: [changeTypes includes: #rename]) ifTrue:
		[(chgs := changeTypes copy) add: 'oldName: ' , priorName.
		^ chgs].
	^ changeTypes
]

{ #category : 'method changes' }
ClassChangeRecord >> atSelector: selector ifAbsent: absentBlock [

	^ (methodChanges at: selector ifAbsent: absentBlock)
		changeType
]

{ #category : 'method changes' }
ClassChangeRecord >> atSelector: selector put: changeType [

	(self findOrMakeMethodChangeAt: selector priorMethod: nil)
		noteChangeType: changeType
]

{ #category : 'method changes' }
ClassChangeRecord >> changedSelectors [
	"Return a set of the changed or removed selectors."

	^ methodChanges keys asSet
]

{ #category : 'definition' }
ClassChangeRecord >> checkCoherence [
	"If I recreate the class then don't remove it"

	(changeTypes includes: #remove) ifTrue: [
		changeTypes remove: #remove.
		changeTypes add: #change ].
	(changeTypes includes: #addedThenRemoved) ifTrue: [
		changeTypes remove: #addedThenRemoved.
		changeTypes add: #add ].
	(changeTypes includes: #movedToOtherPackage) ifTrue: [
		changeTypes remove: #movedToOtherPackage.
		changeTypes add: #add ].
]

{ #category : 'method changes' }
ClassChangeRecord >> findOrMakeMethodChangeAt: selector priorMethod: priorMethod [

	^ methodChanges 
		at: selector
		ifAbsentPut: [MethodChangeRecord new priorMethod: priorMethod]
]

{ #category : 'all changes' }
ClassChangeRecord >> hasNoChanges [

	^ changeTypes isEmpty and: [methodChanges isEmpty]
]

{ #category : 'all changes' }
ClassChangeRecord >> includesChangeType: changeType [

	changeType == #new ifTrue: [^ changeTypes includes: #add].  "Backwd compat"
	^ changeTypes includes: changeType
]

{ #category : 'method changes' }
ClassChangeRecord >> infoFromRemoval: selector [

	^ (methodChanges at: selector ifAbsent: [^ nil])
		methodInfoFromRemoval


]

{ #category : 'initialization' }
ClassChangeRecord >> initFor: className revertable: isRevertable [

	inForce := isRevertable.
	changeTypes := IdentitySet new.
	methodChanges := IdentityDictionary new.
	priorName := thisName := className
]

{ #category : 'removal' }
ClassChangeRecord >> isClassMoveToOtherPackage [
	"NOTE: there are other removals with changeType #addedThenRemoved,
	but this message is used to write out removals in fileOut, and those
	cases should not be written out."

	^ changeTypes includes: #movedToOtherPackage
]

{ #category : 'removal' }
ClassChangeRecord >> isClassRemoval [
	"NOTE: there are other removals with changeType #addedThenRemoved,
	but this message is used to write out removals in fileOut, and those
	cases should not be written out."

	^ (changeTypes includes: #remove) or: [changeTypes includes: #removeClass]
]

{ #category : 'method changes' }
ClassChangeRecord >> methodChangeTypes [
	"Return an old-style dictionary of method change types."

	| dict |
	dict := IdentityDictionary new.
	methodChanges associationsDo:
		[:assn | | selector record |
		selector := assn key.
		record := assn value.
		dict at: selector put: record changeType].
	^ dict
]

{ #category : 'method changes' }
ClassChangeRecord >> methodChanges [

	^ methodChanges
]

{ #category : 'all changes' }
ClassChangeRecord >> noteChangeType: changeSymbol [

	^ self noteChangeType: changeSymbol fromClass: nil
]

{ #category : 'all changes' }
ClassChangeRecord >> noteChangeType: changeSymbol fromClass: class [

	stamp := Utilities changeStamp.
	
	changeSymbol == #movedToOtherPackage ifTrue: [
		^ changeTypes add: changeSymbol].
	"Any other change type meanse we're still here!"
	changeTypes remove: #movedToOtherPackage ifAbsent: nil.

	(changeSymbol == #new or: [changeSymbol == #add]) ifTrue: [
		changeTypes add: #add.
		changeTypes remove: #change ifAbsent: nil.
		^ self].
	changeSymbol == #change ifTrue: [
		(changeTypes includes: #add) ifTrue: [^ self].
		^ changeTypes add: changeSymbol].
	changeSymbol == #addedThenRemoved ifTrue: [
		^ self].  "An entire class was added but then removed"
	changeSymbol == #comment ifTrue: [
		^ changeTypes add: changeSymbol].
	changeSymbol == #reorganize ifTrue: [
		^ changeTypes add: changeSymbol].
	changeSymbol == #rename ifTrue: [
		^ changeTypes add: changeSymbol].
	(changeSymbol beginsWith: 'oldName: ') ifTrue: [
		"Must only be used when assimilating other changeSets"
		(changeTypes includes: #add) ifTrue: [^ self].
		priorName := changeSymbol copyFrom: 'oldName: ' size + 1 to: changeSymbol size.
		^ changeTypes add: #rename].
	changeSymbol == #remove ifTrue: [
		(changeTypes includes: #add)
			ifTrue: [changeTypes add: #addedThenRemoved]
			ifFalse: [changeTypes add: #remove].
		^ changeTypes removeAllFoundIn: #(add change comment reorganize)].

	self error: 'Unrecognized changeType'
]

{ #category : 'method changes' }
ClassChangeRecord >> noteMethodMoveToOtherPackage: selector [

	| methodChange |
	methodChange := self findOrMakeMethodChangeAt: selector priorMethod: nil.
	methodChange noteChangeType: #movedToOtherPackage
]

{ #category : 'method changes' }
ClassChangeRecord >> noteNewMethod: newMethod selector: selector priorMethod: methodOrNil [

	| methodChange |
	methodChange := self findOrMakeMethodChangeAt: selector priorMethod: methodOrNil.
	methodOrNil
		ifNil: [ methodChange noteChangeType: #add ]
		ifNotNil: [ methodChange noteChangeType: #change ].

]

{ #category : 'rename' }
ClassChangeRecord >> noteNewName: newName [

	thisName := newName
]

{ #category : 'definition' }
ClassChangeRecord >> notePriorClassDefinition: oldClassDefinition [

	oldClassDefinition ifNil: [^ self].
	priorDefinition ifNil: [priorDefinition := oldClassDefinition]
]

{ #category : 'method changes' }
ClassChangeRecord >> noteRemoveSelector: selector priorMethod: priorMethod lastMethodInfo: infoOrNil [

	| methodChange |
	methodChange := self findOrMakeMethodChangeAt: selector priorMethod: priorMethod.
	methodChange changeType == #add
		ifTrue: [methodChange noteChangeType: #addedThenRemoved]
		ifFalse: [methodChange noteChangeType: #remove].

	infoOrNil ifNotNil:
		["Save the source code pointer and category so can still browse old versions"
		methodChange noteMethodInfoFromRemoval: infoOrNil]


]

{ #category : 'all changes' }
ClassChangeRecord >> noteRemoved: class fromCategory: aCategoryName [

	priorDefinition := class definitionReplacingCategoryWith: aCategoryName.
	self noteChangeType: #remove fromClass: class

]

{ #category : 'definition' }
ClassChangeRecord >> priorDefinition [

	^ priorDefinition
]

{ #category : 'rename' }
ClassChangeRecord >> priorName [

	^ priorName
]

{ #category : 'isolation layers' }
ClassChangeRecord >> realClass [
	"Return the actual class (or meta), as determined from my name."

	thisName ifNil: [^ nil].
	(thisName endsWith: ' class')
		ifTrue: [^ (Smalltalk at: (thisName copyFrom: 1 to: thisName size - 6) asSymbol
						ifAbsent: [^ nil]) class]
		ifFalse: [^ Smalltalk at: thisName ifAbsent: [^ nil]]
]

{ #category : 'method changes' }
ClassChangeRecord >> removeSelector: selector [
	"Remove all memory of changes associated with the argument, selector, in this class."

	selector == #Comment
		ifTrue:
			[changeTypes remove: #comment ifAbsent: nil]
		ifFalse:
			[methodChanges removeKey: selector ifAbsent: nil]
]

{ #category : 'stamp' }
ClassChangeRecord >> stamp [
	
	^stamp
]

{ #category : 'rename' }
ClassChangeRecord >> thisName [

	^ thisName
]

{ #category : 'all changes' }
ClassChangeRecord >> trimHistory [
	"Drop non-essential history."

	"Forget methods added and later removed"
	methodChanges keysAndValuesRemove:
		[:sel :chgRecord | chgRecord changeType == #addedThenRemoved].

	"Forget renaming and reorganization of newly-added classes."
	(changeTypes includes: #add) ifTrue:
		[changeTypes removeAllFoundIn: #(rename reorganize)].

]
