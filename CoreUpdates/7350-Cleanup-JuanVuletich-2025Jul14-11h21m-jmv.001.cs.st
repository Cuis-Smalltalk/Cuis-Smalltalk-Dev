'From Cuis7.5 [latest update: #7349] on 14 July 2025 at 11:24:46 am'!

!ChangeSet methodsFor: 'change logging' stamp: 'jmv 7/14/2025 11:23:58'!
noteChangeClass: class from: oldClass oldClassDefinition: oldClassDefinition
	"Remember that a class definition has been changed.  Record the original structure, so that a conversion method can be built."

	class wantsChangeSetLogging ifFalse: [^ self].
	class isMeta 
		ifFalse: [self atClass: class add: #change]	"normal"
		ifTrue: [((self classChangeAt: class theNonMetaClass name) includes: #add) 
			ifTrue: [self atClass: class add: #add] 	"When a class is defined, the metaclass
				is not recorded, even though it was added.  A further change is
				really just part of the original add."
			ifFalse: [self atClass: class add: #change]].
	self addCoherency: class name.
	(self changeRecorderFor: class) notePriorClassDefinition: oldClassDefinition.
	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'change logging' stamp: 'jmv 7/14/2025 11:24:07'!
noteRenameClass: class as: newName 
	"Include indication that a class has been renamed."

	| recorder |
	(recorder := self changeRecorderFor: class)
		noteChangeType: #rename;
		noteNewName: newName asSymbol.
		
	"store under new name (metaclass too)"
	changeRecords at: newName put: recorder.
	changeRecords removeKey: class name.

	recorder := changeRecords at: class class name ifAbsent: nil.
	recorder ifNotNil: [
		changeRecords at: (newName, ' class') put: recorder.
		changeRecords removeKey: class class name.
		recorder noteNewName: newName , ' class' ].

	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'moving changes' stamp: 'jmv 7/14/2025 11:23:19'!
removeClassChanges: classOrClassName
	"Remove all memory of changes associated with this class"
	| cname |
	cname := classOrClassName isString
		ifTrue: [ classOrClassName ]
		ifFalse: [ classOrClassName name ].

	changeRecords removeKey: cname ifAbsent: nil.! !

!methodRemoval: ChangeSet #askAddedInstVars: stamp: 'jmv 7/14/2025 11:21:54'!
ChangeSet removeSelector: #askAddedInstVars:!
!methodRemoval: ChangeSet #askRemovedInstVars: stamp: 'jmv 7/14/2025 11:22:04'!
ChangeSet removeSelector: #askRemovedInstVars:!
!methodRemoval: ChangeSet #askRenames:addTo:using: stamp: 'jmv 7/14/2025 11:22:13'!
ChangeSet removeSelector: #askRenames:addTo:using:!
!methodRemoval: ChangeSet #noteClassForgotten: stamp: 'jmv 7/14/2025 11:23:22'!
ChangeSet removeSelector: #noteClassForgotten:!
!methodRemoval: ChangeSet #noteClassStructure: stamp: 'jmv 7/14/2025 11:24:17'!
ChangeSet removeSelector: #noteClassStructure:!
