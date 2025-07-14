'From Cuis7.5 [latest update: #7346] on 11 July 2025 at 5:00:08 pm'!
!classDefinition: #RenameInstanceVariable category: #'Tools-Refactoring'!
Refactoring subclass: #RenameInstanceVariable
	instanceVariableNames: 'oldVariable renamedReferences methodsAndRangesToChange newVariable classToRefactor originalClassToRefactor originalClassDefinition '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!Inspector methodsFor: 'private' stamp: 'jmv 7/11/2025 16:48:11'!
classDefinitionChangedFrom: oldClass to: newClass oldClassDefinition: oldClassDefinition

	self objectClass = newClass ifTrue: [self changed: #fieldList]! !


!ChangeSet methodsFor: 'change logging' stamp: 'jmv 7/11/2025 16:39:58'!
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
	self noteClassStructure: oldClass.
	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'system change notifications' stamp: 'jmv 7/11/2025 16:40:32'!
classDefinitionChangedFrom: oldClass to: newClass oldClassDefinition: oldClassDefinition

	(newClass hasChangedComparedTo: oldClass) ifTrue: [ 
		self noteChangeClass: newClass from: oldClass oldClassDefinition: oldClassDefinition ]! !


!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 7/11/2025 16:41:58'!
classDefinitionChangedFrom: oldClass to: newClass oldClassDefinition: oldClassDefinition
	"In case the class is moved from one package to another, both change sets should be affected.
	But there's no need to do it here, as #classRecategorized:from:to: is also called."

	| packageOrNil |
	
	newClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil := CodePackage packageOfClass: newClass ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet classDefinitionChangedFrom: oldClass to: newClass oldClassDefinition: oldClassDefinition ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !


!ClassChangeRecord methodsFor: 'definition' stamp: 'jmv 7/11/2025 16:38:29'!
notePriorClassDefinition: oldClassDefinition

	oldClassDefinition ifNil: [^ self].
	priorDefinition ifNil: [priorDefinition := oldClassDefinition]! !


!Inspector methodsFor: 'initialization' stamp: 'jmv 7/11/2025 16:56:40'!
initializeEvents

	SystemChangeNotifier uniqueInstance
		when: #classDefinitionChanged send: #classDefinitionChangedFrom:to:oldClassDefinition: to: self! !


!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'jmv 7/11/2025 16:45:42'!
logChange
		
	Smalltalk
		logChange: classToRefactor definition 
		preamble: classToRefactor definitionPreamble.
		
	ChangeSet
		classDefinitionChangedFrom: originalClassToRefactor
		to: classToRefactor
		oldClassDefinition: originalClassDefinition.! !

!RenameInstanceVariable methodsFor: 'applying' stamp: 'jmv 7/11/2025 16:45:12'!
apply

	originalClassDefinition := classToRefactor definition.
	originalClassToRefactor := classToRefactor copy.
	
	self 
		lookForMethodsReferencingOldVariable;
		changeInstanceVariableName;
		logChange;
		renameReferencesToOldVariable.
		
	^renamedReferences 
		! !

!classDefinition: #RenameInstanceVariable category: #'Tools-Refactoring'!
Refactoring subclass: #RenameInstanceVariable
	instanceVariableNames: 'oldVariable renamedReferences methodsAndRangesToChange newVariable classToRefactor originalClassToRefactor originalClassDefinition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Inspector allSubInstancesDo: [ :i | i initializeEvents ]!

