'From Cuis 5.0 [latest update: #3980] on 18 December 2019 at 6:39:08 pm'!
!classDefinition: #RenameInstanceVariable category: #'Tools-Refactoring'!
Refactoring subclass: #RenameInstanceVariable
	instanceVariableNames: 'oldVariable renamedReferences methodsAndRangesToChange newVariable classToRefactor originalClassToRefactor '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!Behavior methodsFor: 'testing' stamp: 'HAW 12/18/2019 18:18:28'!
hasChangedComparedTo: anotherClass

	^self superclass ~~ anotherClass superclass
		or: [ self instVarNames ~= anotherClass instVarNames
		or: [ self classVarNames ~= anotherClass classVarNames
		or: [ self sharedPools ~= anotherClass sharedPools ]]]! !


!ChangeSet methodsFor: 'testing' stamp: 'HAW 12/18/2019 18:37:51'!
isWithClass: aClass 
	
	^changeRecords includesKey: aClass name! !


!ChangeSet class methodsFor: 'enumerating' stamp: 'HAW 12/18/2019 18:37:51'!
allChangeSetsWithClass: aClass 
	
	^ AllChangeSets select: [ :aChangeSet | aChangeSet isWithClass: aClass ]! !


!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'HAW 12/18/2019 18:26:23'!
logChange
		
	Smalltalk
		logChange: classToRefactor definition 
		preamble: classToRefactor definitionPreamble.
		
	ChangeSet
		classDefinitionChangedFrom: originalClassToRefactor to: classToRefactor ! !


!ChangeSet methodsFor: 'system change notifications' stamp: 'HAW 12/18/2019 18:18:57'!
classDefinitionChangedFrom: oldClass to: newClass

	(newClass hasChangedComparedTo: oldClass) ifTrue: [ 
		self noteChangeClass: newClass from: oldClass ]! !


!RenameInstanceVariable methodsFor: 'applying' stamp: 'HAW 12/18/2019 18:26:00'!
apply

	originalClassToRefactor := classToRefactor copy.
	
	self 
		lookForMethodsReferencingOldVariable;
		changeInstanceVariableName;
		logChange;
		renameReferencesToOldVariable.
		
	^renamedReferences 
		! !

!methodRemoval: RenameInstanceVariable #logClassDefinition stamp: 'HAW 12/18/2019 18:26:00'!
RenameInstanceVariable removeSelector: #logClassDefinition!
!methodRemoval: ChangeSet class #allChangeSetWithClass: stamp: 'HAW 12/18/2019 18:37:39'!
ChangeSet class removeSelector: #allChangeSetWithClass:!
!methodRemoval: ChangeSet #isForClass: stamp: 'HAW 12/18/2019 18:37:51'!
ChangeSet removeSelector: #isForClass:!

!ChangeSet class reorganize!
('class initialization' initialize initializeNotificationActions registerNotificationActions removeNotificationActions)
('instance creation' named: new)
('system change notifications' aboutToRenameClass:from:to:inCategory: classAdded:inCategory: classCommented: classDefinitionChangedFrom:to: classRecategorized:from:to: classRemoved:fromCategory: classReorganized: methodAdded:selector:inClass:requestor: methodAdded:selector:inProtocol:class:requestor: methodChangedFrom:to:selector:inClass:requestor: methodRemoved:selector:inProtocol:class: selectorRecategorized:from:to:inClass:)
('change set to use' baseSystemNameFor: changeSetForBaseSystem changeSetForPackage: currentBaseCSNumber incrementCurrentBaseCSNumber installing:do: installingString isInstalling: notInstallOrTestRun runningTest:do:)
('file list services' fileReaderServicesForFile:suffix: serviceFileIn serviceInstall)
('services' fileIn: install: installNewUpdates installNewUpdates: isNewUpdate: newUpdates:)
('enumerating' allChangeSets allChangeSetsWithClass: allChangeSetsWithClass:selector: changeSetNamed: countOfChangeSetsWithClass:andSelector: doesAnyChangeSetHaveClass:andSelector: existingOrNewChangeSetForPackage: existingOrNewChangeSetNamed:forBaseSystem:)
('removing' removeChangeSet: zapAllChangeSets)
!

!classDefinition: #RenameInstanceVariable category: #'Tools-Refactoring'!
Refactoring subclass: #RenameInstanceVariable
	instanceVariableNames: 'oldVariable renamedReferences methodsAndRangesToChange newVariable classToRefactor originalClassToRefactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!RenameInstanceVariable reorganize!
('applying - private' changeInstanceVariableName keepMethodToChangeNamed:in: logChange lookForMethodsReferencingOldVariable lookForMethodsReferencingOldVariableIn: newSourceOf: renameReferencesToOldVariable renameReferencesToOldVariableInMethod:)
('applying' apply)
('initialization' initializeFrom:to:in:)
!

