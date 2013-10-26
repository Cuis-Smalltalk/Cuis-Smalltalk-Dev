'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 22 March 2012 at 10:07:17 am'!
!classDefinition: 'ChangeSet class' category: #'Tools-Changes'!
ChangeSet class
	instanceVariableNames: 'current installing '!
!methodRemoval: ChangeSet class #newChanges:!
ChangeSet class removeSelector: #newChanges:!
!classDefinition: 'ChangeSet class' category: #'Tools-Changes'!
ChangeSet class
	instanceVariableNames: 'installing'!

!ChangeSet class reorganize!
('class initialization' initialize)
('defaults' defaultChangeSetDirectory)
('fileIn/Out' doWeFileOut:given:cache: superclassOrder:)
('instance creation' basicNewNamed: new)
('system change notifications' classAdded:inCategory: classCommented: classDefinitionChangedFrom:to: classRecategorized:from:to: classRemoved:fromCategory: classRenamed:from:to:inCategory: classReorganized: methodAdded:selector:inClass:requestor: methodAdded:selector:inProtocol:class:requestor: methodChangedFrom:to:selector:inClass:requestor: methodRemoved:selector:inProtocol:class: selectorRecategorized:from:to:inClass:)
('change set to use' changeSetForBaseSystem changeSetForClass: changeSetForMethod: changeSetForMethodCategory:ofClass: changeSetForPackage: changeSetForSystemCategory: installing:do:)
!

