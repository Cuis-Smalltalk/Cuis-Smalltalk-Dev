'From Cuis7.7 [latest update: #7951] on 19 May 2026 at 4:07:32 pm'!
!classDefinition: #CodePackage category: #'System-Package Support'!
ActiveModel subclass: #CodePackage
	instanceVariableNames: 'packageName methodCategoryPrefix fullFileName hasUnsavedChanges description sourceSystem featureSpec '
	classVariableNames: 'InstalledPackages '
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #ChangeSet category: #'Tools-Changes'!
ActiveModel subclass: #ChangeSet
	instanceVariableNames: 'name preamble postscript changeRecords structures superclasses hasUnsavedChanges isForBaseSystem codePackage '
	classVariableNames: 'AllChangeSets CurrentBaseCSNumber Installing '
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #CodePackage category: #'System-Package Support'!
ActiveModel subclass: #CodePackage
	instanceVariableNames: 'packageName methodCategoryPrefix fullFileName hasUnsavedChanges description sourceSystem featureSpec'
	classVariableNames: 'InstalledPackages'
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #ChangeSet category: #'Tools-Changes'!
ActiveModel subclass: #ChangeSet
	instanceVariableNames: 'name preamble postscript changeRecords structures superclasses hasUnsavedChanges isForBaseSystem codePackage'
	classVariableNames: 'AllChangeSets CurrentBaseCSNumber Installing'
	poolDictionaries: ''
	category: 'Tools-Changes'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

ChangeSet allSubInstances do: [ :each | each instVarNamed: 'actionMap' put: (ActiveModel actionMapFor: each). ActiveModel releaseActionMapFor: each ].
CodePackage allSubInstances do: [ :each | each instVarNamed: 'actionMap' put: (ActiveModel actionMapFor: each). ActiveModel releaseActionMapFor: each ].!

