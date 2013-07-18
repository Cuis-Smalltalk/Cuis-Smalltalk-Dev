'From Cuis 4.1 of 12 December 2012 [latest update: #1732] on 22 June 2013 at 7:23:45.450133 pm'!
!classDefinition: #ChangeSet category: #'Tools-Changes'!
Object subclass: #ChangeSet
	instanceVariableNames: 'name preamble postscript changeRecords structures superclasses hasUnsavedChanges isForBaseSystem codePackage '
	classVariableNames: 'CurrentBaseCSNumber Installing AllChangeSets '
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeSet class methodsFor: 'class initialization' stamp: 'jmv 6/22/2013 19:23'!
allChangeSets: aCollection
	"
	ChangeSet allChangeSets: ChangeSorter allChangeSets
	"
	AllChangeSets _ aCollection! !

!classDefinition: #ChangeSet category: #'Tools-Changes'!
Object subclass: #ChangeSet
	instanceVariableNames: 'name preamble postscript changeRecords structures superclasses hasUnsavedChanges isForBaseSystem codePackage'
	classVariableNames: 'AllChangeSets CurrentBaseCSNumber Installing'
	poolDictionaries: ''
	category: 'Tools-Changes'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
ChangeSet allChangeSets: ChangeSorter allChangeSets!

