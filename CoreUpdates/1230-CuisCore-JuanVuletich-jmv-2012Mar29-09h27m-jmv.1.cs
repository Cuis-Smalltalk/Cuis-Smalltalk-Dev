'From Cuis 4.0Alpha of 29 March 2012 [latest update: #1225] on 29 March 2012 at 9:30:26 am'!
!classDefinition: 'ChangeSet class' category: #'Tools-Changes'!
ChangeSet class
	instanceVariableNames: 'installing '!

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/29/2012 09:29'!
                  classDefinitionChangedFrom: oldClass to: newClass
	"In case the class is moved from one package to another, both change sets should be affected.
	But there's no need to do it here, as #classRecategorized:from:to: is also called."

	| packageOrNil |
	packageOrNil _ CodePackage packageOfClass: newClass ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet classDefinitionChangedFrom: oldClass to: newClass ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!classDefinition: 'ChangeSet class' category: #'Tools-Changes'!
ChangeSet class
	instanceVariableNames: ''!
