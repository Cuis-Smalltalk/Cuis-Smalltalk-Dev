'From Cuis 4.1 of 12 December 2012 [latest update: #1733] on 22 June 2013 at 8:12:23.151727 pm'!

!ChangeSet class methodsFor: 'enumerating' stamp: 'jmv 6/22/2013 19:24'!
allChangeSets
	"Return the list of all current ChangeSets"

	^ AllChangeSets! !

!ChangeSet class methodsFor: 'enumerating' stamp: 'jmv 6/22/2013 19:35'!
allChangeSetsWithClass: class selector: selector
	class ifNil: [^ #()].
	^ self allChangeSets select: [ :cs |
		(cs atSelector: selector class: class) ~~ #none]! !

!ChangeSet class methodsFor: 'enumerating' stamp: 'jmv 6/22/2013 19:37'!
changeSetNamed: aName
	"Return the change set of the given name, or nil if none found."

	^ AllChangeSets
		detect: [:aChangeSet | aChangeSet name = aName]
		ifNone: nil! !

!ChangeSet class methodsFor: 'enumerating' stamp: 'jmv 6/22/2013 19:26'!
countOfChangeSetsWithClass: aClass andSelector: aSelector
	"Answer how many change sets record a change for the given class and selector"

	^ (self allChangeSetsWithClass: aClass selector: aSelector) size! !

!ChangeSet class methodsFor: 'enumerating' stamp: 'jmv 6/22/2013 19:26'!
doesAnyChangeSetHaveClass: aClass andSelector: aSelector
	"Answer whether any known change set bears a change for the given class and selector"

	^ (self countOfChangeSetsWithClass: aClass andSelector: aSelector) > 0! !

!ChangeSet class methodsFor: 'enumerating' stamp: 'jmv 6/22/2013 19:46'!
existingOrNewChangeSetForPackage: aCodePackage
	| answer |
	answer _ ChangeSet
		existingOrNewChangeSetNamed: 'UnsavedChangesTo-' , aCodePackage name
		forBaseSystem: false.
	answer codePackage: aCodePackage.
	^answer! !

!ChangeSet class methodsFor: 'enumerating' stamp: 'jmv 6/22/2013 19:38'!
existingOrNewChangeSetNamed: aName forBaseSystem: aBoolean

	| newSet |
	^(ChangeSet changeSetNamed: aName)
		ifNotNil: [ :existing |
			existing assert: existing isForBaseSystem = aBoolean ]
		ifNil: [
			newSet _ ChangeSet basicNewNamed: aName.
			newSet isForBaseSystem: aBoolean.
			AllChangeSets add: newSet.
			newSet ]! !

!ChangeSet class methodsFor: 'removing' stamp: 'jmv 6/22/2013 19:40'!
removeChangeSet: aChangeSet
	"Remove the given changeSet.  Caller must assure that it's cool to do this"

	"If the change set to be removed was using the current number for base system changes,
	and it is not empty, start using the following number for the upcoming changes."
	(aChangeSet name initialIntegerOrNil = ChangeSet currentBaseCSNumber and: [
		aChangeSet isEmpty not ])
			ifTrue: [
				ChangeSet incrementCurrentBaseCSNumber ].

	AllChangeSets remove: aChangeSet ifAbsent: nil.
	aChangeSet wither! !

!ChangeSet class methodsFor: 'removing' stamp: 'jmv 6/22/2013 19:42'!
zapAllChangeSets
	"Keep those change sets that are recording the unsaved changes of some package"
	AllChangeSets copy do: [ :changeSet |
		(changeSet codePackage notNil and: [changeSet isEmpty not]) ifFalse: [
			self removeChangeSet: changeSet ]]! !


!ChangeSorter class methodsFor: 'enumerating' stamp: 'jmv 6/22/2013 20:10'!
allChangeSets
	"Return the list of all current ChangeSets"

	^ ChangeSet allChangeSets! !

!ChangeSorter class methodsFor: 'enumerating' stamp: 'jmv 6/22/2013 20:10'!
allChangeSetsWithClass: class selector: selector
^ChangeSet allChangeSetsWithClass: class selector: selector! !

!ChangeSorter class methodsFor: 'enumerating' stamp: 'jmv 6/22/2013 20:10'!
changeSetNamed: aName
		^ChangeSet changeSetNamed: aName! !

!ChangeSorter class methodsFor: 'enumerating' stamp: 'jmv 6/22/2013 20:11'!
existingOrNewChangeSetForPackage: aCodePackage
	^ChangeSet existingOrNewChangeSetForPackage: aCodePackage! !

!ChangeSorter class methodsFor: 'enumerating' stamp: 'jmv 6/22/2013 20:11'!
existingOrNewChangeSetNamed: aName forBaseSystem: aBoolean
		^ChangeSet existingOrNewChangeSetNamed: aName forBaseSystem: aBoolean! !

!ChangeSorter class methodsFor: 'enumerating' stamp: 'jmv 6/22/2013 20:11'!
mostRecentChangeSetWithChangeForClass: class selector: selector
	^ChangeSet mostRecentChangeSetWithChangeForClass: class selector: selector! !

!ChangeSorter class methodsFor: 'removing' stamp: 'jmv 6/22/2013 20:11'!
zapAllChangeSets
	^ChangeSet zapAllChangeSets! !

!ChangeSorter class methodsFor: 'services' stamp: 'jmv 6/22/2013 20:10'!
countOfChangeSetsWithClass: aClass andSelector: aSelector
	^ChangeSet countOfChangeSetsWithClass: aClass andSelector: aSelector! !

!ChangeSorter class methodsFor: 'services' stamp: 'jmv 6/22/2013 20:10'!
doesAnyChangeSetHaveClass: aClass andSelector: aSelector
	^ChangeSet doesAnyChangeSetHaveClass: aClass andSelector: aSelector! !

