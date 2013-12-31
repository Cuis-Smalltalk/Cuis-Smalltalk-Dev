'From Cuis 4.0 of 3 April 2012 [latest update: #1241] on 5 April 2012 at 9:42:12 am'!
!classDefinition: #ChangeSet category: #'Tools-Changes'!
Object subclass: #ChangeSet
	instanceVariableNames: 'name preamble postscript changeRecords structures superclasses '
	classVariableNames: 'Installing LastUsedNumber CurrentBaseCSNumber '
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 4/5/2012 09:35'!
currentBaseCSNumber
	CurrentBaseCSNumber ifNil: [
		CurrentBaseCSNumber _ SystemVersion current highestUpdate + 1 ].
	^CurrentBaseCSNumber! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 4/5/2012 09:34'!
currentBaseCSNumber: aNumber
	CurrentBaseCSNumber _ aNumber! !


!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 4/5/2012 09:42'!
changeSetForBaseSystem

	| csName thisNumber numberToUse |
	Installing

		ifNil: [
			numberToUse _ self currentBaseCSNumber.
			ChangeSorter allChangeSets
				detect: [ :any | any name initialIntegerOrNil = numberToUse ]
				ifFound: [ :existing | ^existing ]
				ifNone: [
					csName _ (self baseSystemNameFor: numberToUse),
						(String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ]).
					^ChangeSorter existingOrNewChangeSetNamed: csName ]]

		ifNotNil: [
			thisNumber _ Installing initialIntegerOrNil.
			thisNumber = (self lastUsedNumber + 1) ifTrue: [
				LastUsedNumber _ thisNumber ].
			csName _ 'Affects-BaseSystem--Install-', Installing.
			^ChangeSorter existingOrNewChangeSetNamed: csName ]! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 4/5/2012 09:32'!
installing: aCodePackageOrChangeSetName do: aBlock

	Installing _ aCodePackageOrChangeSetName.
	aBlock ensure: [ Installing _ nil ].
	SystemVersion current registerUpdate: aCodePackageOrChangeSetName! !


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 4/5/2012 09:37'!
remove
	"Completely destroy my change set.  Check if it's OK first"

	| currentNumber |
	currentNumber _ myChangeSet isForBaseSystem ifTrue: [ myChangeSet name initialIntegerOrNil ].
	self removePrompting: true.

	"If the ChangeSet we're destroying was using the next Cuis Core change set number,
	assume that the user wants a new change set that reuses that number."
	currentNumber ifNotNil: [
		ChangeSet lastUsedNumber = currentNumber ifTrue: [
			ChangeSet lastUsedNumber: currentNumber - 1 ]].

	self showChangeSet: ChangeSet changeSetForBaseSystem.
	self update! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 4/5/2012 09:15'!
removePrompting: doPrompt
	"Completely destroy my change set.  Check if it's OK first, and if doPrompt is true, get the user to confirm his intentions first."

	| message aName changeSetNumber msg |

	"Tiene sentido? Preguntar cosas? Sugerir hacer fileOut?"
	self flag: #ojo.

	aName _ myChangeSet name.
	myChangeSet okayToRemove ifFalse: [^ self]. "forms current changes for some project"
	(myChangeSet isEmpty or: [doPrompt not]) ifFalse:
		[message _ 'Are you certain that you want to 
remove (destroy) the change set
named  "', aName, '" ?'.
		(self confirm: message) ifFalse: [^ self]].

	doPrompt ifTrue:
		[msg _ myChangeSet hasPreamble
			ifTrue:
				[myChangeSet hasPostscript
					ifTrue:
						['a preamble and a postscript']
					ifFalse:
						['a preamble']]
			ifFalse:
				[myChangeSet hasPostscript
					ifTrue:
						['a postscript']
					ifFalse:
						['']].
		msg isEmpty ifFalse:
			[(self confirm: 
'Caution!!  This change set has
', msg, ' which will be
lost if you destroy the change set.
Do you really want to go ahead with this?') ifFalse: [^ self]]].

	"Go ahead and remove the change set"
	false ifTrue: [
		changeSetNumber _ myChangeSet name initialIntegerOrNil.
		changeSetNumber ifNotNil: [SystemVersion current unregisterUpdate: changeSetNumber]].
	ChangeSorter removeChangeSet: myChangeSet.! !


!SystemVersion methodsFor: 'accessing' stamp: 'jmv 4/5/2012 09:30'!
highestUpdate
	highestUpdate ifNil: [
		highestUpdate _ updates
			ifEmpty: [ 0 ]
			ifNotEmpty: [
				updates detectMax: [ :updateName | updateName initialIntegerOrNil ]]].
	^ highestUpdate.! !

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 4/5/2012 09:27'!
registerUpdate: changeSetOrPackageName
	updates add: changeSetOrPackageName.
	self resetHighestUpdate! !

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 4/5/2012 09:28'!
unregisterUpdate: update
	updates remove: update ifAbsent: nil! !

!methodRemoval: SystemVersion #includesUpdate:!
SystemVersion removeSelector: #includesUpdate:!
!classDefinition: #ChangeSet category: #'Tools-Changes'!
Object subclass: #ChangeSet
	instanceVariableNames: 'name preamble postscript changeRecords structures superclasses'
	classVariableNames: 'CurrentBaseCSNumber Installing LastUsedNumber'
	poolDictionaries: ''
	category: 'Tools-Changes'!
