'From Cuis 4.0 of 3 April 2012 [latest update: #1241] on 5 April 2012 at 9:49:57 am'!
!classDefinition: #ChangeSet category: #'Tools-Changes'!
Object subclass: #ChangeSet
	instanceVariableNames: 'name preamble postscript changeRecords structures superclasses '
	classVariableNames: 'CurrentBaseCSNumber Installing LastUsedNumber '
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 4/5/2012 09:44'!
incrementCurrentBaseCSNumber.
	CurrentBaseCSNumber _ self currentBaseCSNumber +1! !


!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 4/5/2012 09:42'!
changeSetForBaseSystem

	| csName numberToUse |
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
			csName _ 'Affects-BaseSystem--Install-', Installing.
			^ChangeSorter existingOrNewChangeSetNamed: csName ]! !


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 4/5/2012 09:49'!
remove
	"Completely destroy my change set.  Check if it's OK first"

	self removePrompting: true.
	self showChangeSet: ChangeSet changeSetForBaseSystem.
	self update! !


!ChangeSorter class methodsFor: 'removing' stamp: 'jmv 4/5/2012 09:47'!
removeChangeSet: aChangeSet
	"Remove the given changeSet.  Caller must assure that it's cool to do this"

	"If the change set to be removed was using the current number for base system changes,
	and it is not empty, start using the following number for the upcoming changes."
	(aChangeSet name initialIntegerOrNil = ChangeSet currentBaseCSNumber and: [
		aChangeSet isEmpty not ])
			ifTrue: [
				ChangeSet incrementCurrentBaseCSNumber ].

	AllChangeSets remove: aChangeSet ifAbsent: nil.
	aChangeSet wither
! !

!methodRemoval: ChangeSet class #lastUsedNumber!
ChangeSet class removeSelector: #lastUsedNumber!
!methodRemoval: ChangeSet class #lastUsedNumber:!
ChangeSet class removeSelector: #lastUsedNumber:!
!classDefinition: #ChangeSet category: #'Tools-Changes'!
Object subclass: #ChangeSet
	instanceVariableNames: 'name preamble postscript changeRecords structures superclasses'
	classVariableNames: 'CurrentBaseCSNumber Installing'
	poolDictionaries: ''
	category: 'Tools-Changes'!
