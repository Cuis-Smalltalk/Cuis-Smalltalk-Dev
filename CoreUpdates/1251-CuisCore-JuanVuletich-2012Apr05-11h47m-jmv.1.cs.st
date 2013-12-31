'From Cuis 4.0 of 3 April 2012 [latest update: #1250] on 5 April 2012 at 11:59:49 am'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 4/5/2012 11:58'!
installing: aCodePackageOrChangeSetName do: aBlock

	| currentCS currentCSNumber newHighestUpdate |
	Installing _ aCodePackageOrChangeSetName.
	aBlock ensure: [ Installing _ nil ].

	SystemVersion current registerUpdate: aCodePackageOrChangeSetName.
	newHighestUpdate _  SystemVersion current highestUpdate.

	currentCSNumber _ self currentBaseCSNumber.
	currentCS _ self changeSetForBaseSystem.
	currentCS isEmpty ifTrue: [
		ChangeSorter removeChangeSet: currentCS.
		currentCSNumber > newHighestUpdate ifFalse: [
			CurrentBaseCSNumber _ newHighestUpdate + 1 ]]! !


!SystemVersion methodsFor: 'accessing' stamp: 'jmv 4/5/2012 11:52'!
registerUpdate: changeSetOrPackageName
	highestUpdate _ self highestUpdate max: changeSetOrPackageName initialIntegerOrNil.
	updates add: changeSetOrPackageName! !

!methodRemoval: SystemVersion #resetHighestUpdate!
SystemVersion removeSelector: #resetHighestUpdate!
!methodRemoval: ChangeSet class #currentBaseCSNumber:!
ChangeSet class removeSelector: #currentBaseCSNumber:!
