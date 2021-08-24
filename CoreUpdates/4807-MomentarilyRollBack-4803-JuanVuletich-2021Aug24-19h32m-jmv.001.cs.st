'From Cuis 5.0 [latest update: #4806] on 24 August 2021 at 7:33:30 pm'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 12/17/2018 10:05:42'!
installing: aCodePackageOrChangeSetName do: aBlock

	| currentCS currentCSNumber newHighestUpdate |
	Installing _ 'Install-', aCodePackageOrChangeSetName.
	aBlock ensure: [ Installing _ nil ].
	Smalltalk forceChangesToDisk.

	SystemVersion current registerUpdate: aCodePackageOrChangeSetName.
	newHighestUpdate _  SystemVersion current highestUpdate.

	currentCSNumber _ self currentBaseCSNumber.
	currentCS _ self changeSetForBaseSystem.
	currentCS isEmpty ifTrue: [
		ChangeSet removeChangeSet: currentCS.
		currentCSNumber > newHighestUpdate ifFalse: [
			CurrentBaseCSNumber _ newHighestUpdate + 1 ]]! !

