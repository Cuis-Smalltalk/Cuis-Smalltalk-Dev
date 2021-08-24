'From Cuis 5.0 [latest update: #4799] on 24 August 2021 at 12:01:35 pm'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 8/24/2021 11:41:19'!
installing: aCodePackageOrChangeSetName do: aBlock

	| currentCS currentCSNumber newHighestUpdate |
	SystemChangeNotifier uniqueInstance
		doSilently: [
			Installing _ 'Install-', aCodePackageOrChangeSetName.
			aBlock ensure: [ Installing _ nil ]].
	CodeWindow allSubInstancesDo: [ :w | w updateListsAndCode ].
	Smalltalk forceChangesToDisk.

	SystemVersion current registerUpdate: aCodePackageOrChangeSetName.
	newHighestUpdate _  SystemVersion current highestUpdate.

	currentCSNumber _ self currentBaseCSNumber.
	currentCS _ self changeSetForBaseSystem.
	currentCS isEmpty ifTrue: [
		ChangeSet removeChangeSet: currentCS.
		currentCSNumber > newHighestUpdate ifFalse: [
			CurrentBaseCSNumber _ newHighestUpdate + 1 ]]! !

