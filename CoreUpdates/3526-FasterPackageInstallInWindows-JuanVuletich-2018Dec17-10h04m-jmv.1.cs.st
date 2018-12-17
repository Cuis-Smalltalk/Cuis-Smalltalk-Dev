'From Cuis 5.0 of 7 November 2016 [latest update: #3512] on 17 December 2018 at 10:09:21 am'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 12/17/2018 10:09:17'!
forceChangesToDisk
	"Ensure that the changes file has been fully written to disk by closing and re-opening it. This makes the system more robust in the face of a power failure or hard-reboot."

	| changesFile |
	"Expensive and not worth doing, esp. in Windows with antivirus active, when installing large packages"
	ChangeSet notInstallOrTestRun ifTrue: [
		changesFile _ SourceFiles at: 2.
		changesFile isFileStream ifTrue: [
			changesFile flush.
			changesFile close.
			changesFile open: changesFile name forWrite: true.
			changesFile setToEnd.
		].
	]! !


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


!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 12/17/2018 10:08:59'!
addSelector: aSymbol method: aCompiledMethod allImplemented: implemented

	| sentValue value |
	self protected: [
		value _ ChangeSet notInstallOrTestRun
			ifTrue: [
				"Expensive and not worth doing, esp. in Windows with antivirus active, when installing large packages"
				aCompiledMethod dateSortingValue ]
			ifFalse: [ 0 ].
		Selectors at: aSymbol put: (value  max: (Selectors at: aSymbol ifAbsent: [0])).
		aCompiledMethod messages do: [ :sentMsg |
			((Selectors includesKey: sentMsg) or: [ 
				implemented
					ifNotNil: [ implemented includes: sentMsg ]
					ifNil: [ Smalltalk isThereAnImplementorOf: sentMsg ]])
						ifTrue: [
							sentValue _ value max: (Selectors at: sentMsg ifAbsent: [0]).
							Selectors at: sentMsg put: sentValue ]]]! !

