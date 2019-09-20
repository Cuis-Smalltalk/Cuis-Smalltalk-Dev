'From Cuis 5.0 [latest update: #3852] on 28 August 2019 at 11:20:43 pm'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'pb 8/28/2019 23:19:53'!
forceChangesToDisk
	"Just flush the buffer and trust the OS to do its job."
	| changesFile |
	"Expensive and not worth doing, esp. in Windows with antivirus active, when installing large packages"
	ChangeSet notInstallOrTestRun ifTrue: [
		changesFile _ SourceFiles at: 2.
		changesFile isFileStream ifTrue: [ changesFile flush ]].! !

