'From Cuis7.9 [latest update: #8038] on 2 July 2026 at 1:00:57 pm'!

!CodePackage methodsFor: 'saving' stamp: 'jmv 2/Jul/2026, 5:00:34 pm (UTC)'!
save
	"If we can't save, find a new destination directory."
	fullFileName ifNotNil: [
		fullFileName asFullFileEntry parent exists ifFalse: [
			fullFileName := nil ]].

	"If we were never saved, or never saved since image was moved, or target directory disappeared,
	then ask where to save, suggesting the NewPackages folder."
	fullFileName ifNil: [
		fullFileName :=
			self request: 'Save on path: '
				initialAnswer: (DirectoryEntry newPackagesDirectory // self packageFileName) pathName
				orCancel: [ ^self ]].

	"If autoNumberPackages is set, back up the current package file."
	(Preferences at: #autoNumberPackages) ifTrue: [ | backUpName oldFile directory |
		oldFile := fullFileName asFullFileEntry.
		oldFile exists ifTrue: [
			directory := oldFile parent.
			backUpName := directory
				nextNameFor: oldFile nameWithoutExtension
				coda: (Utilities authorInitialsPerSe ifNotNil: [ :ai | '-', ai] ifNil: ['']), '.pck'
				extension: 'st'.
			oldFile rename: backUpName ]].

	fullFileName asFullFileEntry forceWriteStreamDo: [ :stream |
		stream writeSystemTimeStamp.
		self writeOnStream: stream ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self).! !

