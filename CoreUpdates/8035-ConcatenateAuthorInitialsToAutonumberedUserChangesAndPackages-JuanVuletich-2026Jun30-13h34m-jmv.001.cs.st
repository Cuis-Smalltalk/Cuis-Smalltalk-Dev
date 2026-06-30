'From Cuis7.9 [latest update: #8034] on 30 June 2026 at 1:37:48 pm'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 30/Jun/2026, 5:35:03 pm (UTC)'!
userChangesFileName
	"
	Smalltalk userChangesFileName
	"
	^(FileIOAccessor default baseNameFor: self imageName pathAndLocalName second),
		'-', Utilities authorInitials,
		(Preferences at: #userChangesFileNameExtension)! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 30/Jun/2026, 5:23:13 pm (UTC)'!
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
				coda: '-', Utilities authorInitials, '.pck'
				extension: 'st'.
			oldFile rename: backUpName ]].

	fullFileName asFullFileEntry forceWriteStreamDo: [ :stream |
		stream writeSystemTimeStamp.
		self writeOnStream: stream ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self).! !

