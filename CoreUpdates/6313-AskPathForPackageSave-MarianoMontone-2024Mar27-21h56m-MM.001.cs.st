'From Cuis6.3 [latest update: #6304] on 27 March 2024 at 10:19:52 pm'!

!CodePackage methodsFor: 'saving' stamp: 'MM 3/27/2024 22:16:05'!
save
	"If we can't save, find a new destination directory."
	fullFileName ifNotNil: [
		fullFileName asFullFileEntry parent exists ifFalse: [
			fullFileName := nil ]].

	"If we were never saved, or never saved since image was moved, or target directory disappeared, then save to image directory."
	fullFileName ifNil: [
		fullFileName :=
			self request: 'Save on path: ' initialAnswer:
				(DirectoryEntry newPackagesDirectory // self packageFileName) pathName ].

	fullFileName asFullFileEntry forceWriteStreamDo: [ :stream |
		stream timeStamp.
		self writeOnStream: stream ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self)! !

