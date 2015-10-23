'From Cuis 4.2 of 25 July 2013 [latest update: #2554] on 23 October 2015 at 3:09:32.495846 pm'!

!CodePackage methodsFor: 'saving' stamp: 'jmv 10/23/2015 15:06'!
save
	"If we can't save, find a new destination directory."
	fullFileName ifNotNil: [
		fullFileName asFileEntry parent exists ifFalse: [
			fullFileName _ nil ]].

	"If we were never saved, or never saved since image was moved, or target directory disappeared, then save to image directory."
	fullFileName ifNil: [
		fullFileName _
			(FmDirectoryEntry smalltalkImageDirectory // self packageFileName) pathName ].

	fullFileName asFileEntry forceWriteStream: [ :stream |
		stream timeStamp.
		self writeOnStream: stream ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self)! !

