'From Cuis 4.2 of 25 July 2013 [latest update: #1774] on 29 July 2013 at 10:53:48.009198 pm'!

!CodePackage class methodsFor: 'startup' stamp: 'jmv 7/29/2013 22:30'!
clearPaths
	InstalledPackages do: [ :a | a fullFileName: nil ]! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 7/29/2013 22:52'!
save
	| file |
	"If we can't save, find a new destination directory."
	fullFileName ifNotNil: [
		(FileDirectory on: (FileDirectory dirPathFor: fullFileName)) exists ifFalse: [
			fullFileName _ nil ]].

	"If we were never saved, or never saved since image was moved, or target directory disappeared, then save to default directory."
	fullFileName ifNil: [
		fullFileName _
			ChangeSet defaultChangeSetDirectory fullNameFor: (self packageName, FileDirectory dot, 'pck.st')].
		
	file _ FileStream forceNewFileNamed: fullFileName.
	[
		file timeStamp.
		self writeOnStream: file ]
			ensure: [ file close ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self)! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 7/29/2013 22:31'!
openSourceFiles
	self imageName = LastImageName ifFalse: [
		LastImageName _ self imageName.
		"Reset the author initials to blank when the image gets moved"
		Utilities clearAuthor.
		"And clear any absolute path that is most likely invalid now."
		CodePackage clearPaths ].
	"Warning: Do open the source files only if nil.
	If not nil, it is because they are internalized and the files should not be opened"
	self openSourcesAndChanges.
	CuisSourceFileArray install! !


!CodePackage class reorganize!
('packages access' deregister: installedPackages named:createIfAbsent:registerIfNew: register:)
('class initialization' initialize)
('searching' packageOfClass:ifNone: packageOfMethod:ifNone: packageOfMethodCategory:ofClass:ifNone: packageOfSystemCategory:ifNone:)
('startup' clearPaths)
!

