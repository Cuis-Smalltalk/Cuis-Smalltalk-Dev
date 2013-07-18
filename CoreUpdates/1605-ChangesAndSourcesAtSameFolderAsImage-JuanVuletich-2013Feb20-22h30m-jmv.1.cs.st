'From Cuis 4.1 of 12 December 2012 [latest update: #1604] on 20 February 2013 at 10:48:55 pm'!

!FileDirectory class methodsFor: 'system start up' stamp: 'jmv 2/20/2013 22:46'!
openSources: sourcesPathName andChanges: changesPathName
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems regarding write permissions or Lf/CrLf mixups."
	"Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sources changes msg wmsg |
	msg _ 'Squeak cannot locate &fileRef.

Please check that the file is named properly and is in the
same directory as this image.'.
	wmsg _ 'Squeak cannot write to &fileRef.

Please check that you have write permission for this file.

You won''t be able to save this image correctly until you fix this.'.

	"Do not open source files if internalized (i.e. notNil)"
	sources _ (SourceFiles at: 1) ifNil: [ StandardFileStream readOnlyFileNamed: sourcesPathName ].
	changes _ (SourceFiles at: 2) ifNil: [ StandardFileStream oldFileNamed: changesPathName ].

	(sources isNil and: [ Preferences valueOfFlag: #warnIfNoSourcesFile ])
		ifTrue: [
			Smalltalk platformName = 'Mac OS'
				ifTrue: [
					msg _ msg , String newLineString, 'Make sure the sources file is not an Alias.'].
			self inform: (msg copyReplaceAll: '&fileRef' with: 'the sources file named ' , sourcesPathName)].

	(changes isNil and: [ Preferences valueOfFlag: #warnIfNoChangesFile ])
		ifTrue: [self inform: (msg copyReplaceAll: '&fileRef' with: 'the changes file named ' , changesPathName)].

	((Preferences valueOfFlag: #warnIfNoChangesFile) and: [changes notNil])
		ifTrue: [
			changes isReadOnly
				ifTrue: [
					self inform: (wmsg copyReplaceAll: '&fileRef' with: 'the changes file named ' , changesPathName)].

			((changes next: 200)
					includesSubString: String crlfString)
				ifTrue: [
					self inform: 'The changes file named ' , changesPathName , '
has been injured by an unpacking utility.  Lfs were changed to CrLfs.
Please set the preferences in your decompressing program to 
"do not convert text files" and unpack the system again.']].

	SourceFiles _ Array with: sources with: changes! !


!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 2/20/2013 22:36'!
defaultChangesName
	"Answer the default full path to the changes file corresponding to the image file name."
	"
	Smalltalk defaultChangesName
	"
	^(FileDirectory baseNameFor: self imageName), FileDirectory dot, FileDirectory changeSuffix! !


!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 2/20/2013 22:31'!
startUp
	"Establish the platform-specific FileDirectory subclass. Do any platform-specific startup."
	self setDefaultDirectoryClass.
	self setDefaultDirectory: Smalltalk imagePath.
	Smalltalk openSourceFiles! !


!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 2/20/2013 22:37'!
defaultSourcesName
	"Answer the default full path to the sources file corresponding to the image file name."
	"
	Smalltalk defaultSourcesName
	"
	^ self imagePath, FileDirectory slash, SourceFileVersionString, FileDirectory dot, FileDirectory sourceSuffix! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 2/20/2013 22:35'!
localChangesName
	"Answer the local name for the changes file corresponding to the image file name."
	"Smalltalk localChangesName"

	| imName |
	imName _ FileDirectory baseNameFor:
		(FileDirectory localNameFor: self imageName).
	^ imName, FileDirectory dot, FileDirectory changeSuffix! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 2/20/2013 22:47'!
openSourceFiles
	self imageName = LastImageName ifFalse: [
		"Reset the author initials to blank when the image gets moved"
		LastImageName _ self imageName.
		Utilities clearAuthor].
	"Warning: Do open the source files only if nil.
	If not nil, it is because they are internalized and the files should not be opened"
	FileDirectory
		openSources: self defaultSourcesName
		andChanges: self defaultChangesName.
	CuisSourceFileArray install! !

!methodRemoval: FileDirectory class #openChanges:forImage:!
FileDirectory class removeSelector: #openChanges:forImage:!
!methodRemoval: FileDirectory class #openSources:andChanges:forImage:!
FileDirectory class removeSelector: #openSources:andChanges:forImage:!
!methodRemoval: FileDirectory class #openSources:forImage:!
FileDirectory class removeSelector: #openSources:forImage:!
