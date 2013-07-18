'From Cuis 4.1 of 12 December 2012 [latest update: #1672] on 12 April 2013 at 3:58:12 pm'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 4/12/2013 11:31'!
alternativeSourcesName
	"Answer the alternative full path to the sources file.
	If Sources not found at #defaultSourcesName (image folder), seek for them here (vm folder)."
	"
	Smalltalk alternativeSourcesName
	"
	"Answer the default full path to the version-stable source code"
	^ self vmPath , SourceFileVersionString , FileDirectory dot , FileDirectory sourceSuffix! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 4/12/2013 15:57'!
openSourcesAndChanges
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems regarding write permissions or Lf/CrLf mixups."
	"Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sourcesPathName changesPathName sources changes msg wmsg |
	msg _ 'Squeak cannot locate 
Please check that the file is named properly and is in the
same directory as this image.'.
	wmsg _ 'Squeak cannot write to XfileRef.

Please check that you have write permission for this file.

You won''t be able to save this image correctly until you fix this.'.

	"Do not open source files if internalized (i.e. notNil)"
	sources _ SourceFiles at: 1.
	sources ifNil: [
		sourcesPathName _ Smalltalk defaultSourcesName.
		sources _ StandardFileStream oldFileOrNoneNamed: sourcesPathName.
		sources ifNil: [
			sourcesPathName _ Smalltalk alternativeSourcesName.
			sources _ StandardFileStream oldFileOrNoneNamed: sourcesPathName ]].
	(sources isNil and: [ Preferences valueOfFlag: #warnIfNoSourcesFile ])
		ifTrue: [
			Smalltalk platformName = 'Mac OS' ifTrue: [
				msg _ msg , String newLineString, 'Make sure the sources file is not an Alias.'].
			self inform: (msg copyReplaceAll: 'XfileRef' with: 'the sources file named ' , sourcesPathName) ].

	changesPathName _ Smalltalk defaultChangesName.
	"Do not open source files if internalized (i.e. notNil)"
	changes _ (SourceFiles at: 2) ifNil: [ StandardFileStream oldFileNamed: changesPathName ].
	(changes isNil and: [ Preferences valueOfFlag: #warnIfNoChangesFile ])
		ifTrue: [self inform: (msg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , changesPathName)].

	((Preferences valueOfFlag: #warnIfNoChangesFile) and: [changes notNil]) ifTrue: [
		changes isReadOnly ifTrue: [
			self inform: (wmsg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , changesPathName)].

		((changes next: 200) includesSubString: String crlfString) ifTrue: [
			self inform: 'The changes file named ' , changesPathName , '
has been injured by an unpacking utility.  Lfs were changed to CrLfs.
Please set the preferences in your decompressing program to 
"do not convert text files" and unpack the system again.']].

	SourceFiles _ Array with: sources with: changes! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 4/12/2013 15:52'!
openSourceFiles
	self imageName = LastImageName ifFalse: [
		"Reset the author initials to blank when the image gets moved"
		LastImageName _ self imageName.
		Utilities clearAuthor].
	"Warning: Do open the source files only if nil.
	If not nil, it is because they are internalized and the files should not be opened"
	self openSourcesAndChanges.
	CuisSourceFileArray install! !

!methodRemoval: FileDirectory class #openSources:andChanges:!
FileDirectory class removeSelector: #openSources:andChanges:!
