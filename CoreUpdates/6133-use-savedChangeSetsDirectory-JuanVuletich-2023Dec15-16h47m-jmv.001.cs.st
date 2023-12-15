'From Cuis6.0 [latest update: #6122] on 15 December 2023 at 4:48:19 pm'!

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 12/15/2023 14:57:42'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and either of the date & time or chosen to have a  
	unique numeric tag, depending on the preference  
	'changeSetVersionNumbers'"
	| folder slips nameToUse |
	folder := DirectoryEntry savedChangeSetsDirectory.
	nameToUse := self name.
	nameToUse := nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asUnaccented asCamelCase.
	nameToUse := (Preferences at: #changeSetVersionNumbers)
				ifTrue: [
					folder
						nextNameFor: nameToUse coda: '-', Utilities authorInitials
						extension: 'cs.st' ]
				ifFalse: [ (nameToUse , '.' , Utilities dateTimeSuffix , '.cs.st') asFileName ].

	folder // nameToUse writeStreamDo: [ :stream |
		stream timeStamp.
		self fileOutPreambleOn: stream.
		self fileOutOn: stream.
		self fileOutPostscriptOn: stream ].
	
	self hasUnsavedChanges: false.
	(Preferences at: #checkForSlips) 		ifFalse: [^ self].
	slips := self checkForSlips.
	(slips notEmpty
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [ Smalltalk browseMessageList: slips name: 'Possible slips in ' , name ]! !

