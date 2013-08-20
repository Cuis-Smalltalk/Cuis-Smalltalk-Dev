'From Cuis 4.2 of 25 July 2013 [latest update: #1804] on 20 August 2013 at 11:09:11.385428 am'!

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 8/20/2013 11:04'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and either of the date & time or chosen to have a  
	unique numeric tag, depending on the preference  
	'changeSetVersionNumbers'"
	| slips nameToUse file |
	self checkForConversionMethods.
	nameToUse _ self name.
	nameToUse _ nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asCamelCase. 
	nameToUse _ Preferences changeSetVersionNumbers
				ifTrue: [ self defaultChangeSetDirectory nextNameFor:nameToUse coda: '-', Utilities authorInitials extension: 'cs.st' ]
				ifFalse: [ (nameToUse , FileDirectory dot , Utilities dateTimeSuffix , FileDirectory dot , 'cs.st') asFileName ].
	file _ self defaultChangeSetDirectory newFileNamed: nameToUse.
	[
		file timeStamp.
		self fileOutPreambleOn: file.
		self fileOutOn: file.
		self fileOutPostscriptOn: file]
			ensure: [ file close ].
	
	self hasUnsavedChanges: false.
	Preferences checkForSlips
		ifFalse: [^ self].
	slips := self checkForSlips.
	(slips size > 0
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [Smalltalk browseMessageList: slips name: 'Possible slips in ' , name]! !

