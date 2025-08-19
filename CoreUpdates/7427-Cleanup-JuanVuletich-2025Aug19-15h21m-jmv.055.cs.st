'From Cuis7.5 [latest update: #7426] on 19 August 2025 at 3:32:37 pm'!

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 8/19/2025 15:31:49'!
logError: errMsg inContext: aContext to: baseFilename
	"Log the error message and a stack trace to the given file.
	Smalltalk logError: 'test error message' inContext: thisContext to: 'testErr'
	"

	| localFilename file |
	localFilename := (Preferences at: #debugLogTimestamp)
		ifTrue: [ baseFilename, '-',
				(String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ]),
				'.log' ]
		ifFalse: [ baseFilename, '.log' ].
	file := DirectoryEntry logsDirectory // localFilename.
	[
		file forceWriteStreamDo: [ :stream |
	 	 	stream nextPutAll: errMsg; newLine.
			aContext errorReportOn: stream ]
	] on: UnhandledError do: [ :ex | ex return]. "avoid recursive errors"
	[
 	 	StdIOWriteStream stdout newLine; nextPutAll: errMsg.
		StdIOWriteStream stdout newLine; nextPutAll: 'See '; nextPutAll: file pathName.
		StdIOWriteStream stdout newLine.
		aContext shortErrorReportOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout flush
	] on: UnhandledError do: [ :ex | ex return]. "avoid recursive errors"! !


!DateAndTime methodsFor: 'public protocol' stamp: 'jmv 8/19/2025 15:31:03'!
filenamishPrintOn: aStream
	"
	String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ] '2025Aug18-16h46m' .
	"
	
	| year month day monthName |
	self dayMonthYearDo: [ :d :m :y | year := y. month := m. day := d ].
	year negative
		ifTrue: [ aStream nextPut: $- ].
	monthName := Month nameOfMonth: month.
	aStream
		nextPutAll: (year abs printString padded: #left to: 4 with: $0);
		nextPutAll: (monthName copyFrom: 1 to: 3);
		nextPutAll: (day printString padded: #left to: 2 with: $0);
		nextPut: $-;
		nextPutAll: (self hour printString padded: #left to: 2 with: $0);
		nextPut: $h;
		nextPutAll: (self minute printString padded: #left to: 2 with: $0);
		nextPut: $m! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 8/19/2025 15:21:16'!
prefSystem
	^ #(
		#(allowNonLocalReturnsInExceptionHandlers true )
		#(warnAboutNonLocalReturnsInExceptionHandlers false )
		#(askConfirmationOnQuit true )
		#(askToInstallComplexPackage true )
		#(automaticPlatformSettings true )
		#(autoNumberUserChanges true )
		#(caseSensitiveFinds false )
		#(checkForSlips true )
		#(checkLostChangesOnStartUp false )
		#(warnAndOfferLastUserChanges true )
		#(cmdDotEnabled true )
		#(cmdDotInterruptTakesStatistics false )
		#(dismissAllOnOptionClose false )
		#(initialFileListDirectories #cuis #(cuis roots) )
		#(machine default #(default slow smalltalk80) machine: )
		#(pointer mouse #(touch mouse) pointer:)
		#(selectionsMayShrink true )
		#(serverMode false )
		#(systemWindowEmbedOK false )
		#(transcriptLogVerbose true )
		#(useUserPrefsFile true )
		#(userChangesFileNameExtension '.user.changes' )
		#(warnIfNoChangesFile true )
		#(warnIfNoSourcesFile true )
		#(saveReleaseOptionsInWorldMenu false)
	)! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 8/19/2025 15:27:12'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and a unique numeric tag."

	| folder slips nameToUse |
	folder := DirectoryEntry savedChangeSetsDirectory.
	nameToUse := self name.
	nameToUse := nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asUnaccented asCamelCase.
	nameToUse := folder nextNameFor: nameToUse coda: '-', Utilities authorInitials extension: 'cs.st'.

	folder // nameToUse writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			fileStream timeStamp.
			self fileOutPreambleOn: fileStream.
			self fileOutOn: fileStream.
			self fileOutPostscriptOn: fileStream ]].

	self hasUnsavedChanges: false.
	(Preferences at: #checkForSlips) 		ifFalse: [^ self].
	slips := self checkForSlips.
	(slips notEmpty
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [ Smalltalk browseMessageList: slips name: 'Possible slips in ' , name ].! !

!methodRemoval: Utilities class #dateTimeSuffix stamp: 'jmv 8/19/2025 15:29:18'!
Utilities class removeSelector: #dateTimeSuffix!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences removeKey: #changeSetVersionNumbers!

