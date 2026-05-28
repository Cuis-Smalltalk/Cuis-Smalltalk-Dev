'From Cuis7.7 [latest update: #7777] on 28 May 2026 at 3:37:37 pm'!

!CodePackage methodsFor: 'saving' stamp: 'jmv 28/May/2026 15:35:39'!
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
			backUpName := directory nextNameFor: oldFile nameWithoutExtension coda: '.pck' extension: 'st'.
			oldFile rename: backUpName ]].

	fullFileName asFullFileEntry forceWriteStreamDo: [ :stream |
		stream writeSystemTimeStamp.
		self writeOnStream: stream ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self).! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 28/May/2026 15:33:28'!
prefSystem
	^ #(
		#(allowNonLocalReturnsInExceptionHandlers true )
		#(warnAboutNonLocalReturnsInExceptionHandlers false )
		#(askConfirmationOnQuit true )
		#(askToInstallComplexPackage true )
		#(automaticPlatformSettings true )
		#(autoNumberUserChanges true )
		#(autoNumberPackages false )
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

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences name: #autoNumberPackages category: #system value: false.
Preferences saveToDisk: #autoNumberPackages.!

