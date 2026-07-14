'From Cuis7.9 [latest update: #8059] on 14 July 2026 at 10:14:27 am'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 14/Jul/2026, 1:06:11 pm (UTC)'!
userChangesFileName
	"
	Smalltalk userChangesFileName
	"
	^(FileIOAccessor default baseNameFor: self imageName pathAndLocalName second),
		(Utilities authorInitialsPerSe ifNotNil: [ :ai | '-', ai] ifNil: ['']),
		'.user.changes'! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 14/Jul/2026, 1:11:34 pm (UTC)'!
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
		#(saveUserChangesFile true )
		#(warnIfNoChangesFile true )
		#(warnIfNoSourcesFile true )
		#(saveReleaseOptionsInWorldMenu false)
	)! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences name: #saveUserChangesFile category: #system value: true.
Preferences saveToDisk: #saveUserChangesFile.
Preferences removeKey: #userChangesFileNameExtension.
!

