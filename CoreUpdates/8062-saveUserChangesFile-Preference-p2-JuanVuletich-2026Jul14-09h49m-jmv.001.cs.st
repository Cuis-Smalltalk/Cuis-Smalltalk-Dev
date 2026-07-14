'From Cuis7.9 [latest update: #8059] on 14 July 2026 at 10:14:27 am'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 14/Jul/2026, 1:14:09 pm (UTC)'!
assureStartupStampLogged
	"If there is a startup stamp not yet actually logged to disk, do it now."
	"Note: This renames the existing user changes file, and creates a new one."
	| changesFile directory oldUserChanges |
	(SourceFiles notNil and: [(changesFile := SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			changesFile nextChunkPut: StartupStamp; newLine.
			self forceChangesToDisk ]].
	
	(Preferences at: #saveUserChangesFile) ifTrue: [
		[
			(Preferences at: #autoNumberUserChanges) ifTrue: [
				oldUserChanges := DirectoryEntry userChangesDirectory // self userChangesFileName.
				oldUserChanges exists ifTrue: [
					directory := oldUserChanges parent.
					PreviousUserChangesName := directory nextNameFor: oldUserChanges nameWithoutExtension extension: 'changes'.
					oldUserChanges rename: PreviousUserChangesName ]]]
			on: Error do: [].
		self userChangesAppendDo: [ :stream |
			stream newLine; newLine.
			stream nextChunkPut: StartupStamp; newLine.
			stream flush ]].! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 14/Jul/2026, 1:13:15 pm (UTC)'!
userChangesAppendDo: aBlock
	"Convenience. On write errors, write a message to Transcript and continue."

	(Preferences at: #saveUserChangesFile) ifTrue: [
		[
			Utilities logsUserChanges ifTrue: [
				DirectoryEntry userChangesDirectory // self userChangesFileName appendStreamDo: aBlock ]]
			on: Error do: [ :ex |
				Transcript show: 'Can not log to User changes. '; show: ex  printString; newLine ]].! !


