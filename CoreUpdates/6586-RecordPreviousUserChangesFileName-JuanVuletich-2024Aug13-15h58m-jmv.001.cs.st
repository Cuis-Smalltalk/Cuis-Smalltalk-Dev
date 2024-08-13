'From Cuis7.1 [latest update: #6584] on 13 August 2024 at 3:59:57 pm'!
!classDefinition: #SystemDictionary category: #'System-Support'!
IdentityDictionary subclass: #SystemDictionary
	instanceVariableNames: 'cachedClassNames cachedNonClassNames startUpScriptArguments startUpArguments '
	classVariableNames: 'ChangesInitialFileSize EndianCache LastImageName LastQuitLogPosition LowSpaceProcess LowSpaceSemaphore ShutDownList SourceFileVersionString SpecialSelectors StartUpList StartupStamp PreviousUserChangesName '
	poolDictionaries: ''
	category: 'System-Support'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/13/2024 13:06:11'!
processShutDownList: quitting
	"Send #shutDown to each class that needs to wrap up before a snapshot."

	self send: #shutDown: toClassesNamedIn: ShutDownList with: quitting.
	EndianCache := nil.
	PreviousUserChangesName := nil.! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 8/13/2024 13:03:20'!
assureStartupStampLogged
	"If there is a startup stamp not yet actually logged to disk, do it now."
	| changesFile directory oldUserChanges |
	(SourceFiles notNil and: [(changesFile := SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			changesFile nextChunkPut: StartupStamp; newLine.
			self forceChangesToDisk ]].
	(Preferences at: #autoNumberUserChanges) ifTrue: [
		oldUserChanges := DirectoryEntry userChangesDirectory // self userChangesFileName.
		oldUserChanges exists ifTrue: [
			directory := oldUserChanges parent.
			PreviousUserChangesName := directory nextNameFor: oldUserChanges nameWithoutExtension extension: 'changes'.
			oldUserChanges rename: PreviousUserChangesName ]].
	DirectoryEntry userChangesDirectory // self userChangesFileName appendStreamDo: [ :stream |
		stream newLine; newLine.
		stream nextChunkPut: StartupStamp; newLine.
		stream flush ].! !

!classDefinition: #SystemDictionary category: #'System-Support'!
IdentityDictionary subclass: #SystemDictionary
	instanceVariableNames: 'cachedClassNames cachedNonClassNames startUpScriptArguments startUpArguments'
	classVariableNames: 'ChangesInitialFileSize EndianCache LastImageName LastQuitLogPosition LowSpaceProcess LowSpaceSemaphore PreviousUserChangesName ShutDownList SourceFileVersionString SpecialSelectors StartUpList StartupStamp'
	poolDictionaries: ''
	category: 'System-Support'!
