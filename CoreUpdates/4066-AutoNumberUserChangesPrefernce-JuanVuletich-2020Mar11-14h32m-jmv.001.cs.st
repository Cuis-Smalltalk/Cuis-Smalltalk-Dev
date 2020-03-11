'From Cuis 5.0 [latest update: #4064] on 11 March 2020 at 2:48:24 pm'!

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 3/11/2020 14:47:53'!
autoNumberUserChanges
	^ self
		valueOfFlag: #autoNumberUserChanges
		ifAbsent: [ true ].! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/11/2020 14:47:57'!
assureStartupStampLogged
	"If there is a startup stamp not yet actually logged to disk, do it now."
	| changesFile directory oldUserChanges oldUserChangesName |
	StartupStamp ifNil: [^ self].
	(SourceFiles notNil and: [(changesFile _ SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			changesFile nextChunkPut: StartupStamp asString; newLine.
			self forceChangesToDisk ]].
	Preferences autoNumberUserChanges ifTrue: [
		oldUserChanges _ Smalltalk defaultUserChangesName asFileEntry.
		oldUserChanges exists ifTrue: [
			directory _ oldUserChanges parent.
			oldUserChangesName _ directory nextNameFor: oldUserChanges nameWithoutExtension extension: 'changes'.
			oldUserChanges rename: oldUserChangesName ]].
	Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
		stream newLine; newLine.
		stream nextChunkPut: StartupStamp asString; newLine ].
	StartupStamp _ nil! !

