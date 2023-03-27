'From Cuis 6.0 [latest update: #5729] on 27 March 2023 at 4:11:01 pm'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/27/2023 16:09:58'!
assureStartupStampLogged
	"If there is a startup stamp not yet actually logged to disk, do it now."
	| changesFile directory oldUserChanges oldUserChangesName |
	StartupStamp ifNil: [^ self].
	(SourceFiles notNil and: [(changesFile := SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			changesFile nextChunkPut: StartupStamp; newLine.
			self forceChangesToDisk ]].
	(Preferences at: #autoNumberUserChanges) ifTrue: [
		oldUserChanges := Smalltalk defaultUserChangesName asFileEntry.
		oldUserChanges exists ifTrue: [
			directory := oldUserChanges parent.
			oldUserChangesName := directory nextNameFor: oldUserChanges nameWithoutExtension extension: 'changes'.
			oldUserChanges rename: oldUserChangesName ]].
	Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
		stream newLine; newLine.
		stream nextChunkPut: StartupStamp; newLine.
		stream flush ].
	StartupStamp := nil! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/27/2023 16:10:09'!
logChange: aStringOrText preamble: preambleOrNil
	"Write the argument, aString, onto the changes file."
	| aString changesFile |
	self assureStartupStampLogged.
	aString := aStringOrText asPlainString.
	aString firstNonSeparator = 0 ifTrue: [^ self].  "null doits confuse replay"
	
	(SourceFiles notNil and: [(changesFile := SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			preambleOrNil ifNotNil: [
				changesFile nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			changesFile nextChunkPut: aString.
			self forceChangesToDisk ]].
	Utilities logsUserChanges ifTrue: [
		Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
			stream newLine; newLine.
			preambleOrNil ifNotNil: [
				stream nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			stream nextChunkPut: aString.
			stream flush ]]! !

