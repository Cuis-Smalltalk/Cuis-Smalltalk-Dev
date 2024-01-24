'From Cuis6.3 [latest update: #6169] on 24 January 2024 at 8:01:30 pm'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'HAW 1/24/2024 19:58:35'!
logToUserChanges: aString preamble: preambleOrNil

	"This message is used by TDD-Guru, do not remove it without looking at it - Hernan"
	Utilities logsUserChanges ifTrue: [
		DirectoryEntry userChangesDirectory // self userChangesFileName appendStreamDo: [ :stream |
			stream newLine; newLine.
			preambleOrNil ifNotNil: [
				stream nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			stream nextChunkPut: aString.
			stream flush ]]! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'HAW 1/24/2024 19:57:41'!
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
	self logToUserChanges: aString preamble: preambleOrNil! !

