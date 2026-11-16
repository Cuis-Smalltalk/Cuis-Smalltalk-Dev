'From Cuis7.9 [latest update: #8351] on 13 November 2026 at 3:06:56 pm'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 13/Nov/2026, 6:06:26 pm (UTC)'!
logChange: aStringOrText preamble: preambleOrNil
	"Write the argument, aString, onto the changes file."
	| aString changesFile |
	aString := aStringOrText asPlainString.
	aString firstNonSeparator = 0 ifTrue: [^ self].  "null doits confuse replay"
	
	(SourceFiles notNil and: [(changesFile := SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			preambleOrNil ifNotNil: [
				changesFile nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			changesFile nextChunkPut: aString.
			self forceChangesToDisk ]].

	self userChangesAppendDo: [ :stream |
		stream newLine; newLine.
		preambleOrNil ifNotNil: [
			stream nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
		stream nextChunkPut: aString.
		stream flush ].! !

!methodRemoval: SystemDictionary #logToUserChanges:preamble: stamp: 'jmv 13/Nov/2026, 6:06:46 pm (UTC)'!
SystemDictionary removeSelector: #logToUserChanges:preamble:!
