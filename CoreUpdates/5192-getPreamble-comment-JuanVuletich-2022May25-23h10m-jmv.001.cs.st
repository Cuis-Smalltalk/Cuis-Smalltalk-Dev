'From Cuis 6.0 [latest update: #5191] on 25 May 2022 at 11:10:11 pm'!

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 5/25/2022 23:10:03'!
getPreamble
	| file preamble |
	self fileIndex = 0 ifTrue: [^ String new].  "no source pointer for this method"
	file _ SourceFiles at: self fileIndex.
	file ifNil: [^ ''].  "sources file not available"
	"file does not exist happens in secure mode"
	[
		file name asFileEntry readStreamDo: [ :stream | | p |
			preamble _ ''.
			p _ 0 max: self filePosition.
			p < stream size ifTrue: [
				"It appears that on Linux systems, immediately after adding a new method, the OS file cache may not be updated, so contents may still not be accessible. Protect against that."
				stream position: p.
				"Skip back blank space."
				stream backChunk.
				"Find and answer preamble chunk."
				preamble _ stream backChunk ]]
	] on: FileDoesNotExistException do: [ :ex | preamble _ '' ].
	^ preamble! !

