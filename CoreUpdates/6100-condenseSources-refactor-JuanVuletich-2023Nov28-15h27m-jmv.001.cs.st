'From Cuis 6.0 [latest update: #6099] on 28 November 2023 at 3:31:50 pm'!

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 11/28/2023 15:30:09'!
condenseSources: newVersionString
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| classCount oldChanges oldChangesLocalName oldChangesPathName newChangesPathName newSourcesName |
	SourceFileVersionString := newVersionString.

	"Write all sources with fileIndex 1"
	newSourcesName := self defaultSourcesName.
	newSourcesName asFileEntry writeStreamDo: [ :f |
		f timeStamp.
		'Condensing Sources File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: Smalltalk classNames size
			during: [ :barBlock |
				classCount := 0.
				Smalltalk allClassesDo: [ :class |
					barBlock value: (classCount := classCount + 1).
					class fileOutOn: f moveSource: true toFile: 1]]].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	"Make a new empty changes file"
	oldChanges := SourceFiles at: 2.
	oldChangesPathName := oldChanges name.
	oldChangesLocalName := oldChanges localName.
	self closeSourceFiles.
	oldChangesPathName ifNotNil: [
		(oldChangesPathName, '.old') asFileEntry delete.
	oldChangesPathName asFileEntry rename: oldChangesLocalName, '.old' ].
	newChangesPathName := self defaultChangesName.
	newChangesPathName asFileEntry writeStreamDo: [ :stream |
		stream timeStamp ].
	LastQuitLogPosition := 0.

	self openSourceFiles.! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 11/28/2023 15:29:56'!
condenseSources	
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	
	self request: 'Please name the new sources file' initialAnswer: SourceFileVersionString verifying: [:newVersionString| newVersionString ~= SourceFileVersionString] do: [:newVersionString|
		self condenseSources: newVersionString.
		self inform: 'Source files have been rewritten!!
 
Check that all is well, and then save/quit.
 
Otherwise, remove new sources/changes,
replace them with the former ones, and
exit without saving the image.
 ']! !

