'From Cuis 4.2 of 25 July 2013 [latest update: #2972] on 7 November 2016 at 12:00:10 pm'!

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 11/7/2016 11:56:10'!
condenseSources	
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| classCount newVersionString oldChanges oldChangesLocalName oldChangesPathName newChangesPathName newSourcesName |
	newVersionString _ FillInTheBlankMorph request: 'Please name the new sources file' initialAnswer: SourceFileVersionString.
	newVersionString ifNil: [^ self].
	newVersionString = SourceFileVersionString ifTrue: [
		^ self error: 'The new source file must not be the same as the old.'].
	SourceFileVersionString _ newVersionString.

	"Write all sources with fileIndex 1"
	newSourcesName _ self defaultSourcesName.
	newSourcesName asFileEntry writeStreamDo: [ :f |
		f timeStamp.
		'Condensing Sources File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: Smalltalk classNames size
			during: [ :bar |
				classCount _ 0.
				Smalltalk allClassesDo: [ :class |
					bar value: (classCount _ classCount + 1).
					class fileOutOn: f moveSource: true toFile: 1]]].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	"Make a new empty changes file"
	oldChanges _ SourceFiles at: 2.
	oldChangesPathName _ oldChanges name.
	oldChangesLocalName _ oldChanges localName.
	self closeSourceFiles.
	oldChangesPathName ifNotNil: [
		(oldChangesPathName, '.old') asFileEntry delete.
		oldChangesPathName asFileEntry rename: oldChangesLocalName, '.old' ].
	newChangesPathName _ self defaultChangesName.
	newChangesPathName asFileEntry writeStreamDo: [ :stream |
		stream timeStamp ].
	LastQuitLogPosition _ 0.

	self openSourceFiles.
	self inform: 'Source files have been rewritten!!
 
Check that all is well, and then save/quit.
 
Otherwise, remove new sources/changes,
replace them with the former ones, and
exit without saving the image.
 '! !

