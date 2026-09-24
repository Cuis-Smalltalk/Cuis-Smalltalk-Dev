'From Cuis7.9 [latest update: #8210] on 22 September 2026 at 3:26:08 pm'!

!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 22/Sep/2026, 4:17:48 pm (UTC)'!
historyPointer
	"The last four bytes of a CompiledMethod encode access to source code and method history.
	0 means not source code and no method history.
	If value is in [16rFC000000 .. 16rFDFFFFFF] source code is in .sources file.
		Substract 16rFC000000 and multiply * 32 for position in file
	If value is in [16rFE000000 .. 16rFFFFFFFF] source code is in .changes file.
		Substract 16rFE000000 and multiply * 32 for position in file
	If value is below that, it is an index of History Array.
	"
	^self uint32At: self size - 3 bigEndian: false.! !

!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 22/Sep/2026, 4:49:01 pm (UTC)'!
historyPointer: aNumber
	"The last four bytes of a CompiledMethod encode access to source code and method history.
	0 means not source code and no method history.
	If value is in [16rFC000000 .. 16rFDFFFFFF] source code is in .sources file.
		Substract 16rFC000000 and multiply * 32 for position in file
	If value is in [16rFE000000 .. 16rFFFFFFFF] source code is in .changes file.
		Substract 16rFE000000 and multiply * 32 for position in file
	If value is below that, it is an index of History Array.
	"
	self uint32At: self size - 3 put: aNumber bigEndian: false.! !


!CuisSourceFileArray methodsFor: 'sourcePointer conversion' stamp: 'jmv 22/Sep/2026, 4:31:13 pm (UTC)'!
fileIndexFromHistoryPointer: anInteger
	"Return the index of the source file which contains the source chunk addressed by anInteger"
	"No source pointer => no fileIndex"
	
	anInteger = 0 ifTrue: [ ^0 ].
	^(anInteger - 16rFC000000 anyMask: 16r2000000)
		ifTrue: [2]
		ifFalse: [1]! !

!CuisSourceFileArray methodsFor: 'sourcePointer conversion' stamp: 'jmv 22/Sep/2026, 4:19:49 pm (UTC)'!
filePositionFromHistoryPointer: anInteger
	"Return the position of the source chunk addressed by anInteger"

	| unscaledPosition |
	"No source pointer => no filePosition"
	anInteger = 0 ifTrue: [ ^0 ].
	unscaledPosition := (anInteger - 16rFC000000) bitAnd: 16r1FFFFFF.
	^unscaledPosition * pointerScale! !

!CuisSourceFileArray methodsFor: 'sourcePointer conversion' stamp: 'jmv 22/Sep/2026, 4:40:54 pm (UTC)'!
historyPointerFromFileIndex: index andPosition: position
	"Return a sourcePointer encoding the given file index and position"
	| answer changesFlag |
	((index between: 1 and: 2) and: [position >= 0])
		ifFalse: [self error: 'invalid source code pointer'].
	changesFlag := index = 2 ifTrue: [ 16r2000000 ] ifFalse: [ 0 ].
	answer := (position // pointerScale bitOr: changesFlag) + 16rFC000000.
	^answer! !


!RemoteString methodsFor: 'accessing' stamp: 'jmv 22/Sep/2026, 4:42:08 pm (UTC)'!
historyPointer
	sourceFileNumber ifNil: [^ 0].
	^SourceFiles historyPointerFromFileIndex: sourceFileNumber andPosition: filePositionHi! !


!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 22/Sep/2026, 4:10:32 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel05ClassAndMethodCreation}!
endPC
	"Answer the index of the last bytecode."
	"Normal 4-byte source pointer"
	^self size - 4! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 22/Sep/2026, 4:30:20 pm (UTC)'!
fileIndex
	^ SourceFiles fileIndexFromHistoryPointer: self historyPointer! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 22/Sep/2026, 4:30:32 pm (UTC)'!
filePosition
	^SourceFiles filePositionFromHistoryPointer: self historyPointer! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 22/Sep/2026, 5:06:36 pm (UTC)'!
getSourceFor: selector in: class
	"Retrieve or reconstruct the source code for this method."

	^self getSourceFromFile
		ifNil: [ (class decompilerClass new decompile: selector in: class method: self) decompileString ]! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 22/Sep/2026, 4:55:54 pm (UTC)'!
setSourcePosition: position inFile: fileIndex 
	self historyPointer: (SourceFiles historyPointerFromFileIndex: fileIndex andPosition: position)! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 22/Sep/2026, 5:02:27 pm (UTC)'!
condenseChanges
	"Move all the changes onto a compacted sources file."
	"
	Smalltalk condenseChanges
	"

	| oldChanges classCount oldChangesLocalName oldChangesPathName |
	DirectoryEntry smalltalkImageDirectory // 'ST80.temp' forceWriteStreamDo: [ :f |
		f writeSystemTimeStamp.
		'Condensing Changes File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: self classNames size
			during: [ :barBlock |
				classCount := 0.
				self allClassesDo: [ :class | 
					barBlock value: (classCount := classCount + 1).
					class moveChangesTo: f.
					class putClassCommentToCondensedChangesFile: f.
					class class moveChangesTo: f ]].
		LastQuitLogPosition := f position ].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e historyPointer: 0 ] ].

	oldChanges := SourceFiles at: 2.
	oldChangesPathName := oldChanges name.
	oldChangesLocalName := oldChanges localName.
	oldChanges close.
	(oldChangesPathName, '.old') asFullFileEntry delete.
	oldChangesPathName asFullFileEntry rename: oldChangesLocalName, '.old'.
	DirectoryEntry smalltalkImageDirectory // 'ST80.temp' rename: oldChangesLocalName.
	
	SourceFiles
			at: 2 put: oldChangesPathName asFullFileEntry appendStream.

	self inform: 'Changes file has been rewritten!!

Check that all is well, and then save/quit.
 
Otherwise, remove new changes,
replace it with the former one, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 22/Sep/2026, 5:02:32 pm (UTC)'!
condenseSources: newVersionString
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| classCount oldChanges oldChangesLocalName oldChangesPathName newChangesPathName newSourcesName |
	SourceFileVersionString := newVersionString.

	"Write all sources with fileIndex 1"
	newSourcesName := self defaultSourcesName.
	newSourcesName asFullFileEntry writeStreamDo: [ :f |
		f writeSystemTimeStamp.
		'Condensing Sources File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: self classNames size
			during: [ :barBlock |
				classCount := 0.
				self allClassesDo: [ :class |
					barBlock value: (classCount := classCount + 1).
					class fileOutOn: f moveSource: true toFile: 1]]].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e historyPointer: 0 ] ].

	"Make a new empty changes file"
	oldChanges := SourceFiles at: 2.
	oldChangesPathName := oldChanges name.
	oldChangesLocalName := oldChanges localName.
	self closeSourceFiles.
	oldChangesPathName ifNotNil: [
		(oldChangesPathName, '.old') asFullFileEntry delete.
	oldChangesPathName asFullFileEntry rename: oldChangesLocalName, '.old' ].
	newChangesPathName := self defaultChangesName.
	newChangesPathName asFullFileEntry writeStreamDo: [ :stream |
		stream writeSystemTimeStamp ].
	LastQuitLogPosition := 0.

	self openSourceFiles.! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 22/Sep/2026, 5:02:11 pm (UTC)'!
abandonSources
	"
	Smalltalk abandonSources
	"
	 | m bTotal bCount |
	(self confirm:
'This method will detach the image fom source code.
A fresh changes file will be created to record further changes.
-- CAUTION --
If you have backed up your system and
are prepared to face the consequences of
abandoning source code files, choose Yes.
If you have any doubts, you may choose No
to back out with no harm done.')
		==  true ifFalse: [^ self inform: 'Okay - no harm done'].
	bTotal := 0.  bCount := 0.
	self allBehaviorsDo: [: b | bTotal := bTotal + 1].
'Doing #destroySourcePointer ...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: bTotal
	during: [ :barBlock |
		self allBehaviorsDo: [ :cl |
		"for testing"
		"{ EllipseMorph } do: [ :cl |"
			barBlock value: (bCount := bCount + 1).
			cl selectors do: [:selector |
				m := cl compiledMethodAt: selector.
				m historyPointer: 0 ]]].
	self allBehaviorsDo: [:b | b zapOrganization].
	self closeSourceFiles.
	Preferences at: #warnIfNoChangesFile put: false.
	Preferences at: #warnIfNoSourcesFile put: false! !

!methodRemoval: CompiledMethod #zapSourcePointer stamp: 'jmv 22/Sep/2026, 6:21:28 pm (UTC)'!
CompiledMethod removeSelector: #zapSourcePointer!
!methodRemoval: CompiledMethod #destroySourcePointer stamp: 'jmv 22/Sep/2026, 6:25:12 pm (UTC)'!
CompiledMethod removeSelector: #destroySourcePointer!
