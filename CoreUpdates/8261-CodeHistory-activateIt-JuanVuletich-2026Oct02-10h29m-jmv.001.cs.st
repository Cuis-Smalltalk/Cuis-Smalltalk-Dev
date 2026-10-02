'From Cuis7.9 [latest update: #8259] on 2 October 2026 at 10:37:12 am'!

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 2/Oct/2026, 1:31:25 pm (UTC)'!
fileIndex
	"Only valid for methods whose source is stored in .sources or .changes file."

	| hp libraryIndex |
	hp := self historyPointer.

	"No #historyPointer => no source file index at all."
	hp = 0 ifTrue: [ ^0 ].

	"#historyPointer doesn't point to CodeHistory, it points directly to .sources or .changes file."
	hp >= 16rFC000000 ifTrue: [
		^  CodeHistory fileIndexFromHistoryPointer: hp ].

	"CodeHistory holds the actual file index and file position"
	libraryIndex := CodeHistory current libraryIndexAt: hp.
	libraryIndex = 16rFFFFFFFF ifTrue: [ ^1 ]. "Sources"
	libraryIndex = 16rFFFFFFFE ifTrue: [ ^2 ]. "Changes"
	
	"Source code stored in a code library, not in a file."
	^0! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 2/Oct/2026, 1:31:36 pm (UTC)'!
filePosition
	"Only valid for methods whose source is stored in .sources or .changes files."

	| hp |
	hp := self historyPointer.

	"No #historyPointer => no source position at all."
	hp = 0 ifTrue: [ ^0 ].

	"#historyPointer doesn't point to CodeHistory, it points directly to .sources or .changes file."
	hp >= 16rFC000000 ifTrue: [
		^CodeHistory filePositionFromHistoryPointer: self historyPointer ].

	"CodeHistory holds the actual source code position position (either in .sources, .changes or in a code library."
	^CodeHistory current positionIndexAt: hp! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 2/Oct/2026, 1:33:10 pm (UTC)'!
setSourcePosition: position inFile: fileIndex
	"See other senders of #historyPointer:
	This method will be called when saving code to .changes file.
	Later #historyPointer: will be called again when entry added to CodeHistory."

	self historyPointer: (CodeHistory historyPointerFromFileIndex: fileIndex andPosition: position).! !

CodeHistory initialize!
