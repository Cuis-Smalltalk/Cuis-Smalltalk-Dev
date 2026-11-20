'From Cuis7.9 [latest update: #8388] on 20 November 2026 at 12:01:33 pm'!

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 20/Nov/2026, 3:00:03 pm (UTC)'!
inSourcesFile
	"Answer true if our source code is stored in the .sources file."

	| hp |
	hp := self historyPointer.

	"#historyPointer doesn't point to CodeHistory, it points directly to .sources or .changes file."
	hp >= 16rFC000000 ifTrue: [
		^  (CodeHistory fileIndexFromHistoryPointer: hp) = 1 ].

	"CodeHistory holds the actual file index and file position"
	^ (CodeHistory current libraryIndexAt: hp) = 16rFFFFFFFF! !


!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 20/Nov/2026, 3:01:03 pm (UTC)'!
browseMyChanges
	"Browse only the changes (in the changes file) by the current author.
	Smalltalk browseMyChanges
	"
	self browseAllSelect: [ :method |
       method inSourcesFile not 	"not if in .sources file"
               and: [ method authorAndStampAsSaved beginsWith: Utilities authorInitials, ' ' ]]! !

