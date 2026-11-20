'From Cuis7.9 [latest update: #8384] on 17 November 2026 at 6:30:12 pm'!

!TextProvider methodsFor: 'contents' stamp: 'jmv 17/Nov/2026, 9:30:03 pm (UTC)'!
acceptedContents
	^ self acceptedStringOrText
		ifNil: [ '' ]
		ifNotNil: [ :str |
			Text initialFont: (Preferences at: #standardCodeFont) stringOrText: str ].! !


!MethodVersionsBrowser methodsFor: 'viewing access' stamp: 'jmv 17/Nov/2026, 9:29:52 pm (UTC)'!
diffedVersionContents
	"Answer diffed version contents, maybe pretty maybe not"

	| change class earlier later |
	(listIndex = 0
			or: [changeList size < listIndex])
		ifTrue: [^ ''].

	change := changeList at: listIndex.
	later := change string.
	later ifNil: [ ^ '' ].
	
	class := change changeClass.
	(listIndex = changeList size or: [class == nil])
		ifTrue: [^ later].
	earlier := (changeList at: listIndex + 1) string.
	earlier ifNil: [ ^ later ].
	
	^ DifferenceFinder
		displayPatchFrom: earlier
		to: later
		tryWords: self shouldDiffWords
		prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [class])! !

