'From Cuis7.1 [latest update: #6758] on 4 October 2024 at 10:52:37 am'!

!FileList methodsFor: 'private' stamp: 'jmv 10/4/2024 10:43:02'!
defaultContents
	| n |
	acceptedContentsCache := list
			ifNil: [String new]
			ifNotNil: [
				"In the contents pane, list only up to a certain number of files."
				n := list size min: 100.
				String streamContents: [ :s | 
					s nextPutAll: 'NO FILE SELECTED'; newLine.
					s nextPutAll: '  -- Folder Summary --'; newLine.
					s nextPutAll: 'MANY FILES'; newLine.
					1 to: n do: [ :i | s nextPutAll: (list at: i); newLine].
					n < list size ifTrue: [
						s nextPutAll: '  -- End of this Folder Summary --'; newLine.
						s nextPutAll: '  -- Additional files not listed here --'; newLine ]]].
	brevityState := #FileList.
	^ acceptedContentsCache! !

!FileList methodsFor: 'stepping' stamp: 'jmv 10/4/2024 10:51:00'!
stepAt: millisecondSinceLast

	"Do not auto-update the list if there are too many files. It could take too long."
	list size <= 1000 ifTrue: [
		self updateFileList ].! !

