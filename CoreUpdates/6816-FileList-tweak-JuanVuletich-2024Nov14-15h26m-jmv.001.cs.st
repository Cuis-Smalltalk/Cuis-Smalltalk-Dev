'From Cuis7.1 [latest update: #6814] on 14 November 2024 at 3:28:27 pm'!

!FileList methodsFor: 'private' stamp: 'jmv 11/14/2024 15:28:09'!
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
					n < list size ifTrue: [
						s nextPutAll: 'MANY FILES...'; newLine  ].
					1 to: n do: [ :i | s nextPutAll: (list at: i); newLine].
					n < list size ifTrue: [
						s nextPutAll: '  -- End of this Folder Summary --'; newLine.
						s nextPutAll: '  -- Additional files not listed here --'; newLine ]]].
	brevityState := #FileList.
	^ acceptedContentsCache! !

