'From Cuis7.7 [latest update: #7800] on 16 January 2026 at 10:50:30 am'!

!AutoCompleter methodsFor: 'menu morph' stamp: 'jmv 16/Jan/2026 10:50:24'!
openCompletionMenu

	| theEditor |
	theEditor := textMorph editor.
	position := theEditor startIndex - 1.
	self closeMenu.
	self computeEntries.
	entries notEmpty
		ifTrue: [ | startIndex characterBlock cursorIndex |
			cursorIndex := theEditor pointIndex.
			startIndex := (theEditor text at: cursorIndex-1) = Character space
				ifTrue: [ cursorIndex ]
				ifFalse: [ theEditor previousWordStart: cursorIndex ].
			characterBlock := theEditor characterBlockForIndex: startIndex.
			menuMorph :=  self morphClass
				completer: self
				position: characterBlock bottomLeft + textMorph positionInWorld ].! !

