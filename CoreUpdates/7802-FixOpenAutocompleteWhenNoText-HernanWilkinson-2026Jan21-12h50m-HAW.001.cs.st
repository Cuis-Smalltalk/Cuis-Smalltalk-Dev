'From Cuis7.7 [latest update: #7801] on 21 January 2026 at 12:51:36 pm'!

!AutoCompleter methodsFor: 'menu morph' stamp: 'HAW 21/Jan/2026 12:50:24'!
openCompletionMenu

	| theEditor |
	theEditor := textMorph editor.
	position := theEditor startIndex - 1.
	self closeMenu.
	self computeEntries.
	entries notEmpty
		ifTrue: [ | startIndex characterBlock cursorIndex |
			cursorIndex := theEditor pointIndex.
			startIndex := (theEditor text at: cursorIndex-1 ifAbsent: [ Character space ]) = Character space
				ifTrue: [ cursorIndex ]
				ifFalse: [ theEditor previousWordStart: cursorIndex ].
			characterBlock := theEditor characterBlockForIndex: startIndex.
			menuMorph :=  self morphClass
				completer: self
				position: characterBlock bottomLeft + textMorph positionInWorld ].! !

