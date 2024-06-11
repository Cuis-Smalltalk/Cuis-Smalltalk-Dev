'From Cuis7.1 [latest update: #6460] on 11 June 2024 at 4:01:10 pm'!

!TextEditor methodsFor: 'attributes' stamp: 'jmv 6/11/2024 15:59:49'!
offerColorMenu
	"Present a menu of available colors, and if one is chosen, apply it to the current selection."
	"This is a user command, and generates undo"

	| attribute colors index thisSel |
	thisSel := self selection.
	colors := #(#black #magenta #red #yellow #green #blue #cyan #white ).
	index := (PopUpMenu
		labelArray: colors ", #('choose color...' )
		lines: (Array with: colors size)") startUpMenu.
	index = 0 ifTrue: [ ^ true ].
	index <= colors size
		ifTrue: [ attribute := TextColor color: (Color perform: (colors at: index)) ]
		ifFalse: [
			index := index - colors size - 1.
			"Re-number!!!!!!"
			index = 0 ifTrue: [ attribute := self chooseColor ].
			thisSel ifNil: [ ^ true ]].
	attribute ifNotNil: [ self applyAttribute: attribute ].
	morph updateFromTextComposition.
	^ true.! !

