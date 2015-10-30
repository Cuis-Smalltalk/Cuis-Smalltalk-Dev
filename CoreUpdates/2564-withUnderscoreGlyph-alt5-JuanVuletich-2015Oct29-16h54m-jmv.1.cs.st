'From Cuis 4.2 of 25 July 2013 [latest update: #2563] on 29 October 2015 at 4:55:11.459174 pm'!

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 10/29/2015 12:08'!
changeEmphasis: aKeyboardEvent
	"Change the emphasis of the current selection or prepare to accept characters with the change in emphasis."
	"control 0..9 -> 0..9"
	"This is a user command, and generates undo"

	| keyCode attributeToAdd attributeToRemove oldAttributes |
	keyCode _ ('012345'
		indexOf: aKeyboardEvent keyCharacter
		ifAbsent: [ 1 ]) - 1.
	oldAttributes _ emphasisHere.

	keyCode = 0 ifTrue: [ attributeToAdd _ TextEmphasis normal ].

	(keyCode between: 1 and: 5) ifTrue: [
		attributeToAdd _ TextEmphasis perform: (#(#bold #italic #underlined #struckThrough #withUnderscoreGlyphs ) at: keyCode).
		oldAttributes do: [ :oldAtt |
			oldAtt = attributeToAdd ifTrue: [
				attributeToAdd _ nil.
				attributeToRemove _ oldAtt ]]].

	attributeToAdd ifNotNil: [ self applyAttribute: attributeToAdd ].
	attributeToRemove ifNotNil: [ self unapplyAttribute: attributeToRemove ].
	^ true.! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 10/29/2015 12:08'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(		$0 	changeEmphasis:			'Normal')
		#(		$1 	changeEmphasis:			'Bold')
		#(		$2 	changeEmphasis:			'Italic')
		#(		$3 	changeEmphasis:			'Underline')
		#(		$4 	changeEmphasis:			'Strikeout')
		#(		$5 	changeEmphasis:			'With Underscore Glyph')
		#(		$8	#offerColorMenu:			'Change color')

		#(		$k	#offerFontMenu:			'Set font')

		#(		$u	#align:						'Toggle alignment')
	)! !

