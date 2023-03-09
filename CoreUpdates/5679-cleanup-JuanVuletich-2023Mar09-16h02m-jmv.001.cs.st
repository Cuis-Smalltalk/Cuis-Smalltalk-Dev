'From Cuis 6.0 [latest update: #5678] on 9 March 2023 at 4:04:45 pm'!

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 3/9/2023 16:03:33'!
changeEmphasis: aKeyboardEvent
	"Change the emphasis of the current selection or prepare to accept characters with the change in emphasis."
	"control 0..9 -> 0..9"
	"This is a user command, and generates undo"

	| keyCode attributeToAdd attributeToRemove oldAttributes |
	keyCode _ ('0123456'
		indexOf: aKeyboardEvent keyCharacter
		ifAbsent: [ 1 ]) - 1.
	oldAttributes _ emphasisHere.

	keyCode = 0 ifTrue: [ attributeToAdd _ TextEmphasis normal ].
	(keyCode between: 1 and: 6) ifTrue: [
		attributeToAdd _ TextEmphasis perform: (
			#(#bold #italic #underlined #struckThrough #superscript #subscript)
				at: keyCode).
		oldAttributes do: [ :oldAtt |
			oldAtt = attributeToAdd ifTrue: [
				attributeToAdd _ nil.
				attributeToRemove _ oldAtt ]]].

	attributeToAdd ifNotNil: [ self applyAttribute: attributeToAdd ].
	attributeToRemove ifNotNil: [ self unapplyAttributes: {attributeToRemove} ].
	^ true! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 3/9/2023 16:04:25'!
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
		#(		$5 	changeEmphasis:			'Superscript')
		#(		$6 	changeEmphasis:			'Subscript')
		#(		$8	#offerColorMenu:		'Change color')

		#(		$k	#offerFontMenu:			'Set font')

		#(		$u	#align:				'Toggle alignment')
	)! !

