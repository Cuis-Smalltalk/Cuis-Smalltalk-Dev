'From Cuis 6.0 [latest update: #5839] on 6 June 2023 at 8:19:22 pm'!

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/6/2023 20:08:05'!
changeEmphasis: aKeyboardEvent
	"Change the emphasis of the current selection or prepare to accept characters with the change in emphasis."
	"control 0..9 -> 0..9"
	"This is a user command, and generates undo"

	| keyCode attributeToAdd attributesToRemove oldAttributes |
	keyCode := ('0123456'
		indexOf: aKeyboardEvent keyCharacter
		ifAbsent: [ 1 ]) - 1.
	oldAttributes := emphasisHere.

	keyCode = 0
		ifTrue: [
			"Remove all TextEmphasis"
			attributeToAdd := nil.
			attributesToRemove := oldAttributes ]
		ifFalse: [
			"Toggle the indicated TextEmphasis"
			(keyCode between: 1 and: 6) ifTrue: [
				attributeToAdd := TextEmphasis perform: (
					#(#bold #italic #underlined #struckThrough #superscript #subscript)
						at: keyCode).
				oldAttributes do: [ :oldAtt |
					oldAtt = attributeToAdd ifTrue: [
						attributeToAdd := nil.
						attributesToRemove := {oldAtt} ]]]].

	attributeToAdd ifNotNil: [ self applyAttribute: attributeToAdd ].
	attributesToRemove ifNotNil: [ self unapplyAttributes: attributesToRemove ].
	^ true! !

