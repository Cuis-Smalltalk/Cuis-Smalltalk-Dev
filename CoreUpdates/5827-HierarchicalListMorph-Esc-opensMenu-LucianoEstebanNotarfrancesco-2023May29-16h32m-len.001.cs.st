'From Cuis 6.0 [latest update: #5826] on 29 May 2023 at 4:33:19 pm'!

!HierarchicalListMorph methodsFor: 'events' stamp: 'len 5/25/2023 09:56:32'!
keyStroke: aKeyboardEvent 
	"Process potential command keys"

	| args |
	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self arrowKey: aKeyboardEvent) ifNotNil: [ ^ self ].
	aKeyboardEvent isEsc ifTrue: [ " escape key" ^ self mouseButton2Activity ].
	keystrokeActionSelector ifNil: [^self].
	(args := keystrokeActionSelector numArgs) = 1 
		ifTrue: [^mainView perform: keystrokeActionSelector with: aKeyboardEvent keyCharacter].
	args = 2 
		ifTrue: [
			^mainView 
				perform: keystrokeActionSelector
				with: aKeyboardEvent keyCharacter
				with: self].
	^self error: 'The keystrokeActionSelector must be a 1- or 2-keyword symbol'! !

