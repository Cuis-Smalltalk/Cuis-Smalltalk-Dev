'From Cuis 5.0 [latest update: #4666] on 15 July 2021 at 3:29:59 pm'!

!TextEditor methodsFor: 'attributes' stamp: 'jmv 7/15/2021 15:23:41'!
offerColorMenu
	"Present a menu of available colors, and if one is chosen, apply it to the current selection."
	"This is a user command, and generates undo"

	| attribute colors index thisSel |
	thisSel _ self selection.
	colors _ #(#black #magenta #red #yellow #green #blue #cyan #white ).
	index _ (PopUpMenu
		labelArray: colors , #('choose color...' )
		lines: (Array with: colors size + 1)) startUpMenu.
	index = 0 ifTrue: [ ^ true ].
	index <= colors size
		ifTrue: [ attribute _ TextColor color: (Color perform: (colors at: index)) ]
		ifFalse: [
			index _ index - colors size - 1.
			"Re-number!!!!!!"
			index = 0 ifTrue: [ attribute _ self chooseColor ].
			thisSel ifNil: [ ^ true ]].
	attribute ifNotNil: [ self applyAttribute: attribute ].
	morph updateFromTextComposition.
	^ true.! !

!TextEditor methodsFor: 'menu' stamp: 'jmv 7/15/2021 15:29:16'!
addStyleMenuSectionTo: aMenu
	"Adds to the given menu text styiling related operations"
	
	aMenu
		addItemsFromDictionaries: 
	`{
		{
				#label 			-> 		'Toggle WordWrap'.
				#selector 			-> 		#wrapOnOff.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#label 			-> 		'Set Font... (k)'.
				#selector 			-> 		#offerFontMenu.
				#icon 			-> 		#preferencesDesktopFontIcon
			} asDictionary.
			{
				#label 			-> 		'Clear Font'.
				#selector 			-> 		#clearFont.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#label 			-> 		'Set Font Style'.
				#selector 			-> 		#offerFontStyleMenu.
				#icon 			-> 		#preferencesDesktopFontIcon
			} asDictionary.
			{
				#label 			-> 		'Set Text Color'.
				#selector 			-> 		#offerColorMenu.
				#icon 			-> 		#graphicsIcon
			} asDictionary.
			{
				#label 			-> 		'Set Default Font...'.
				#selector 			-> 		#offerDefaultFontMenu.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			{
				#label 			-> 		'Set Alignment...'.
				#selector 			-> 		#chooseAlignment.
				#icon 			-> 		#formatJustifyLeftIcon
			} asDictionary.
		}`.
		^aMenu! !

