"
I represent a list of items. My instances are presented on the display screen in a rectangular area. The user points to an item, pressing a mouse button; the item is highlighted. When the button is released, the highlighted item indicates the selection.
"
Class {
	#name : 'PopUpMenu',
	#superclass : 'Object',
	#instVars : [
		'labelString',
		'lineArray',
		'icons'
	],
	#category : 'Tools-Menus',
	#package : 'Tools',
	#tag : 'Menus'
}

{ #category : 'dialogs' }
PopUpMenu class >> confirm: queryString [
	"Put up a yes/no menu with caption queryString. Answer true if the 
	response is yes, false if no. This is a modal question--the user must 
	respond yes or no."

	"
	PopUpMenu confirm: 'Are you hungry?'
	"

	^ self confirm: queryString trueChoice: 'Yes' falseChoice: 'No' icons: #(acceptIcon cancelIcon)
]

{ #category : 'dialogs' }
PopUpMenu class >> confirm: queryString orCancel: cancelBlock [
	"Put up a yes/no/cancel menu with caption aString. Answer true if  
	the response is yes, false if no. If cancel is chosen, evaluate  
	cancelBlock. This is a modal question--the user must respond yes or no."

	"PopUpMenu confirm: 'Reboot universe' orCancel: [^'Nevermind']"

	| menu choice |
	menu := self labelArray: {'Yes'. 'No'. 'Cancel'} lines: #() icons: #(acceptIcon cancelIcon collapseIcon).
	choice := menu startUpWithCaption: queryString.
	choice = 1 ifTrue: [^ true].
	choice = 2 ifTrue: [^ false].
	^ cancelBlock value
]

{ #category : 'dialogs' }
PopUpMenu class >> confirm: queryString trueChoice: trueChoice falseChoice: falseChoice [
	
	"See #confirm:trueChoice:falseChoice:icons:"

	^self confirm: queryString trueChoice: trueChoice falseChoice: falseChoice icons: #()
]

{ #category : 'dialogs' }
PopUpMenu class >> confirm: queryString trueChoice: trueChoice falseChoice: falseChoice icons: icons [
	"Put up a yes/no menu with caption queryString. The actual wording 
	for the two choices will be as provided in the trueChoice and 
	falseChoice parameters. Answer true if the response is the true-choice, 
	false if it's the false-choice.
	This is a modal question -- the user must respond one way or the other."

	"PopUpMenu confirm: 'Are you hungry?' trueChoice: 'yes, I''m famished' falseChoice: 'no, I just ate'"

	"PopUpMenu  confirm: 'Are you hungry?' trueChoice: 'yes, I''m famished' falseChoice: 'no, I just ate' icons: #(acceptIcon cancelIcon)"

	| menu choice |
	menu := self labelArray: {trueChoice. falseChoice} lines: nil icons: icons.
	[(choice := menu startUpWithCaption: queryString) isNil] whileTrue.
	^ choice = 1
]

{ #category : 'dialogs' }
PopUpMenu class >> inform: aString [
	"PopUpMenu inform: 'I like Cuis'"

	UISupervisor whenUIinSafeState: [
		(self labels: ' OK ' icons: #(emblemImportantIcon)) startUpNonModalWithCaption: aString ]
]

{ #category : 'instance creation' }
PopUpMenu class >> labelArray: labelArray [
	"Answer an instance of me whose items are in labelArray."

	^ self labelArray: labelArray lines: nil
]

{ #category : 'instance creation' }
PopUpMenu class >> labelArray: labelArray lines: lineArray [
	
	^self labelArray: labelArray lines: lineArray icons: #()
]

{ #category : 'instance creation' }
PopUpMenu class >> labelArray: labelArray lines: lineArray icons: icons [
	"Answer an instance of me whose items are in labelArray, with lines 
	drawn after each item indexed by anArray. 2/1/96 sw"

	labelArray isEmpty ifTrue: [self error: 'Menu must not be zero size'].
	^ self
		labels: (String streamContents: [ :stream |
			labelArray do: [:each | stream nextPutAll: each; newLine].
			stream skipBack "remove last newline"])
		lines: lineArray
		icons: icons

"Example:
	(PopUpMenu labelArray: #('frog' 'and' 'toad') lines: #() icons: #()) startUpWithCaption: 'Please pick one.'
"
]

{ #category : 'instance creation' }
PopUpMenu class >> labels: aString [
	"Answer an instance of me whose items are in aString."
	^self labels: aString lines: nil
]

{ #category : 'instance creation' }
PopUpMenu class >> labels: aString icons: icons [
	"Answer an instance of me whose items are in aString."

	^self labels: aString lines: nil icons: icons
]

{ #category : 'instance creation' }
PopUpMenu class >> labels: aString lines: anArray [
	"Answer an instance of me whose items are in aString, with lines drawn 
	after each item indexed by anArray."

	^ self labels: aString lines: anArray icons: #()
]

{ #category : 'instance creation' }
PopUpMenu class >> labels: aString lines: anArray icons: icons [
	"Answer an instance of me whose items are in aString, with lines drawn 
	after each item indexed by anArray and icons per item."

	^ self new labels: aString lines: anArray icons: icons
]

{ #category : 'dialogs' }
PopUpMenu class >> notify: message [
	"Deprecated. Use #inform: instead."

	self inform: message
]

{ #category : 'instance creation' }
PopUpMenu class >> withCaption: cap chooseFrom: labels [
	"Simply put up a menu.  Get the args in the right order with the caption first.  labels may be either an array of items or a string with CRs in it.  May use backslashes for returns."

	^ (labels isString
		ifTrue: [ self labels: labels withNewLines lines: nil ]
		ifFalse: [ self labelArray: labels lines: nil ])
			startUpWithCaption: cap withNewLines
]

{ #category : 'accessing' }
PopUpMenu >> frameHeight [
	"Designed to avoid the entire frame computation (includes MVC form),
	since the menu may well end up being displayed in Morphic anyway."
	| nItems |
	nItems := 1 + labelString lineCount.
	^ (nItems * (Preferences at: #standardMenuFont) lineSpacing) + 4 "border width"
]

{ #category : 'accessing' }
PopUpMenu >> iconAt: aPosition [

	^icons at: aPosition ifAbsent: nil
]

{ #category : 'accessing' }
PopUpMenu >> labelString [

	^ labelString
]

{ #category : 'private' }
PopUpMenu >> labels: aString lines: anArray [

	self labels: aString lines: anArray icons: #()
]

{ #category : 'private' }
PopUpMenu >> labels: aString lines: anArray icons: iconCollection [

	labelString := aString.
	lineArray := anArray.
	icons := iconCollection 

]

{ #category : 'accessing' }
PopUpMenu >> lineArray [

	^ lineArray
]

{ #category : 'basic control sequence' }
PopUpMenu >> startUpMenu [
	"Display and make a selection from the receiver as long as the button 
	is pressed. Answer the current selection."
	
	^ self startUpWithCaption: nil
]

{ #category : 'basic control sequence' }
PopUpMenu >> startUpNonModalWithCaption: captionOrNil [
	"Display the menu, slightly offset from the cursor,
	so that a slight tweak is required to confirm any action."
	^ self 
		startUpNonModalWithCaption: captionOrNil 
		at: Sensor mousePoint 
		allowKeyboard: (Preferences at: #menuKeyboardControl)
]

{ #category : 'basic control sequence' }
PopUpMenu >> startUpNonModalWithCaption: captionOrNil at: location allowKeyboard: aBoolean  [
	"Display the menu, with caption if supplied. Wait for the mouse button to go down, then track the selection as long as the button is pressed. When the button is released,
	Answer the index of the current selection, or zero if the mouse is not released over  any menu item. Location specifies the desired topLeft of the menu body rectangle. The final argument indicates whether the menu should seize the keyboard focus in order to allow the user to navigate it via the keyboard."

	| maxHeight |
	maxHeight := Display height * 3 // 4.
	self frameHeight > maxHeight 
		ifTrue: [
			^self 
				startUpSegmented: maxHeight
				withCaption: captionOrNil
				at: location
				allowKeyboard: aBoolean].
	^(MVCMenuMorph from: self title: captionOrNil) 
		invokeNonModalAt: location
		allowKeyboard: aBoolean
]

{ #category : 'basic control sequence' }
PopUpMenu >> startUpSegmented: segmentHeight withCaption: captionOrNil at: location allowKeyboard: aBoolean [
	"This menu is too big to fit comfortably on the screen.
	Break it up into smaller chunks, and manage the relative indices.
	Inspired by a special-case solution by Reinier van Loon.  The boolean parameter indicates whether the menu should be given keyboard focus (if in morphic)"

"
(PopUpMenu labels: (String streamContents: [:s | 1 to: 100 do: [:i | s print: i; newLine]. s skipBack])
		lines: (5 to: 100 by: 5)) startUpWithCaption: 'Give it a whirl...'.
"
	| nLines nLinesPer allLabels from to subset subLines index |
	allLabels := labelString lines.
	nLines := allLabels size.
	lineArray ifNil: [lineArray := #()].
	nLinesPer := segmentHeight // (Preferences at: #standardMenuFont) lineSpacing - 5.
	from := 1.
	[ true ] whileTrue: [
		to := (from + nLinesPer) min: nLines.
		subset := (allLabels copyFrom: from to: to) asOrderedCollection.
		subset add: (to = nLines ifTrue: ['start over...'] ifFalse: ['more...'])
			before: subset first.
		subLines := lineArray select: [:n | n >= from] thenCollect: [:n | n - (from-1) + 1].
		subLines := (Array with: 1) , subLines.
		index := (PopUpMenu labels: subset printStringWithNewline lines: subLines)
					startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean.
		index = 1
			ifTrue: [from := to + 1.
					from > nLines ifTrue: [ from := 1 ]]
			ifFalse: [index = 0 ifTrue: [^ 0].
					^ from + index - 2]]
]

{ #category : 'basic control sequence' }
PopUpMenu >> startUpWithCaption: captionOrNil [
	"Display the menu, slightly offset from the cursor,
	so that a slight tweak is required to confirm any action."
	^ self 
		startUpWithCaption: captionOrNil 
		at: Sensor mousePoint 
		allowKeyboard: (Preferences at: #menuKeyboardControl)
]

{ #category : 'basic control sequence' }
PopUpMenu >> startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean  [
	"Display the menu, with caption if supplied. Wait for the mouse button to go down, then track the selection as long as the button is pressed. When the button is released,
	Answer the index of the current selection, or zero if the mouse is not released over  any menu item. Location specifies the desired topLeft of the menu body rectangle. The final argument indicates whether the menu should seize the keyboard focus in order to allow the user to navigate it via the keyboard."

	| maxHeight |
	maxHeight := Display height * 3 // 4.
	self frameHeight > maxHeight 
		ifTrue: [
			^self 
				startUpSegmented: maxHeight
				withCaption: captionOrNil
				at: location
				allowKeyboard: aBoolean].
	^(MVCMenuMorph from: self title: captionOrNil) 
		invokeAt: location
		allowKeyboard: aBoolean
]
