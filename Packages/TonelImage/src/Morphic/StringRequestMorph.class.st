"
This is a simple morph that allows the user to input a string. The user has the option to cancel (with the Esc key), or input a string and then accept (pressing Enter). For example:

	StringRequestMorph
		request: 'What''s your name?'
		initialAnswer: 'Jose'
		do: [:aString| PopUpMenu inform: 'Hello ', aString, '!']

Note that the request is not modal and a handler block must be provided. When the user accepts the input, the handler block is called with the user-provided string as argument. There's also the option to handle cancellation:

	StringRequestMorph
		request: 'What''s your name?'
		initialAnswer: 'Jose'
		do: [:aString| PopUpMenu inform: 'Hello ', aString, '!']
		orCancel: [PopUpMenu inform: 'Ok, nevermind']

And there's the option to provide a validation block that prevents from accepting invalid input:

	StringRequestMorph
		request: 'Guess an even number between 1 and 3'
		initialAnswer: '42'
		verifying: [:aString| aString size > 0 and: [aString allSatisfy: [:each| each isDigit]]]
		do: [:aString| PopUpMenu inform: (aString asInteger = 2 ifTrue: ['Yeah!'] ifFalse: ['Nope'])]

"
Class {
	#name : 'StringRequestMorph',
	#superclass : 'LinearLayoutMorph',
	#instVars : [
		'response',
		'acceptBlock',
		'cancelBlock',
		'validationBlock',
		'textMorph',
		'titleLabelMorph'
	],
	#category : 'Morphic-Composite Widgets',
	#package : 'Morphic',
	#tag : 'Composite Widgets'
}

{ #category : 'new-morph participation' }
StringRequestMorph class >> categoryInNewMorphMenu [

	^ 'User Interface'
]

{ #category : 'instance creation' }
StringRequestMorph class >> defaultInitialAnswer [

	^''
]

{ #category : 'private' }
StringRequestMorph class >> deltaToTextPane [
	"Answer a distance to translate an instance of the receiver by when it is opened in the world in order to have the hand be over the text pane (so the text pane has focus).
	Distance is relative to font size"
	| e |
	e := (Preferences at: #windowTitleFont) lineSpacing.
	^ (0)@(0.5 * e)
]

{ #category : 'new-morph participation' }
StringRequestMorph class >> initializedInstance [
"
	StringRequestMorph initializedInstance.
"
	| stringRequestMorph |
	stringRequestMorph := self 
					request: 'Enter answer here'
					 initialAnswer: 'What is the Answer?' 
					do: [:answer | PopUpMenu inform: answer ].
	^stringRequestMorph
]

{ #category : 'instance creation' }
StringRequestMorph class >> request: queryString [
	"Create an instance of me whose question is queryString. Invoke it centered at the cursor, and answer the string the user accepts. Answer the empty string if the user cancels."
	"Modal. This method returns when an answer is provided."
	"
	StringRequestMorph request: 'What is your favorite color?'
	"

	^ self request: queryString initialAnswer: '' orCancel: nil
]

{ #category : 'instance creation' }
StringRequestMorph class >> request: queryString centeredAt: aPoint initialAnswer: defaultAnswer validationBlock: validationBlock acceptBlock: acceptBlock cancelBlock: cancelBlock [
	"Non-modal. Method returns immediately.
	Uses acceptBlock and cancelBlock to process user's answer later, when provided by them."
	| morph |
	morph := self newColumn
		setQuery: queryString
		initialAnswer: defaultAnswer;
		validationBlock: validationBlock;
		acceptBlock: acceptBlock;
		cancelBlock: cancelBlock.
	self runningWorld addMorph: morph centeredNear: aPoint - self deltaToTextPane.
	"(Preferences at: #focusFollowsMouse) ifFalse: [answer textBox focusText]."
	morph textBox focusText.
	^ morph
]

{ #category : 'instance creation' }
StringRequestMorph class >> request: queryString initialAnswer: defaultAnswer do: acceptBlock [
	"Non-modal. Method returns immediately.
	Uses acceptBlock to process user's answer later, when provided by them."
	
	^ self
		request: queryString
		centeredAt: self runningWorld activeHand morphPosition
		initialAnswer: defaultAnswer
		validationBlock: [:aString| true]
		acceptBlock: acceptBlock
		cancelBlock: nil
]

{ #category : 'instance creation' }
StringRequestMorph class >> request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock [
	"Modal. This method returns after an answer is provided."

	| morph world prevKeyboardFocus response |
	morph := self newColumn
		setQuery: queryString
		initialAnswer: defaultAnswer.
	world := self runningWorld.
	world ifNil: [
		| answer |
		answer := self.	"Just a marker object, can not use nil, because it is a possible answer (if user cancels)"
		UISupervisor whenUIinSafeState: [ answer := self request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock ].
		 [ answer == self ] whileTrue: [ Processor yield ].
		^answer ].
	prevKeyboardFocus := world activeHand keyboardFocus.
	world addMorph: morph centeredNear: world activeHand morphPosition - self deltaToTextPane.
	morph textBox focusText.
	response := morph getUserResponseOrCancel: cancelBlock.
	prevKeyboardFocus ifNotNil: [ world activeHand newKeyboardFocus: prevKeyboardFocus ].
	^response
]

{ #category : 'instance creation' }
StringRequestMorph class >> request: queryString initialAnswer: defaultAnswer verifying: validationBlock [
	"Modal. This method returns when an answer is provided."

	^self request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: nil
]

{ #category : 'instance creation' }
StringRequestMorph class >> request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock [
	"Non-modal. Method returns immediately.
	Uses acceptBlock and cancelBlock to process user's answer later, when provided by them."
	
	^ self 
		request: queryString 
		centeredAt: self runningWorld activeHand morphPosition 
		initialAnswer: defaultAnswer 
		validationBlock: validationBlock 
		acceptBlock: acceptBlock
		cancelBlock: cancelBlock
]

{ #category : 'instance creation' }
StringRequestMorph class >> request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: cancelBlock [
	"Modal. This method returns when an answer is provided."

	| morph world prevKeyboardFocus response |
	morph := self newColumn
		setQuery: queryString
		initialAnswer: defaultAnswer;
		validationBlock: validationBlock.
	world := self runningWorld.
	world ifNil: [
		| answer |
		answer := self.	"Just a marker object, can not use nil, because it is a possible answer (if user cancels)"
		UISupervisor whenUIinSafeState: [
			answer := self request: queryString initialAnswer: defaultAnswer verifying: validationBlock orCancel: cancelBlock ].
		 [ answer == self ] whileTrue: [ Processor yield ].
		^answer ].
	prevKeyboardFocus := world activeHand keyboardFocus.
	world addMorph: morph centeredNear: world activeHand morphPosition - self deltaToTextPane.
	morph textBox focusText.
	response := morph getUserResponseOrCancel: cancelBlock.
	prevKeyboardFocus ifNotNil: [ world activeHand newKeyboardFocus: prevKeyboardFocus ].
	^response
]

{ #category : 'instance creation' }
StringRequestMorph class >> request: queryString orCancel: cancelBlock [
	"Modal. This method returns when an answer is provided."

	^self request: queryString initialAnswer: self defaultInitialAnswer orCancel: cancelBlock 
]

{ #category : 'initialization' }
StringRequestMorph >> acceptBlock: aBlock [
	acceptBlock := aBlock
]

{ #category : 'initialization' }
StringRequestMorph >> addTextPane [
	"Add a text pane and two buttons."
	
	| controlsContainer |
	textMorph := (TextModelMorph
		textProvider: self
		textGetter: #response
		textSetter: #response:
		selectionGetter: #selectionInterval) emptyTextDisplayMessage: 'Enter response'.
	self
		valueOfProperty: #emptyTextDisplayMessage
		ifPresentDo: [ :msg |
			textMorph emptyTextDisplayMessage: msg ].
	textMorph
		hasUnacceptedEdits: true; "Allow accepting initial contents"
		acceptOnCR: true;
		escAction: [ self cancel ].
	(controlsContainer := LinearLayoutMorph newRow)
		addMorphUseAll: textMorph;
		addMorph: 
			((PluggableButtonMorph model: self action: #ok)
				setBalloonText: 'OK' bold, ' (accept input)';
				icon: Theme current acceptIcon);
		addMorph:
			((PluggableButtonMorph model: self action: #cancel)
				setBalloonText: 'Cancel' bold, '  (discard input)';
				icon: Theme current cancelIcon).
	self addMorphUseAll: controlsContainer
]

{ #category : 'initialization' }
StringRequestMorph >> addTitle: aString [
	| titleBarMorph pp w |
	titleBarMorph := ColoredBoxMorph new.
	titleBarMorph color: Theme current menuTitleBar.
	pp := `8@2`.
	aString asPlainString linesDo: [ :line |
		titleLabelMorph := LabelMorph new
			contents: line;
			font: (Preferences at: #standardMenuFont) bold.
		titleBarMorph addMorphBack: titleLabelMorph position: pp.
		pp := pp + (0@(titleLabelMorph morphHeight+2)) ].
	w := titleBarMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleBarMorph morphExtent: (w + 24) @ (pp y).
	self addMorphKeepMorphHeight: titleBarMorph.
	^titleBarMorph morphExtent
]

{ #category : 'private' }
StringRequestMorph >> cancel [
	self delete.
	cancelBlock ifNotNil: [cancelBlock value]
]

{ #category : 'initialization' }
StringRequestMorph >> cancelBlock: aBlock [
	cancelBlock := aBlock
]

{ #category : 'initialization' }
StringRequestMorph >> defaultBorderWidth [
	"Answer the default border width for the receiver."
	^ Theme current windowBorderWidth
]

{ #category : 'initialization' }
StringRequestMorph >> defaultColor [
	^ Theme current menu
]

{ #category : 'accessing' }
StringRequestMorph >> defaultLayerNumber [
	"Morphs with smaller layer number will always cover those with larger ones.
	We want to be above regular morphs."

	^30
]

{ #category : 'drawing' }
StringRequestMorph >> drawOn: aCanvas [
	
	| roundCorners |
	roundCorners := Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas roundRect: self localBounds color: color radius: Theme current roundedWindowRadius ]
		ifFalse: [
			aCanvas fillRectangle: self localBounds color: color borderWidth: borderWidth borderStyleSymbol: #raised baseColorForBorder: color ]
]

{ #category : 'accessing' }
StringRequestMorph >> emptyTextDisplayMessage: aString [
	self
		setProperty: #emptyTextDisplayMessage
		toValue: aString.
]

{ #category : 'private' }
StringRequestMorph >> getUserResponseOrCancel: aBlock [
	"Wait for the user to accept or cancel, and answer the result string. Answers the empty string if the user cancels."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop."
	| w delay done canceled |
	w := self world.
	w isNil ifTrue: [^ response asString asByteStringIfAscii ].
	done := false.
	canceled := false.
	(Preferences at: #focusFollowsMouse) ifFalse: [self textBox focusText].
	acceptBlock := [:aString| done := true].
	cancelBlock := [done := true. canceled := true].
	delay := Delay forMilliseconds: 10.
	[done not and: [self isInWorld]] whileTrue: [ w doOneMinimalCycleNow. delay wait ].
	self delete.
	w doOneMinimalCycleNow.
	canceled ifTrue: [^ aBlock value].
	^ response asString asByteStringIfAscii.
]

{ #category : 'initialization' }
StringRequestMorph >> initialize [
	super initialize.
	extent := `20@10`.
	response := ''
]

{ #category : 'private' }
StringRequestMorph >> ok [
	textMorph scroller acceptContents
]

{ #category : 'accessing' }
StringRequestMorph >> response [
	^ response copy

]

{ #category : 'accessing' }
StringRequestMorph >> response: aText [
	"Sent when text pane accepts."
	| validationResult |
	response := aText.

	"ValidationBlock can evaluate to:
		true: validation passed
		false: validation failed
		a String: validation failed, feedback to user."
	validationBlock ifNotNil: [
		validationResult := validationBlock value: aText asPlainString.
		validationResult == true ifFalse: [
			validationResult isString ifTrue: [
				titleLabelMorph contents: validationResult ].
			self flash.
			^ false ]].

	[
		acceptBlock ifNotNil: [acceptBlock value: aText asPlainString]
	] ensure: [ self delete ].
	^ true
]

{ #category : 'private' }
StringRequestMorph >> selectionInterval [
	^ 1 to: response size

]

{ #category : 'initialization' }
StringRequestMorph >> setQuery: queryString initialAnswer: initialAnswer [
	| titleMorphExtent titleHeight titleWidth requiredLines lineWidth contentsWidth maxLineWidth extraWidth |
	response := initialAnswer.
	titleMorphExtent := self addTitle: queryString.
	titleWidth := 20 * FontFamily defaultPointSize max: titleMorphExtent x.
	titleHeight := titleMorphExtent y + FontFamily defaultLineSpacing. "Some extra room for borders, etc."
	self addTextPane.

	requiredLines := 0.
	"Single line preferred. For instance, for file names and paths."
	(response noneSatisfy: [ :each | each isSeparator ]) ifTrue: [
		maxLineWidth := Display width * 0.8.
		contentsWidth := FontFamily defaultFamilyAndPointSize widthOfString: response.
		contentsWidth < maxLineWidth ifTrue: [
			"Will fit in the screen. Ok."
			requiredLines := 1.
			extraWidth := (10 * FontFamily defaultPointSize). "Some extra room for edition plus accept/cancel buttons." ]].

	"May need to be multiple lines"
	requiredLines = 0 ifTrue: [
		maxLineWidth := Display width // 3.
		contentsWidth := 0.
		response linesDo: [ :eachLine |
			lineWidth := FontFamily defaultFamilyAndPointSize widthOfString: eachLine.
			contentsWidth := contentsWidth max: (lineWidth min: maxLineWidth).
			requiredLines := requiredLines + (lineWidth / maxLineWidth) ceiling ].
		extraWidth := (10 * FontFamily defaultPointSize). "Some extra room for edition plus accept/cancel buttons."
		requiredLines := requiredLines min: 20. "Use scrollbars if needed." 		].

	self morphExtent: ((titleWidth max: contentsWidth) + extraWidth)@(titleHeight + (FontFamily defaultLineSpacing * requiredLines)).
]

{ #category : 'private' }
StringRequestMorph >> textBox [
	
	^textMorph
]

{ #category : 'initialization' }
StringRequestMorph >> validationBlock: aBlock [
	validationBlock := aBlock
]
