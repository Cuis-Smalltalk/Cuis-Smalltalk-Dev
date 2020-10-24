'From Cuis 5.0 [latest update: #4416] on 23 October 2020 at 4:14:51 pm'!
!classDefinition: #StringRequestMorph category: 'Morphic-Composite Widgets'!
LayoutMorph subclass: #StringRequestMorph
	instanceVariableNames: 'response acceptBlock cancelBlock validationBlock '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 10/23/2020 15:37:44'!
layoutBounds
	"Return the bounds for laying out children of the receiver"

	^ self morphLocalBounds insetBy: borderWidth! !


!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 10/23/2020 16:06:31'!
addTextPane
	| result |
	result _ (TextModelMorph
		textProvider: self
		textGetter: #response
		textSetter: #response:
		selectionGetter: #selectionInterval) emptyTextDisplayMessage: 'Enter response'.
	self
		valueOfProperty: #emptyTextDisplayMessage
		ifPresentDo: [ :msg |
			result emptyTextDisplayMessage: msg ].
	result
		hasUnacceptedEdits: true;
		acceptOnCR: true;
		escAction: [ self cancel ].
	self addMorph: result layoutSpec: LayoutSpec useAll.! !

!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 10/23/2020 16:14:23'!
addTitle: aString
	| titleMorph s pp w |
	titleMorph _ WidgetMorph new noBorder.
	titleMorph color: Theme current menuTitleBar.
	pp _ `8@2`.
	aString asString linesDo: [ :line |
		s _ LabelMorph new
			contents: line;
			font: Preferences standardMenuFont bold.
		titleMorph addMorphBack: s position: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphExtent: (w + 24) @ (pp y).
	self addMorphKeepMorphHeight: titleMorph.
	^titleMorph morphWidth! !

!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 10/23/2020 15:45:59'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 5! !

!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 10/23/2020 16:14:08'!
setQuery: queryString initialAnswer: initialAnswer
	| lineCount newExtent w |
	response _ initialAnswer.
	w _ self addTitle: queryString.
	self addTextPane.
	"If the initial response is big or has multiple lines we make the text pane bigger and with scrollbars:"
	lineCount _ response lineCount.
	newExtent _ lineCount > 1
		ifTrue: [ 40 @ (lineCount*2) * FontFamily defaultLineSpacing ]
		ifFalse: [ (response size max: 12) @ 3.2 * FontFamily defaultLineSpacing ].
	self morphExtent: (newExtent max: w@0)! !


!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 10/23/2020 15:32:25'!
request: queryString centeredAt: aPoint initialAnswer: defaultAnswer validationBlock: validationBlock acceptBlock: acceptBlock cancelBlock: cancelBlock
	| answer |
	answer _ self newColumn
		setQuery: queryString
		initialAnswer: defaultAnswer;
		validationBlock: validationBlock;
		acceptBlock: acceptBlock;
		cancelBlock: cancelBlock.
	self runningWorld addMorph: answer centeredNear: aPoint - self deltaToTextPane.
	^ answer! !

!StringRequestMorph class methodsFor: 'instance creation' stamp: 'jmv 10/23/2020 15:32:30'!
request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock

	| morph world |

	morph _ self newColumn
		setQuery: queryString
		initialAnswer: defaultAnswer.
	(world _ self runningWorld) addMorph: morph centeredNear: world activeHand morphPosition - self deltaToTextPane.
	
	^ morph getUserResponseOrCancel: cancelBlock! !

!StringRequestMorph class methodsFor: 'private' stamp: 'jmv 10/23/2020 15:54:06'!
deltaToTextPane
	"Answer a distance to translate an instance of the receiver by when it is opened in the world in order to have the hand be over the text pane (so the text pane has focus).
	Distance is relative to font size"
	| e |
	e _ Preferences windowTitleFont lineSpacing.
	^ (0)@(0.5 * e)! !

!methodRemoval: StringRequestMorph #intoWorld: stamp: 'jmv 10/23/2020 16:05:21'!
StringRequestMorph removeSelector: #intoWorld:!
!methodRemoval: StringRequestMorph #adjustSubmorphsLayout stamp: 'jmv 10/23/2020 16:05:21'!
StringRequestMorph removeSelector: #adjustSubmorphsLayout!
!classDefinition: #StringRequestMorph category: 'Morphic-Composite Widgets'!
LayoutMorph subclass: #StringRequestMorph
	instanceVariableNames: 'response acceptBlock cancelBlock validationBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!
