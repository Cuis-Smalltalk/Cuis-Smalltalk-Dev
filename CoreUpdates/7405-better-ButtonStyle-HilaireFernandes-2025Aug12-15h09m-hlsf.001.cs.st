'From Cuis7.5 [latest update: #7404] on 12 August 2025 at 3:21:02 pm'!
!classDefinition: #PluggableButtonMorph category: #'Morphic-Widgets'!
PluggableMorph subclass: #PluggableButtonMorph
	instanceVariableNames: 'label font icon stateSelector enableSelector actionSelector isPressed mouseIsOver magnifiedIcon actWhen roundButtonStyle iconName secondaryActionSelector stateArgument actionArgument mouseOverColor buttonStyle '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'hlsf 8/12/2025 17:51:07'!
buttonStyle
	^ buttonStyle ! !

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'hlsf 8/12/2025 17:53:39'!
buttonStyle: aSymbol
" 
	#none 			Draw only the icon and or label, with no button shape
	#theme 		Honor theme
	#round 			Draw as round button
	#rectangular 	Draw as classic 3d border rectangular button
"

	buttonStyle := aSymbol ! !

!PluggableButtonMorph methodsFor: 'testing' stamp: 'hlsf 8/12/2025 18:03:16'!
isRound
	^ buttonStyle == #round 
	or: [ buttonStyle == #theme and: [Theme current roundButtons] ]
! !


!PluggableButtonMorph methodsFor: 'accessing' stamp: 'hlsf 8/12/2025 17:53:39'!
noButtonShape
	""
	self buttonStyle: #none.
	self redrawNeeded.! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'hlsf 8/12/2025 18:02:59'!
drawOn: aCanvas
	buttonStyle == #none ifFalse: [
		self isRound 
			ifTrue:  [ self drawRoundLookOn: aCanvas ]
			ifFalse: [ self draw3DLookOn: aCanvas ]].

	aCanvas drawButtonIconFromCurrentMorph.! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'hlsf 8/12/2025 17:53:39'!
initialize
	"initialize the state of the receiver"
	super initialize.

	self buttonStyle: #theme.	
	model := nil.
	enableSelector := nil.
	stateSelector := nil.
	stateArgument := nil.
	actionSelector := nil.
	actionArgument := nil.
	isPressed := false.
	mouseIsOver := false.
	mouseOverColor := nil.
	actWhen := #buttonUp.
	extent :=  `20 @ 15`! !


!ScrollBar methodsFor: 'initialization' stamp: 'hlsf 8/12/2025 17:53:52'!
updateDownButton: aPluggableButtonMorph
	"Update the argument as a downButton."

	aPluggableButtonMorph
		iconDrawSelector: #drawDownIcon;
		action: #scrollDown;
		buttonStyle: #rectangular;
		redrawNeeded! !

!ScrollBar methodsFor: 'initialization' stamp: 'hlsf 8/12/2025 17:54:02'!
updateLeftButton: aPluggableButtonMorph
	"Update the argument as a leftButton."

	aPluggableButtonMorph
		iconDrawSelector: #drawLeftIcon;
		action: #scrollUp;
		buttonStyle: #rectangular;
		redrawNeeded! !

!ScrollBar methodsFor: 'initialization' stamp: 'hlsf 8/12/2025 17:54:09'!
updateRightButton: aPluggableButtonMorph
	"Update the argument as a rightButton."

	aPluggableButtonMorph
		iconDrawSelector: #drawRightIcon;
		action: #scrollDown;
		buttonStyle: #rectangular;
		redrawNeeded! !

!ScrollBar methodsFor: 'initialization' stamp: 'hlsf 8/12/2025 17:54:16'!
updateUpButton: aPluggableButtonMorph
	"Update the argument as an upButton."

	aPluggableButtonMorph
		iconDrawSelector: #drawUpIcon;
		action: #scrollUp;
		buttonStyle: #rectangular;
		redrawNeeded! !

!methodRemoval: PluggableButtonMorph #roundButtonStyle: stamp: 'hlsf 8/12/2025 15:19:02'!
PluggableButtonMorph removeSelector: #roundButtonStyle:!
!classDefinition: #PluggableButtonMorph category: #'Morphic-Widgets'!
PluggableMorph subclass: #PluggableButtonMorph
	instanceVariableNames: 'label font icon stateSelector enableSelector actionSelector isPressed mouseIsOver magnifiedIcon actWhen roundButtonStyle iconName secondaryActionSelector stateArgument actionArgument mouseOverColor buttonStyle'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
PluggableButtonMorph allSubInstancesDo: [:aButton |
	| roundButtonStyle newStyle |
	roundButtonStyle := aButton instVarNamed: 'roundButtonStyle'.
	roundButtonStyle == nil ifTrue: [newStyle := #theme].
	roundButtonStyle == true ifTrue: [newStyle := #round].
	roundButtonStyle == false ifTrue: [newStyle := #rectangular].
	aButton buttonStyle: newStyle.
	aButton redrawNeeded]!

