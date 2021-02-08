'From Cuis 5.0 [latest update: #4528] on 8 February 2021 at 5:52:57 pm'!

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 2/8/2021 17:17:37'!
updateDownButton: aPluggableButtonMorph
	"update the argument as a downButton.  put a new image inside"

	aPluggableButtonMorph
		icon: (BitBltCanvas arrowOfDirection: #down size: ScrollBar scrollbarThickness);
		iconName: #drawDownIcon;
		action: #scrollDown;
		roundButtonStyle: false;
		redrawNeeded! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 2/8/2021 17:36:03'!
updateLeftButton: aPluggableButtonMorph
	"update the argument as a leftButton.  put a new image inside"

	aPluggableButtonMorph
		icon: (BitBltCanvas arrowOfDirection: #left size: ScrollBar scrollbarThickness);
		iconName: #drawLeftIcon;
		action: #scrollUp;
		roundButtonStyle: false;
		redrawNeeded! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 2/8/2021 17:19:03'!
updateRightButton: aPluggableButtonMorph
	"update the argument as a rightButton.  put a new image inside"

	aPluggableButtonMorph
		icon: (BitBltCanvas arrowOfDirection: #right size: ScrollBar scrollbarThickness);
		iconName: #drawRightIcon;
		action: #scrollDown;
		roundButtonStyle: false;
		redrawNeeded! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 2/8/2021 17:19:41'!
updateUpButton: aPluggableButtonMorph
	"update the argument as a rightButton.  put a new image inside"

	aPluggableButtonMorph
		icon: (BitBltCanvas arrowOfDirection: #up size: ScrollBar scrollbarThickness);
		iconName: #drawUpIcon;
		action: #scrollUp;
		roundButtonStyle: false;
		redrawNeeded! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 2/8/2021 17:51:52'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."
	| isH wasH |

	super extentChanged: oldExtent.
	"Doesn't move!!"
	"upButton morphPosition: borderWidth@borderWidth."
	downButton morphPosition: extent - borderWidth - downButton morphExtent.
	wasH _ oldExtent notNil and: [oldExtent x > oldExtent y].
	isH _ self isHorizontal.
	isH = wasH ifFalse: [
		isH
			ifTrue: [
				self updateLeftButton: upButton.
				self updateRightButton: downButton ]
			ifFalse: [
				self updateUpButton: upButton.
				self updateDownButton: downButton ]].! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 2/8/2021 17:21:07'!
initializeDownButton
	"initialize the receiver's downButton"

	| e |
	e _ self buttonExtent.
	downButton _ self buttonClass new.
	downButton model: self.
	downButton morphExtent: e@e.
	Theme current minimalWindows ifTrue: [^ self].
	self addMorph: downButton position: extent - borderWidth - e.
	downButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down".
	self isHorizontal
		ifTrue: [ self updateRightButton: downButton ]
		ifFalse: [ self updateDownButton: downButton ]! !

!ScrollBar methodsFor: 'initialization' stamp: 'jmv 2/8/2021 17:35:47'!
initializeUpButton
	"initialize the receiver's upButton"

	| e |
	e _ self buttonExtent.
	upButton _ self buttonClass new.
	upButton model: self.
	upButton morphExtent: e@e.
	Theme current minimalWindows ifTrue: [^ self].
	self addMorph: upButton position: borderWidth@borderWidth.
	upButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down"
	self isHorizontal
		ifTrue: [ self updateLeftButton: upButton ]
		ifFalse: [ self updateUpButton: upButton ].
! !

!methodRemoval: PluggableButtonMorph #updateDownButtonImage stamp: 'jmv 2/8/2021 17:52:11'!
PluggableButtonMorph removeSelector: #updateDownButtonImage!
!methodRemoval: PluggableButtonMorph #updateUpButtonImage stamp: 'jmv 2/8/2021 17:52:11'!
PluggableButtonMorph removeSelector: #updateUpButtonImage!
!methodRemoval: PluggableButtonMorph #updateLeftButtonImage stamp: 'jmv 2/8/2021 17:52:11'!
PluggableButtonMorph removeSelector: #updateLeftButtonImage!
!methodRemoval: PluggableButtonMorph #updateRightButtonImage stamp: 'jmv 2/8/2021 17:52:11'!
PluggableButtonMorph removeSelector: #updateRightButtonImage!
