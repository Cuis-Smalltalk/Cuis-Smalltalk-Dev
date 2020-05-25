'From Cuis 5.0 [latest update: #4176] on 24 May 2020 at 9:41:38 pm'!
!classDefinition: #PluggableButtonMorph category: #'Morphic-Views'!
PluggableMorph subclass: #PluggableButtonMorph
	instanceVariableNames: 'label font icon getStateSelector actionSelector isPressed mouseIsOver magnifiedIcon actWhen roundButtonStyle iconName secondaryActionSelector '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views'!

!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 5/24/2020 21:38:17'!
mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition

	secondaryActionSelector ifNotNil: [
		model perform: secondaryActionSelector ].
	self redrawNeeded! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 5/24/2020 21:36:57'!
secondaryActionSelector: actionSel

	secondaryActionSelector _ actionSel.! !


!HoverableButtonMorph methodsFor: 'initialization' stamp: 'jmv 5/24/2020 21:35:06'!
model: anObject stateGetter: getStateSel action: actionSel onMouseEnterSend: aMouseEnterSelector onMouseLeaveSend: aMouseLeaveSelector

	self model: anObject stateGetter: getStateSel action: actionSel label: nil.
	mouseEnterSelector _ aMouseEnterSelector.
	mouseLeaveSelector _ aMouseLeaveSelector.! !


!TaskbarMorph methodsFor: 'services' stamp: 'jmv 5/24/2020 21:40:25'!
addButtonFor: aMorph

	| button |
	aMorph == self ifFalse: [
		button _ HoverableButtonMorph
			model: aMorph
			stateGetter: nil
			action: #endPreviewAndToggleCollapseOrShow
			onMouseEnterSend: #beginPreview
			onMouseLeaveSend: #endPreview.
		(aMorph is: #SystemWindow) ifTrue: [
			button
				secondaryActionSelector: #offerWindowMenu ].
		button
			color: self color;
			icon: (aMorph imageForm: 400@300 depth: 32);
			setBalloonText: #label.
		button icon: button magnifiedIcon.
		viewBox
			addMorph: button
			fixedWidth: self defaultHeight ]! !

!classDefinition: #PluggableButtonMorph category: #'Morphic-Views'!
PluggableMorph subclass: #PluggableButtonMorph
	instanceVariableNames: 'label font icon getStateSelector actionSelector isPressed mouseIsOver magnifiedIcon actWhen roundButtonStyle iconName secondaryActionSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views'!
