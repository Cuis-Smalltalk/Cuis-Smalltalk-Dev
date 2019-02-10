'From Cuis 5.0 of 7 November 2016 [latest update: #3589] on 7 February 2019 at 12:00:51 am'!
!classDefinition: #HoverableButtonMorph category: #'Morphic-Views'!
PluggableButtonMorph subclass: #HoverableButtonMorph
	instanceVariableNames: 'mouseEnterSelector mouseLeaveSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'AY 2/4/2019 03:26:10'!
atFront
	
	^owner firstSubmorph == self! !

!Morph methodsFor: 'previewing' stamp: 'AY 2/6/2019 23:55:29'!
beginPreview

	(self visible and: [self atFront]) ifFalse: [
		self visibleBeforePreview: self visible.
		self morphBehindBeforePreview: (self owner submorphBehind: self).
		self previewing: true.
		self showAndComeToFront. ]! !

!Morph methodsFor: 'previewing' stamp: 'AY 2/6/2019 23:55:39'!
endPreview
	
	self previewing ifTrue: [
		self visible: self visibleBeforePreview.
		self owner addMorph: self inFrontOf: self morphBehindBeforePreview.
		self previewing: false. ]! !

!Morph methodsFor: 'previewing' stamp: 'AY 2/4/2019 05:46:06'!
endPreviewAndToggleCollapseOrShow

	self endPreview.
	self toggleCollapseOrShow.! !

!Morph methodsFor: 'previewing' stamp: 'AY 2/6/2019 05:28:28'!
morphBehindBeforePreview

	^self valueOfProperty: #morphBehindBeforePreview! !

!Morph methodsFor: 'previewing' stamp: 'AY 2/6/2019 05:28:19'!
morphBehindBeforePreview: aMorph

	self setProperty: #morphBehindBeforePreview toValue: aMorph! !

!Morph methodsFor: 'previewing' stamp: 'AY 2/6/2019 23:56:03'!
previewing

	^(self valueOfProperty: #previewing) = true! !

!Morph methodsFor: 'previewing' stamp: 'AY 2/6/2019 23:56:28'!
previewing: aBoolean

	self setProperty: #previewing toValue: aBoolean! !

!Morph methodsFor: 'previewing' stamp: 'AY 2/6/2019 05:27:23'!
visibleBeforePreview

	^self valueOfProperty: #visibleBeforePreview! !

!Morph methodsFor: 'previewing' stamp: 'AY 2/6/2019 05:25:28'!
visibleBeforePreview: aBoolean

	self setProperty: #visibleBeforePreview toValue: self visible! !


!HoverableButtonMorph methodsFor: 'initialization' stamp: 'AY 2/6/2019 04:01:16'!
model: anObject stateGetter: getStateSel action: actionSel onMouseEnterSend: aMouseEnterSelector onMouseLeaveSend: aMouseLeaveSelector

	super model: anObject stateGetter: getStateSel action: actionSel label: nil.
	mouseEnterSelector _ aMouseEnterSelector.
	mouseLeaveSelector _ aMouseLeaveSelector.! !

!HoverableButtonMorph methodsFor: 'events' stamp: 'AY 2/4/2019 04:04:24'!
mouseEnter: event
	
	mouseEnterSelector ifNotNil: [ model perform: mouseEnterSelector ].
	^super mouseEnter: event! !

!HoverableButtonMorph methodsFor: 'events' stamp: 'AY 2/4/2019 04:05:06'!
mouseLeave: event
	
	mouseLeaveSelector ifNotNil: [ model perform: mouseLeaveSelector ].
	^super mouseLeave: event! !


!HoverableButtonMorph class methodsFor: 'instance creation' stamp: 'AY 2/6/2019 03:59:26'!
model: anObject stateGetter: getStateSel action: actionSel onMouseEnterSend: aMouseEnterSelector onMouseLeaveSend: aMouseLeaveSelector

	^ self new
		model: anObject
		stateGetter: getStateSel
		action: actionSel
		onMouseEnterSend: aMouseEnterSelector
		onMouseLeaveSend: aMouseLeaveSelector! !


!Morph methodsFor: 'submorphs-add/remove' stamp: 'AY 2/4/2019 03:26:20'!
comeToFront

	self atFront ifFalse: [owner addMorphFront: self]! !


!TaskbarMorph methodsFor: 'services' stamp: 'AY 2/6/2019 04:01:56'!
addButtonFor: aMorph

	| button |
	aMorph == self ifFalse: [
		button _ HoverableButtonMorph
			model: aMorph
			stateGetter: nil
			action: #endPreviewAndToggleCollapseOrShow
			onMouseEnterSend: #beginPreview
			onMouseLeaveSend: #endPreview.
		button
			color: self color;
			icon: (aMorph imageForm: 32);
			setBalloonText: #label.
		button icon: button magnifiedIcon.
		viewBox
			addMorph: button
			fixedWidth: self defaultHeight ]! !


!HoverableButtonMorph reorganize!
('initialization' model:stateGetter:action:onMouseEnterSend:onMouseLeaveSend:)
('events' mouseEnter: mouseLeave:)
!


!HoverableButtonMorph class reorganize!
('instance creation' model:stateGetter:action:onMouseEnterSend:onMouseLeaveSend:)
!

