'From Cuis7.5 [latest update: #7412] on 13 August 2025 at 6:09:25 pm'!

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'HAW 8/13/2025 17:58:26'!
draw3DLookOn: aCanvas

	| borderStyleSymbol c |
	borderStyleSymbol := self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c := color.
	(self mouseIsOver or: [ self isPressed ]) ifTrue: [
		c := mouseOverColor ifNil: [ c lighter ]].
	c := self enableColorWith: c.
	aCanvas
		fillRectangle: self localBounds
		color: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol
		baseColorForBorder: (borderColor ifNil: [ c ]).

	self drawRegularLabelOn: aCanvas! !

!PluggableButtonMorph methodsFor: 'testing' stamp: 'HAW 8/13/2025 17:58:39'!
isPressed
	^isPressed or: [ self getState ]! !


!PluggableButtonMorph reorganize!
('accessing' actWhen: action: actionSelector adoptWidgetsColor: borderWidth: buttonStyle buttonStyle: font font: icon: iconDrawSelector iconDrawSelector: iconName iconName: label label: label:font: magnifiedIcon: mouseOverColor: noButtonShape padding: performAction)
('drawing' draw3DLookOn: drawEmbossedLabelOn: drawOn: drawRegularLabelOn: drawRoundLookOn: enableColorWith: enableLabelColorWith: fontToUse roundButtonRect)
('events' mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Up:localPosition: mouseEnter: mouseLeave: mouseStillDown)
('event handling testing' handlesMouseDown: handlesMouseOver: handlesMouseStillDown:)
('event handling' mouseStillDownStepRate)
('initialization' actionArgument: actionTarget: defaultBorderColor defaultBorderWidth defaultColor enableSelector: initialize model:stateGetter:action:label: model:stateGetter:stateArgument:action:actionArgument:label: secondaryActionSelector: stateArgument:)
('updating' update:)
('private' actionBlock getEnabled getState magnifiedIcon)
('testing' is: isDisabled isEnabled isPressed isRound mouseIsOver)
('geometry' extentChanged: minimumExtent)
('enabling' disable enable)
('debug and other' buildDebugMenu: debugAction)
!

