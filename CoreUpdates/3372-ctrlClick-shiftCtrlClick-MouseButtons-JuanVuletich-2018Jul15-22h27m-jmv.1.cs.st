'From Cuis 5.0 of 7 November 2016 [latest update: #3371] on 15 July 2018 at 10:28:26 pm'!

!MouseEvent methodsFor: 'button state' stamp: 'jmv 7/15/2018 22:24:54'!
mouseButton1Pressed
	"Answer true if the mouseButton1 is being pressed.
	This is the first mouse button, usually the one at the left.
	But if they are combined with ctrl or option(Mac) keys, it is considered button 2 or 3 (depending on shift).
	See also #mouseButton1Changed"

	self controlKeyPressed ifTrue: [ ^ false ].
	^ buttons anyMask: InputSensor mouseButton1! !

!MouseEvent methodsFor: 'button state' stamp: 'jmv 7/15/2018 22:23:15'!
mouseButton2Pressed
	"Answer true if the mouseButton2 is being pressed.
	This is usually the right mouse button or option+click on the Mac.
	It is also emulated here with shift-ctrl-click on any platform."

	(self controlKeyPressed and: [self shiftPressed] and: [ buttons anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	^ buttons anyMask: InputSensor mouseButton2! !

!MouseEvent methodsFor: 'button state' stamp: 'jmv 7/15/2018 22:21:11'!
mouseButton3Pressed
	"Answer true if the mouseButton3 is being pressed.
	This is usually the center (wheel) mouse button or cmd+click on the Mac or ctrl+click on Linux.
	It is also emulated here with ctrl-click on any platform (i.e. Windows)."

	(self controlKeyPressed and: [self shiftPressed not] and: [ buttons anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	^ buttons anyMask: InputSensor mouseButton3! !


!MouseButtonEvent methodsFor: 'accessing' stamp: 'jmv 7/15/2018 22:25:04'!
mouseButton1Changed
	"Answer true if the mouseButton1 has changed.
	This is usually the left mouse button.
	But if they are combined with ctrl or option(Mac) keys, it is considered button 2 or 3 (depending on shift).
	The check for button change (instead of button press) is specially useful on buttonUp events.
	See also #mouseButton1Pressed"

	self controlKeyPressed ifTrue: [ ^ false ].
	^ whichButton anyMask: InputSensor mouseButton1! !

!MouseButtonEvent methodsFor: 'accessing' stamp: 'jmv 7/15/2018 22:22:40'!
mouseButton2Changed
	"Answer true if the mouseButton2 has changed.
	This is the usually the right mouse button or option+click on the Mac.
	It is also emulated here with shift-ctrl-click on any platform.
	The check for button change (instead of button press) is specially useful on buttonUp events."

	(self controlKeyPressed and: [self shiftPressed] and: [ whichButton anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	^ whichButton anyMask: InputSensor mouseButton2! !

!MouseButtonEvent methodsFor: 'accessing' stamp: 'jmv 7/15/2018 22:22:16'!
mouseButton3Changed
	"Answer true if the mouseButton3 has changed.
	This is usually the center (wheel) mouse button or cmd+click on the Mac or ctrl+click on Linux.
	It is also emulated here with ctrl-click on any platform (i.e. Windows).
	The check for button change (instead of button press) is specially useful on buttonUp events."

	(self controlKeyPressed and: [self shiftPressed not] and: [ whichButton anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	^ whichButton anyMask: InputSensor mouseButton3! !

!methodRemoval: Preferences class #commandClickOpensHalo!
Preferences class removeSelector: #commandClickOpensHalo!
