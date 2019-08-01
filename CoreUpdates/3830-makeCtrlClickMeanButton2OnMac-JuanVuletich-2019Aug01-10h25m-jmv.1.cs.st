'From Cuis 5.0 [latest update: #3829] on 1 August 2019 at 11:18:12 am'!

!MouseEvent methodsFor: 'button state' stamp: 'jmv 8/1/2019 10:45:48'!
turnMouseButton1Into2
	"Answer true if modifier keys are such that button 1 should be considered as button 2.
	ctrl - click -> right click
	"

	(self controlKeyPressed and: [self shiftPressed not]) ifTrue: [ ^ true ].
	^ false! !

!MouseEvent methodsFor: 'button state' stamp: 'jmv 8/1/2019 11:01:35'!
turnMouseButton1Into3
	"Answer true if modifier keys are such that button 1 should be considered as button 3.
	ctrl - shift - click -> center click
	alt -> click -> center click (effective only on Windows,
						the vm on Mac already reports center click, and on Linux right click)
	"

	(self controlKeyPressed and: [self shiftPressed]) ifTrue: [ ^ true ].
	self commandAltKeyPressed ifTrue: [ ^ true ].
	^ false! !


!MouseEvent methodsFor: 'button state' stamp: 'jmv 8/1/2019 11:14:36'!
mouseButton1Pressed
	"Answer true if the mouseButton1 is being pressed.
	Reported by the VM for the single/first mouse button, usually the one at the left.
	But if they are combined with modifier keys, it is might button 2 or 3.
		See mouseButton2Pressed and mouseButton3Pressed.
	See also #mouseButton1Changed"

	self turnMouseButton1Into2 ifTrue: [ ^ false ].
	self turnMouseButton1Into3 ifTrue: [ ^ false ].
	^ buttons anyMask: InputSensor mouseButton1! !

!MouseEvent methodsFor: 'button state' stamp: 'jmv 8/1/2019 11:12:43'!
mouseButton2Pressed
	"Answer true if the mouseButton2 is being pressed.
	Reported by the VM for right mouse button or option+click on the Mac, ctrl-click on Windows, or ctrl-click or alt-click on Linux.
	It is also emulated here with ctrl-click on any platform."

	(self turnMouseButton1Into2 and: [ buttons anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	^ buttons anyMask: InputSensor mouseButton2! !

!MouseEvent methodsFor: 'button state' stamp: 'jmv 8/1/2019 11:12:08'!
mouseButton3Pressed
	"Answer true if the mouseButton3 is being pressed.
	Reported by the VM for center (wheel) mouse button or cmd+click on the Mac or win/meta+click on Windows and Linux.
	It is also emulated here with shift-ctrl-click on any platform."

	(self turnMouseButton1Into3 and: [ buttons anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	^ buttons anyMask: InputSensor mouseButton3! !


!MouseButtonEvent methodsFor: 'accessing' stamp: 'jmv 8/1/2019 11:14:43'!
mouseButton1Changed
	"Answer true if the mouseButton1 has changed.
	Reported by the VM for the single/first mouse button, usually the one at the left.
	But if they are combined with modifier keys, it is might button 2 or 3.
		See mouseButton1Changed and mouseButton3Changed.
	The check for button change (instead of button press) is specially useful on buttonUp events.
	See also #mouseButton1Pressed"

	self turnMouseButton1Into2 ifTrue: [ ^ false ].
	self turnMouseButton1Into3 ifTrue: [ ^ false ].
	^ whichButton anyMask: InputSensor mouseButton1! !

!MouseButtonEvent methodsFor: 'accessing' stamp: 'jmv 8/1/2019 11:12:49'!
mouseButton2Changed
	"Answer true if the mouseButton2 has changed.
	Reported by the VM for right mouse button or option+click on the Mac.
	It is also emulated here with ctrl-click on any platform.
	The check for button change (instead of button press) is specially useful on buttonUp events."

	(self turnMouseButton1Into2 and: [ whichButton anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	^ whichButton anyMask: InputSensor mouseButton2! !

!MouseButtonEvent methodsFor: 'accessing' stamp: 'jmv 8/1/2019 11:12:33'!
mouseButton3Changed
	"Answer true if the mouseButton3 has changed.
	Reported by the VM for center (wheel) mouse button or cmd+click on the Mac or meta+click on Linux.
	It is also emulated here with shift-ctrl-click on any platform.
	The check for button change (instead of button press) is specially useful on buttonUp events."

	(self turnMouseButton1Into3 and: [ whichButton anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	^ whichButton anyMask: InputSensor mouseButton3! !

