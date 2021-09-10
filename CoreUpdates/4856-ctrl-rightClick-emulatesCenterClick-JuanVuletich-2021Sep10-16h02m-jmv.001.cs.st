'From Cuis 5.0 [latest update: #4855] on 10 September 2021 at 4:10:30 pm'!

!MouseEvent methodsFor: 'button state' stamp: 'jmv 9/10/2021 16:02:58'!
turnMouseButton2Into3
	"Answer true if modifier keys are such that button 2 should be considered as button 3.
	ctrl - click right -> center click
	"

	self controlKeyPressed ifTrue: [ ^ true ].
	^ false! !


!MouseEvent methodsFor: 'button state' stamp: 'jmv 9/10/2021 16:08:01'!
mouseButton2Pressed
	"Answer true if the mouseButton2 is being pressed.
	Reported by the VM for right mouse button or option+click on the Mac, ctrl-click on Windows, or ctrl-click or alt-click on Linux.
	It is also emulated here with ctrl-click on any platform."

	(self turnMouseButton1Into2 and: [ buttons anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	self turnMouseButton2Into3 ifTrue: [ ^ false ].
	^ buttons anyMask: InputSensor mouseButton2! !

!MouseEvent methodsFor: 'button state' stamp: 'jmv 9/10/2021 16:06:43'!
mouseButton3Pressed
	"Answer true if the mouseButton3 is being pressed.
	Reported by the VM for center (wheel) mouse button or cmd+click on the Mac or win/meta+click on Windows and Linux.
	It is also emulated here with on any platform with:	
		shift - ctrl - click
		ctrl - rightClick"

	(self turnMouseButton1Into3 and: [ buttons anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	(self turnMouseButton2Into3 and: [ buttons anyMask: InputSensor mouseButton2 ])
		ifTrue: [ ^ true ].
	^ buttons anyMask: InputSensor mouseButton3! !


!MouseButtonEvent methodsFor: 'accessing' stamp: 'jmv 9/10/2021 16:09:10'!
mouseButton2Changed
	"Answer true if the mouseButton2 has changed.
	Reported by the VM for right mouse button or option+click on the Mac.
	It is also emulated here with ctrl-click on any platform.
	The check for button change (instead of button press) is specially useful on buttonUp events."

	(self turnMouseButton1Into2 and: [ whichButton anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	self turnMouseButton2Into3 ifTrue: [ ^ false ].
	^ whichButton anyMask: InputSensor mouseButton2! !

!MouseButtonEvent methodsFor: 'accessing' stamp: 'jmv 9/10/2021 16:07:19'!
mouseButton3Changed
	"Answer true if the mouseButton3 has changed.
	Reported by the VM for center (wheel) mouse button or cmd+click on the Mac or meta+click on Linux.
	It is also emulated here with shift-ctrl-click or ctrl-rightClick on any platform.
	The check for button change (instead of button press) is specially useful on buttonUp events."

	(self turnMouseButton1Into3 and: [ whichButton anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	(self turnMouseButton2Into3 and: [ whichButton anyMask: InputSensor mouseButton2 ])
		ifTrue: [ ^ true ].
	^ whichButton anyMask: InputSensor mouseButton3! !

