'From Cuis 5.0 of 7 November 2016 [latest update: #3190] on 20 October 2017 at 9:37:21 pm'!
'Description A blinking avatar with eyes following the mouse cursor.'!
!provides: 'Avatar' 1 2!
!classDefinition: #AvatarMorph category: #Avatar!
EllipseMorph subclass: #AvatarMorph
	instanceVariableNames: 'blink'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Avatar'!
!classDefinition: 'AvatarMorph class' category: #Avatar!
AvatarMorph class
	instanceVariableNames: ''!


!AvatarMorph commentStamp: '<historical>' prior: 0!
AvatarMorph new openInWorld .!

!AvatarMorph methodsFor: 'visual properties' stamp: 'HilaireFernandes 10/20/2017 11:10:03'!
defaultColor
	^ Color pink paler paler! !

!AvatarMorph methodsFor: 'drawing' stamp: 'HilaireFernandes 10/20/2017 21:32:20'!
drawIrisOn: canvas at: irisCenter
	| center radius shiftedDirection |
	radius _ (extent x min: extent y) // 12.
	shiftedDirection _ (self activeHand morphPosition - (self externalizeToWorld: irisCenter)) normalized * 5.
	center _ irisCenter + shiftedDirection.
	canvas 
		ellipseCenterX: center x
		y: center y
		rx: radius
		ry:  radius
		borderWidth:  1 
		borderColor: Color black
		fillColor: Color blue! !

!AvatarMorph methodsFor: 'drawing' stamp: 'HilaireFernandes 10/20/2017 20:50:11'!
drawLeftEyeOn: canvas
	canvas 
		ellipseCenterX: extent x // 4 
		y: extent y // 4 
		rx: extent x // 6
		ry:  extent y // 6 
		borderWidth:  3 
		borderColor: Color black
		fillColor: Color white.
	self drawIrisOn: canvas at: (extent x // 4) @ (extent y// 4)! !

!AvatarMorph methodsFor: 'drawing' stamp: 'HilaireFernandes 10/20/2017 21:11:57'!
drawOn: canvas
	super drawOn: canvas.
	self drawLeftEyeOn: canvas.
	self drawRightEyeOn: canvas.
	"Mouth, no much option to draw it"
	canvas line: (extent x // 4) @ (extent y * 3 // 4) to: (extent x *3 // 4) @ (extent y * 3 // 4) width: 3 color: Color brown! !

!AvatarMorph methodsFor: 'drawing' stamp: 'HilaireFernandes 10/20/2017 20:51:08'!
drawRightEyeOn: canvas
	blink
	ifFalse: [ 
		canvas 
			ellipseCenterX: extent x * 3 // 4 
			y: extent y // 4 
			rx: extent x // 6
			ry:  extent y // 6 
			borderWidth:  3 
			borderColor: Color black
			fillColor: Color white.
		self drawIrisOn: canvas at: (extent x * 3 // 4) @ (extent y // 4)]
	ifTrue: [
		canvas 
			line: (extent x * 7 // 12) @ (extent y // 4) to: (extent x *11 // 12) @ (extent y // 4) width: 3 color: Color black]
! !

!AvatarMorph methodsFor: 'initialization' stamp: 'HilaireFernandes 10/20/2017 17:14:11'!
initialize
	super initialize.
	extent _ 70@90.
	blink _ false! !

!AvatarMorph methodsFor: 'stepping' stamp: 'HilaireFernandes 10/20/2017 21:34:00'!
step
	blink _ 10 atRandom > 7 .
	self redrawNeeded! !

!AvatarMorph methodsFor: 'stepping' stamp: 'HilaireFernandes 10/20/2017 21:13:30'!
stepTime
	^ 500! !

!AvatarMorph methodsFor: 'stepping' stamp: 'HilaireFernandes 10/20/2017 17:11:03'!
wantsSteps
	^ true! !
