'From Cuis7.5 [latest update: #7285] on 26 June 2025 at 5:47:45 pm'!

!InputSensor methodsFor: 'joystick' stamp: 'jmv 6/26/2025 17:47:26'!
testJoystick: index
	"Sensor testJoystick: 3"

	| f pt buttons status |
	f := Form extent: 110@50 depth: 32.
	[Sensor isAnyButtonPressed] whileFalse: [
		pt := Sensor joystickXY: index.
		buttons := Sensor joystickButtons: index.
		status :=
'xy: ', pt printString, '
buttons: ', buttons hex.
		f fillWhite.
		status displayOn: f at: 10@10.
		f displayOn: Display at: 10@10.
	].
! !

