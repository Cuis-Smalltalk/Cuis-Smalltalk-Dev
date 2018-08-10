'From Cuis 5.0 of 7 November 2016 [latest update: #3190] on 5 November 2017 at 4:46:23 pm'!
'Description Please enter a description for this package'!
!provides: 'Wheel' 1 1!
!classDefinition: #WheelMorph category: #Wheel!
BorderedRectMorph subclass: #WheelMorph
	instanceVariableNames: 'target actionSelector factor circular old angle maxAngle lastRedraw'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Wheel'!
!classDefinition: 'WheelMorph class' category: #Wheel!
WheelMorph class
	instanceVariableNames: ''!


!WheelMorph commentStamp: 'HilaireFernandes 11/5/2017 16:42:57' prior: 0!
A thumb wheel widget control:

| w |
w := WheelMorph new.
w color: Color red.
w target: Transcript; actionSelector: #print:.
w openInWorld.
!

!WheelMorph methodsFor: 'accessing'!
actionSelector
	^ actionSelector
! !

!WheelMorph methodsFor: 'accessing' stamp: 'HilaireFernandes 11/5/2017 12:27:56'!
actionSelector: aSymbol
	actionSelector _ aSymbol.

! !

!WheelMorph methodsFor: 'private' stamp: 'HilaireFernandes 11/5/2017 12:15:57'!
addAngle: aNumber
	| new |
	new _ self angle + aNumber.
	self isCircular ifTrue: [new _ new \\ self maxAngle].
	self angle: new.
	lastRedraw _ lastRedraw + aNumber.
	(lastRedraw abs > 2) ifTrue: [
		lastRedraw _ 0.
		self changed.
		self redrawNeeded]! !

!WheelMorph methodsFor: 'accessing'!
angle
	^angle! !

!WheelMorph methodsFor: 'accessing' stamp: 'HilaireFernandes 11/5/2017 11:09:00'!
angle: aNumber
	angle _ aNumber! !

!WheelMorph methodsFor: 'properties' stamp: 'HilaireFernandes 11/5/2017 11:10:42'!
beCircular
	circular _ true! !

!WheelMorph methodsFor: 'properties' stamp: 'HilaireFernandes 11/5/2017 11:13:23'!
beHorizontal
	self isHorizontal
		ifFalse: [self morphExtent: extent transposed] ! !

!WheelMorph methodsFor: 'properties' stamp: 'HilaireFernandes 11/5/2017 11:11:02'!
beLinear
	circular _ false! !

!WheelMorph methodsFor: 'properties' stamp: 'HilaireFernandes 11/5/2017 11:11:14'!
beVertical
	self isHorizontal
		ifTrue: [extent  _ extent transposed] ! !

!WheelMorph methodsFor: 'accessing' stamp: 'HilaireFernandes 11/5/2017 16:42:45'!
color: aColor
	super color: aColor.
	self borderColor: aColor muchDarker! !

!WheelMorph methodsFor: 'event handling' stamp: 'HilaireFernandes 11/5/2017 11:14:14'!
doTargetAction: cursorPoint
	| new delta |
	self isHorizontal
		ifTrue: [new _ cursorPoint x]
		ifFalse: [new _ cursorPoint y].
	delta _ new - old.
	delta isZero ifTrue: [^self].
	self addAngle: delta.
	(target ~~ nil and: [actionSelector ~~ nil]) ifTrue: [
		Cursor normal showWhile: [
			target perform: actionSelector withArguments: (Array with: (delta * self factor))]].
	old _ new.
! !

!WheelMorph methodsFor: 'drawing' stamp: 'HilaireFernandes 11/5/2017 16:44:01'!
drawOn: aCanvas
	| interval delta frame x0 x1 y0 y1 deltaX deltaY  |
	super drawOn: aCanvas.		"border & background"
	frame := self morphLocalBounds insetBy: 2.
	aCanvas frameRectangle: frame color: color borderWidth: 1 borderStyleSymbol: #raised.
	frame := frame insetBy: 1.
	x0 := frame origin x.
	x1 := frame corner x.
	y0 := frame origin y.
	y1 := frame corner y.
	deltaX := (x1 - x0) / 2.
	deltaY := (y1 - y0) / 2.
	interval := 10.
	delta := self angle \\ interval.
	1 to: (self maxAngle / (2 * interval)) rounded do: [ :i |
		| x y |
		self isHorizontal ifTrue: [
			x := x0 + deltaX - (((i * interval + delta) * (2 * Float pi / 360.0)) cos * deltaX).
			aCanvas fillRectangle: (Rectangle origin: (x-1)@y0 corner: x@y1) color: borderColor]
		ifFalse: [
			y := y0 + deltaY - (((i * interval + delta) * (2 * Float pi / 360.0)) cos * deltaY).
			aCanvas fillRectangle: (Rectangle origin: x0@(y-1) corner: x1@y) color: borderColor]
	]! !

!WheelMorph methodsFor: 'accessing'!
factor
	^factor! !

!WheelMorph methodsFor: 'accessing' stamp: 'HilaireFernandes 11/5/2017 11:09:05'!
factor: aFloat
	factor _ aFloat! !

!WheelMorph methodsFor: 'event handling'!
handlesMouseDown: evt
	^true
! !

!WheelMorph methodsFor: 'initialization' stamp: 'HilaireFernandes 11/5/2017 12:26:42'!
initialize
	super initialize.
	self angle: 0.
	self maxAngle: 360.
	self factor: 1.0.
	self morphExtent: 100@23.
	self beCircular.
	self color: Color lightGray.
	lastRedraw := 0.! !

!WheelMorph methodsFor: 'properties'!
isCircular
	^circular! !

!WheelMorph methodsFor: 'properties' stamp: 'HilaireFernandes 11/5/2017 11:33:06'!
isHorizontal
	^ self morphBounds isWide! !

!WheelMorph methodsFor: 'properties'!
isLinear
	^circular not! !

!WheelMorph methodsFor: 'properties'!
isVertical
	^self isHorizontal not! !

!WheelMorph methodsFor: 'accessing'!
maxAngle
	^maxAngle! !

!WheelMorph methodsFor: 'accessing' stamp: 'HilaireFernandes 11/5/2017 11:09:10'!
maxAngle: aNumber
	maxAngle _ aNumber! !

!WheelMorph methodsFor: 'event handling' stamp: 'HilaireFernandes 11/5/2017 12:12:54'!
mouseButton1Down: evt localPosition: position
	self isHorizontal
		ifTrue: [old _ position x]
		ifFalse: [old _ position y].! !

!WheelMorph methodsFor: 'event handling' stamp: 'HilaireFernandes 11/5/2017 12:12:40'!
mouseMove: evt localPosition: position
	(evt anyButtonPressed)
		ifTrue: [self doTargetAction: position]! !

!WheelMorph methodsFor: 'accessing'!
target
	^target! !

!WheelMorph methodsFor: 'accessing' stamp: 'HilaireFernandes 11/5/2017 11:09:16'!
target: aTargetObject
	target _ aTargetObject! !
