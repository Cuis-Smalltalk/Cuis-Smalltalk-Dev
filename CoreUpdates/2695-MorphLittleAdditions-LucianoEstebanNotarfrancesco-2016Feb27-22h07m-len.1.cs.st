'From Cuis 4.2 of 25 July 2013 [latest update: #2690] on 27 February 2016 at 10:07:49.916732 pm'!

!Morph methodsFor: 'geometry' stamp: 'len 2/17/2016 21:55'!
morphAlign: aPoint with: anotherPoint
	^ self morphPosition: self morphPosition + anotherPoint - aPoint! !

!Morph methodsFor: 'geometry' stamp: 'len 2/17/2016 21:51'!
morphBounds
	^ self morphPosition extent: self morphExtent! !

!Morph methodsFor: 'geometry' stamp: 'len 2/17/2016 22:04'!
morphBounds: aRectangle
	self morphPosition: aRectangle topLeft.
	self morphExtent: aRectangle extent! !

!Morph methodsFor: 'stepping' stamp: 'len 2/26/2016 03:02'!
step
	^ self! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'len 2/24/2016 21:17'!
findA: aClass
	^ self findDeepSubmorphThat: [:each| each isKindOf: aClass] ifAbsent: [self error: 'not found']! !


!Morph methodsFor: 'stepping' stamp: 'len 2/26/2016 03:03'!
stepAt: millisecondClockValue
	"Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. The time between steps is specified by this morph's answer to the stepTime message.
	The millisecondClockValue parameter gives the value of the millisecond clock at the moment of dispatch."
	"
	m _ RectangleLikeMorph new.
	m color: Color random.
	m openInWorld.
	start _ Time localMillisecondClock.
	last _ start.
	m when: #morphicStep evaluate: [ :v |  | delta |
		delta _ v - last.
		absolute _ v - start.
		absolute < 30000 ifTrue: [
			(m owner is: #HandMorph) ifFalse: [
				m morphPosition: 3@2 * absolute // 100 ]]
		ifFalse: [ m stopStepping ].
		self disableCode: [
			{m. v. last. delta } print.].
		last _ v.
		 ].
	m startSteppingStepTime: 20.
	"
	self step.
	self triggerEvent: #morphicStep with: millisecondClockValue! !

