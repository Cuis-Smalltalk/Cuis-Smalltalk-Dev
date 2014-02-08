'From Cuis 4.2 of 25 July 2013 [latest update: #1960] on 7 February 2014 at 9:58:14.088785 am'!

!Morph methodsFor: 'stepping' stamp: 'jmv 2/7/2014 09:58'!
startStepping: aSelector
	"Start getting sent the requested message at each Morphic step.
	See sample code snippet at Morph>>stepAt:"

	self startStepping: aSelector stepTime: nil! !

!Morph methodsFor: 'stepping' stamp: 'jmv 2/7/2014 09:56'!
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

	self triggerEvent: #morphicStep with: millisecondClockValue! !

