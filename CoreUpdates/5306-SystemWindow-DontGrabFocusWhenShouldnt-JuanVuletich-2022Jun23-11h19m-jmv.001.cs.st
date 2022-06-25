'From Cuis 6.0 [latest update: #5305] on 23 June 2022 at 11:21:01 am'!

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 6/23/2022 09:50:02'!
visible: aBoolean
	self visible == aBoolean
		ifTrue: [ ^ self ].
	super visible: aBoolean.
	aBoolean ifTrue: [
		self activate ]! !

