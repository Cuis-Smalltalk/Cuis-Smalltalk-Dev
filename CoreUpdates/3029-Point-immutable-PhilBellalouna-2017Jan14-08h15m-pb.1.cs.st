'From Cuis 5.0 of 7 November 2016 [latest update: #3028] on 14 January 2017 at 8:18:04 am'!

!Point commentStamp: 'jmv 12/30/2016 17:39:06' prior: 0!
I represent an x-y pair of numbers usually designating a location on the screen.

When dealing with display coordinates, the y axis is usually considered to increase downwards. However, the standard math convention is to consider it increasing upwards. 
Points don't need to know about this. In the first case, theta increases clockwise. In the second case, it increases counter-clockwise, also the standard math convention.

Any method that doesn't follow this (because it assumes one specific convention) include this fact in the selector and in a comment.

My instances are immutable. See #privateSetX:setY:!


!Point methodsFor: 'private' stamp: 'jmv 12/11/2016 10:28:44'!
privateSetX: xValue setY: yValue
	"Points are immutable. Right now this is by convention, but we'll make this enfoced by VM.
	Do not all this method, except from instance creation."
	x _ xValue.
	y _ yValue! !

!Point methodsFor: 'copying' stamp: 'pb 10/29/2016 18:18:07'!
shallowCopy
	"Immutable"
	^ self.! !


!Object class methodsFor: 'instance creation' stamp: 'jmv 12/30/2016 17:33:31'!
unStream: aByteArray
	^ ReferenceStream unStream: aByteArray! !


!DataStream class methodsFor: 'as yet unclassified' stamp: 'jmv 12/30/2016 17:33:27'!
unStream: aByteArray

	^(self on: ((RWBinaryOrTextStream with: aByteArray) reset; binary)) next! !


!Point class methodsFor: 'instance creation' stamp: 'pb 10/29/2016 17:14:00'!
r: rho degrees: degrees
	"Answer an instance of me with polar coordinates rho and theta."
	^ self
		rho: rho
		theta: degrees asFloat degreesToRadians.! !

!Point class methodsFor: 'instance creation' stamp: 'pb 10/29/2016 17:12:53'!
rho: rho theta: radians
	"Answer an instance of me with polar coordinates rho and theta."
	^ self
		x: rho asFloat * radians cos
		y: rho asFloat * radians sin.! !

!Point class methodsFor: 'instance creation' stamp: 'jmv 12/11/2016 10:28:50'!
x: anX y: anY
	"Answer an instance of me with supplied coordinates."

	^self new privateSetX: anX setY: anY! !

!methodRemoval: Point #setR:degrees:!
Point removeSelector: #setR:degrees:!
!methodRemoval: Point #setRho:theta:!
Point removeSelector: #setRho:theta:!
!methodRemoval: Point #setX:setY:!
Point removeSelector: #setX:setY:!
