'From Cuis 4.2 of 25 July 2013 [latest update: #2343] on 28 May 2015 at 10:07:36.928406 am'!

!String methodsFor: 'converting' stamp: 'jmv 5/28/2015 10:03'!
adaptToNumber: rcvr andSend: selector
	"If I am involved in arithmetic with a number, convert me to a number."

"	^ rcvr perform: selector with: self asNumber"
	self shouldNotImplement.! !

!String methodsFor: 'converting' stamp: 'jmv 5/28/2015 10:03'!
adaptToPoint: rcvr andSend: selector
	"If I am involved in arithmetic with a point, convert me to a number."

"	^ rcvr perform: selector with: self asNumber"
	self shouldNotImplement.! !

