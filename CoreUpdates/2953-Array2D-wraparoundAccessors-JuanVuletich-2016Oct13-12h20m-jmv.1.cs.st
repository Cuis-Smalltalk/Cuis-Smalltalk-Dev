'From Cuis 4.2 of 25 July 2013 [latest update: #2950] on 14 October 2016 at 12:55:26 pm'!

!Array2D methodsFor: 'accessing' stamp: 'jmv 10/14/2016 12:33:31'!
wrapI: i j: j

	^ self i: i-1\\height+1 j: j-1\\width+1! !

!Array2D methodsFor: 'accessing' stamp: 'jmv 10/14/2016 12:33:50'!
wrapI: i j: j put: anObject

	^ self i: i-1\\height+1 j: j-1\\width+1 put: anObject! !

