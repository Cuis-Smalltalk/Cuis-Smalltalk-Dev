'From Cuis 4.0 of 21 April 2012 [latest update: #1461] on 26 September 2012 at 5:59:58 pm'!

!Morph methodsFor: 'change reporting' stamp: 'jmv 9/26/2012 17:59'!
invalidateRect: aRectangle

	| rectInOwner rectInOwns |

	"warning. Senders are using global coordinates. Redesign!!"
	"local now!!!!!!!!!!"
	self flag: #jmvVer2.	"ok?"
	
 	rectInOwns _ self clipsSubmorphs
		ifTrue: [ aRectangle intersect: (0@0 extent: self morphExtent) ]
		ifFalse: [ aRectangle ].
	owner ifNotNil: [
		rectInOwner _ location displayBoundsOfTransformOf: rectInOwns.
		owner invalidateRect: rectInOwner ]! !

