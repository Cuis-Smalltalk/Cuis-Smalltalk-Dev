'From Cuis7.5 [latest update: #7685] on 4 November 2025 at 10:18:46 am'!

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 11/3/2025 17:45:47'!
makeMeFullyVisible 

	self world morphExtent > `0@0` ifFalse: [^ self].

	(self morphPosition >= `0@0` and: [ self morphPosition < (self world morphExtent-extent)]) ifTrue: [
		^ self "OK -- visible"].
"
	self morphPosition: (RealEstateAgent initialFrameFor: self initialExtent: self extentInWorld world: self world) topLeft
"
	self fitInOwner
! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 11/3/2025 17:45:55'!
makeMeVisible 

	self world morphExtent > `0@0` ifFalse: [^ self].

	(self morphPosition >= `0@0` and: [ self morphPosition < (self world morphExtent-self labelHeight)]) ifTrue: [
		^ self "OK -- at least my top left is visible"].

	"window not on screen (probably due to reframe) -- move it now"
"
	self morphPosition: (RealEstateAgent initialFrameFor: self initialExtent: extent world: self world) topLeft
"
	self fitInOwner! !

