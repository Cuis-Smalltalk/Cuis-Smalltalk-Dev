'From Cuis 5.0 of 7 November 2016 [latest update: #3384] on 30 July 2018 at 9:40:43 am'!

!Number methodsFor: 'testing' stamp: 'jmv 7/30/2018 08:51:35'!
ifNotZero: aBlock
	"
	Useful for workarounding division by zero
	#(1.0 2.0 0.0) collect: [ :k | k ifNotZero: [100.0/k]]
	"
	^ self isZero ifFalse: aBlock ifTrue: [self]! !


!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 7/30/2018 08:53:30'!
scrollBy: delta
	"Move the contents in the direction delta."

	| newYoffset yRange xRange newXoffset |
	
	"Set the offset on the scroller"
	yRange _ self vLeftoverScrollRange.
	xRange _ self hLeftoverScrollRange.
	newYoffset _ self scrollerOffset y - delta y min: yRange max: 0.
	newXoffset _ self scrollerOffset x - delta x min: xRange max: 0.
	
	self scrollerOffset: newXoffset@newYoffset.

	"Update the scrollBars"
	scrollBar scrollValue: (yRange ifNotZero: [newYoffset asFloat / yRange]).
	hScrollBar scrollValue: (xRange ifNotZero: [newXoffset asFloat / xRange])! !

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 7/30/2018 08:58:19'!
scrollToShow: aRectangle
	"scroll to include as much of aRectangle as possible, where aRectangle is in the scroller's local space.
	This means that 0@0 is scrolling all the way top and all the way left"
	| deltaY |
	(aRectangle top >= (0.1*self viewableHeight) and: [
		aRectangle bottom <= (0.9*self viewableHeight) ])
		ifTrue: [
			"already visible"
			^self ].

	"Scroll center of selection into view if necessary"
	deltaY _ self viewableExtent y * 0.1 - aRectangle top.
	deltaY ~= 0 ifTrue: [
		self scrollBy: 0@deltaY ]! !

