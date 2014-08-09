'From Cuis 4.2 of 25 July 2013 [latest update: #2065] on 6 August 2014 at 9:48:05.707 am'!

!TextModelMorph methodsFor: 'initialization' stamp: 'jmv 8/6/2014 09:26'!
defaultColor
	"answer the default color/fill style for the receiver"
	^ Theme current textPane! !


!FillInTheBlankMorph methodsFor: 'drawing' stamp: 'jmv 8/6/2014 09:47'!
drawOn: aCanvas

	Theme current roundWindowCorners
		ifTrue: [
			aCanvas
				roundRect: (0@0 extent: extent)
				color: color
				radius: Theme current roundedWindowRadius ]
		ifFalse: [ super drawOn: aCanvas ]! !

