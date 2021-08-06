'From Cuis 5.0 [latest update: #4733] on 4 August 2021 at 7:02:29 pm'!

!KernelMorph methodsFor: 'geometry' stamp: 'jmv 8/4/2021 18:35:22'!
morphExtent: newExtent
	"In our own coordinates!!"

	| oldExtent |
	extent = newExtent ifFalse: [
		oldExtent _ extent.
		(self privateExtent: newExtent) ifTrue: [
			self extentChanged: oldExtent.
			owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
			self redrawNeeded ]]! !

!KernelMorph methodsFor: 'geometry' stamp: 'jmv 8/4/2021 18:36:04'!
morphHeight: aNumber
	"In our own coordinates!!"

	self morphExtent: extent x@aNumber! !

!KernelMorph methodsFor: 'geometry' stamp: 'jmv 8/4/2021 18:35:59'!
morphWidth: aNumber
	"In our own coordinates!!"

	self morphExtent: aNumber@extent y! !


!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 8/4/2021 18:35:27'!
morphExtent: newExtent
	"In our own coordinates!!"

	| oldExtent |
	extent = newExtent ifFalse: [
		oldExtent _ extent.
		(self privateExtent: newExtent) ifTrue: [
			self extentChanged: oldExtent.
			owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
			self redrawNeeded ]]! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 8/4/2021 18:36:07'!
morphHeight: aNumber
	"In our own coordinates!!"

	self morphExtent: extent x@aNumber! !

!WidgetMorph methodsFor: 'geometry' stamp: 'jmv 8/4/2021 18:36:01'!
morphWidth: aNumber
	"In our own coordinates!!"

	self morphExtent: aNumber@extent y! !

