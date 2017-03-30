'From Cuis 5.0 of 7 November 2016 [latest update: #3072] on 30 March 2017 at 8:57:52 am'!

!TaskbarMorph methodsFor: 'services' stamp: 'jmv 3/30/2017 08:54:49'!
aboutToCollapse: aMorph
	"Add a button for aMorph if not already there (see #taskbarIncludesAllWindows)"

	(self buttonFor: aMorph) ifNil: [
		self addButtonFor: aMorph ]! !


!Morph methodsFor: 'user interface' stamp: 'jmv 3/30/2017 08:55:31'!
collapse
	"If taskbar not visible, just hide."

	self taskbar
		ifNotNil: [ :tb | tb aboutToCollapse: self ].
	self hide! !

!methodRemoval: TaskbarMorph #wasCollapsed:!
TaskbarMorph removeSelector: #wasCollapsed:!
