'From Cuis 5.0 of 7 November 2016 [latest update: #3607] on 16 February 2019 at 7:46:40 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 2/7/2019 20:44:52'!
visible: aBoolean
	"set the 'visible' attribute of the receiver to aBoolean"

	self visible == aBoolean
		ifTrue: [ ^ self ].
	aBoolean ifFalse: [
		self redrawNeeded ].
	self setProperty: #visible toValue: aBoolean.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	aBoolean ifTrue: [
		self redrawNeeded]! !


!TaskbarMorph methodsFor: 'services' stamp: 'jmv 2/7/2019 20:45:05'!
wasOpened: aMorph
	"aMorph was added to the world. Add button for aMorph if appropriate (see #taskbarIncludesAllWindows)"

	self addButtonFor: aMorph! !

!TaskbarMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 2/7/2019 20:44:10'!
noteNewOwner: aMorph
	"I have just been added as a submorph of aMorph"
	super noteNewOwner: aMorph.
	aMorph submorphsDo: [ :m |
		self addButtonFor: m ].
	self notifyDisplayResize! !

!methodRemoval: TaskbarMorph #wasMadeVisible:!
TaskbarMorph removeSelector: #wasMadeVisible:!
!methodRemoval: Preferences class #taskbarIncludesAllWindows!
Preferences class removeSelector: #taskbarIncludesAllWindows!
