'From Cuis 4.2 of 25 July 2013 [latest update: #1867] on 8 December 2013 at 3:13:20.059611 pm'!

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 12/8/2013 15:13'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."
	"
	This is starting to work:
		| c |
		c _ Display getCanvas into: self runningWorld.
		self runningWorld submorphsDo: [ :m | c fullDraw: m ].
	"

	"To replace #fullDrawOn:"
	self flag: #jmvVer3.

	aMorph visible ifFalse: [^ self].

	self into: aMorph.

	aMorph layoutSubmorphsIfNeeded.
	(self isVisible: aMorph morphFullBoundsInWorld) ifFalse: [
		self outOf: aMorph.
		^ self].

	aMorph isKnownFailing 
		ifTrue: [ aMorph drawErrorOn: self ]
		ifFalse: [ aMorph fullDrawOn: self ].

	self outOf: aMorph! !


!PasteUpMorph methodsFor: 'misc' stamp: 'jmv 12/8/2013 15:11'!
backgroundImageData: aByteArray
	"
	| filename |
	filename _ 'bg/free-3d-art-pictures-gallery-wallpaper-desktop-18.jpg'.
	filename _ 'bg/free-desktop-wallpaper.jpg'.
	filename _ 'bg/jellyfish-thumb.jpg'.
	filename _ 'bg/splash_by_beefpepsi.jpg'.
	filename _ 'bg/gray ocean and pier.jpg'.
	filename _ 'bg/newyork.jpg'.
	filename _ 'bg/download-free-desktop-wallpaper-nature-conquestofparadise-marirs-pic.jpg'.
	filename _ 'bg/desktop-wallpaper-tropical-1280x1024.jpg'.

	filename _ 'bg/free-3d-art-pictures-gallery-wallpaper-desktop-18.jpg'.
	self runningWorld backgroundImageData: (FileStream readOnlyFileNamed: filename) binary contentsOfEntireFile.
	"
	backgroundImageData _ aByteArray.
	self buildMagnifiedBackgroundImage! !

