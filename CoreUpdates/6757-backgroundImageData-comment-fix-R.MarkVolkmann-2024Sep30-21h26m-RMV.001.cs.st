'From Cuis7.1 [latest update: #6756] on 30 September 2024 at 9:30:17 pm'!

!PasteUpMorph methodsFor: 'misc' stamp: 'RMV 9/30/2024 21:29:56'!
backgroundImageData: aByteArray
	"
	| filename |
	filename := 'bg/free-3d-art-pictures-gallery-wallpaper-desktop-18.jpg'.
	filename := 'bg/free-desktop-wallpaper.jpg'.
	filename := 'bg/jellyfish-thumb.jpg'.
	filename := 'bg/splash_by_beefpepsi.jpg'.
	filename := 'bg/gray ocean and pier.jpg'.
	filename := 'bg/newyork.jpg'.
	filename := 'bg/download-free-desktop-wallpaper-nature-conquestofparadise-marirs-pic.jpg'.
	filename := 'bg/desktop-wallpaper-tropical-1280x1024.jpg'.

	self runningWorld backgroundImageData: (filename asFileEntry readStream) binary contentsOfEntireFile.
	"
	backgroundImageData := aByteArray.
	self buildMagnifiedBackgroundImage! !

