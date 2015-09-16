'From Cuis 4.2 of 25 July 2013 [latest update: #2491] on 15 September 2015 at 10:17:06.36744 am'!

!TheWorldMenu methodsFor: 'menu' stamp: 'KenD 9/12/2015 19:51'!
openTerseGuide
	"In TerseGuide.pck"
	Smalltalk at: #TerseGuideWindow ifAbsent: [ 
		(PopUpMenu 
		confirm: 'The Terse Guide is not loaded', String newLineString, 'Would you like me to load it for you now?')
		ifTrue: [Feature require: #'TerseGuide'] 
	].
	Smalltalk at: #TerseGuideWindow ifPresent: [ :cls | ^cls openTerseGuide ].
	self inform: 'Please install optional package TerseGuide.pck.st', String newLineString, 'Feature require: #''TerseGuide'' '! !

