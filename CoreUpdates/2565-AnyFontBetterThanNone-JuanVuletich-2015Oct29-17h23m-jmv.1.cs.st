'From Cuis 4.2 of 25 July 2013 [latest update: #2563] on 29 October 2015 at 5:24:03.654581 pm'!

!Preferences class methodsFor: 'fonts' stamp: 'jmv 10/29/2015 17:23'!
setDefaultFonts: defaultFontsSpec
		
	| font |
	defaultFontsSpec do: [ :triplet |
		font _ AbstractFont familyName: triplet second pointSize: triplet third.
		font ifNil: [ font _ AbstractFont default ]. 
		triplet size > 3 ifTrue: [
			font _ font emphasized: triplet fourth ].
		self
			perform: triplet first
			with: font]! !

