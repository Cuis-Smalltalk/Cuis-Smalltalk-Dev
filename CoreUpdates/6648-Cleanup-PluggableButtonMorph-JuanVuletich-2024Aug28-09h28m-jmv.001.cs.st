'From Cuis7.1 [latest update: #6646] on 28 August 2024 at 9:33:47 am'!

!PluggableButtonMorph methodsFor: 'enabling' stamp: 'jmv 8/28/2024 09:30:18'!
disable

	enableSelector := false.
	self redrawNeeded.! !

!PluggableButtonMorph methodsFor: 'enabling' stamp: 'jmv 8/28/2024 09:30:08'!
enable

	enableSelector := true.
	self redrawNeeded.! !

