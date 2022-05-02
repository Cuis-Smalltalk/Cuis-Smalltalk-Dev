'From Cuis 5.0 [latest update: #4984] on 28 November 2021 at 2:19:00 pm'!

!PluggableButtonMorph methodsFor: 'enabling' stamp: 'KenD 11/28/2021 14:18:01'!
disable

	enableSelector := false.
	self update: #enableSelector.
	self redrawNeeded ! !

!PluggableButtonMorph methodsFor: 'enabling' stamp: 'KenD 11/28/2021 14:18:11'!
enable

	enableSelector := true.
	self update: #enableSelector.
	self redrawNeeded ! !

