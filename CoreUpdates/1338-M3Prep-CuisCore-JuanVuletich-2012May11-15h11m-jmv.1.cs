'From Cuis 4.0 of 21 April 2012 [latest update: #4219] on 11 May 2012 at 3:17:29 pm'!

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 4/27/2012 16:38'!
bringWindowsFullOnscreen
	"Make ever SystemWindow on the desktop be totally on-screen, whenever possible."
	
	(SystemWindow windowsIn: self satisfying: [:w | true]) do: [ :each |
		each makeMeFullyVisible ]! !
