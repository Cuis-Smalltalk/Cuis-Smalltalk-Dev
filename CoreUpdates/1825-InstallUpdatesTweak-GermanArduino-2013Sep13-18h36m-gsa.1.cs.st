'From Cuis 4.2 of 25 July 2013 [latest update: #1823] on 13 September 2013 at 6:44:15.588444 pm'!

!ChangeSet class methodsFor: 'services' stamp: 'gsa 9/13/2013 18:43'!
installNewUpdates: updatesFileDirectory
	(self newUpdates: updatesFileDirectory) do: [:each |
		self install: (updatesFileDirectory fullNameFor: each)].
	Smalltalk aboutThisSystem.! !

