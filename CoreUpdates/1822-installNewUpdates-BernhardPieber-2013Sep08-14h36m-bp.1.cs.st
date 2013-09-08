'From Cuis 4.2 of 25 July 2013 [latest update: #1821] on 8 September 2013 at 2:47:46.369369 pm'!

!ChangeSet class methodsFor: 'services' stamp: 'bp 9/8/2013 14:41'!
installNewUpdates: updatesFileDirectory
	(self newUpdates: updatesFileDirectory) do: [:each |
		self install: (updatesFileDirectory fullNameFor: each)]! !

!ChangeSet class methodsFor: 'services' stamp: 'bp 9/8/2013 14:38'!
newUpdates: updatesFileDirectory
	^(updatesFileDirectory fileNames select: [:each | each asNumber > SystemVersion current highestUpdate]) asSortedCollection! !

