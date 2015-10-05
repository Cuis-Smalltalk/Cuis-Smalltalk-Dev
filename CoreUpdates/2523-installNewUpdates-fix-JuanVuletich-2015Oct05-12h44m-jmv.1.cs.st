'From Cuis 4.2 of 25 July 2013 [latest update: #2522] on 5 October 2015 at 12:44:33.758036 pm'!

!ChangeSet class methodsFor: 'services' stamp: 'jmv 10/5/2015 12:44'!
installNewUpdates
 	
	| updatesFileDirectory |

	updatesFileDirectory _ (FmDirectoryEntry smalltalkImageDirectory / 'CoreUpdates') asDirectoryEntry.

	updatesFileDirectory exists
		ifFalse: [ self inform: 'Could not find a CoreUpdates folder\No updates loaded' withNewLines ] 
		ifTrue: [ ChangeSet installNewUpdates: updatesFileDirectory ].! !

