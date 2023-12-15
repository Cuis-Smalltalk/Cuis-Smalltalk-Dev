'From Cuis6.0 [latest update: #6122] on 15 December 2023 at 4:47:28 pm'!

!ChangeSet class methodsFor: 'services' stamp: 'jmv 12/15/2023 15:21:41'!
installNewUpdates
 	
	| updatesFileDirectory |
	updatesFileDirectory := DirectoryEntry coreUpdatesDirectory.

	updatesFileDirectory exists ifFalse: [
		^self inform: 'Could not find a CoreUpdates folder\No updates loaded' withNewLines ].
		
	ChangeSet installNewUpdates: updatesFileDirectory.! !

