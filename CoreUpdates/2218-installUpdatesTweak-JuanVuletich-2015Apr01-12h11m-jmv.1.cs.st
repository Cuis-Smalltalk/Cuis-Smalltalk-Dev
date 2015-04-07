'From Cuis 4.2 of 25 July 2013 [latest update: #2217] on 1 April 2015 at 12:11:39.647158 pm'!

!ChangeSet class methodsFor: 'services' stamp: 'jmv 4/1/2015 11:09'!
installNewUpdates
 	
	| imagePath oneLevelUp pathToCoreUpdates updatesFileDirectory |

	imagePath := Smalltalk imagePath.
	pathToCoreUpdates := imagePath, FileDirectory slash, 'CoreUpdates'.
	updatesFileDirectory := FileDirectory on: pathToCoreUpdates.

	updatesFileDirectory exists ifFalse: [
		oneLevelUp := imagePath copyUpToLast: FileDirectory slash asCharacter.
		pathToCoreUpdates := oneLevelUp, FileDirectory slash, 'Cuis-Smalltalk-Dev', FileDirectory slash, 'CoreUpdates'.
		updatesFileDirectory := FileDirectory on: pathToCoreUpdates ]. 

	updatesFileDirectory exists
		ifFalse: [ self inform: 'Could not find a CoreUpdates folder\No updates loaded' withNewLines ] 
		ifTrue: [ ChangeSet installNewUpdates: updatesFileDirectory ]! !

