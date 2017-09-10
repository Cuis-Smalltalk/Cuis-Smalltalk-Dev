'From Cuis 5.0 of 7 November 2016 [latest update: #3161] on 6 September 2017 at 8:02:36 pm'!

!ChangeList class methodsFor: 'public access' stamp: 'HAW 9/6/2017 19:58:01'!
browseFrom: startPosition on: aChangesFileName labeled: aLabel
	
	"
	ChangeList browseFrom: Smalltalk lastQuitLogPosition on: Smalltalk currentChangesName labeled: 'Lost changes'
	"
	
	| changeList end |

	aChangesFileName asFileEntry readStreamDo: [ :changesFile |
		end _ changesFile size.
		changeList _ self new
			scanFile: changesFile
			from: startPosition 
			to: end.
	].

	ChangeListWindow open: changeList label: aLabel! !


!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/6/2017 19:59:40'!
restoreLostChangesManually

	ChangeList browseFrom: LastQuitLogPosition on: self currentChangesName labeled: 'Lost changes'
! !

