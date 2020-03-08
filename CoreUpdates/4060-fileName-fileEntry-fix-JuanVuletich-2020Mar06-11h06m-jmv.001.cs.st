'From Cuis 5.0 [latest update: #4057] on 6 March 2020 at 11:09:45 am'!

!ChangeList class methodsFor: 'public access' stamp: 'jmv 3/6/2020 11:08:35'!
browseFrom: startPosition on: aChangesFileEntry labeled: aLabel
	
	"
	ChangeList browseFrom: Smalltalk lastQuitLogPosition on: Smalltalk currentChangesName labeled: 'Lost changes'
	"
	
	| changeList end |

	aChangesFileEntry readStreamDo: [ :changesFile |
		end _ changesFile size.
		changeList _ self new
			scanFile: changesFile
			from: startPosition 
			to: end.
	].

	ChangeListWindow open: changeList label: aLabel! !

!ChangeList class methodsFor: 'public access' stamp: 'jmv 3/6/2020 11:07:19'!
browseRecent: charCount on: origChangesFileEntry 
	"Opens a changeList on the end of the specified changes log file"
	
	| changeList end |
	origChangesFileEntry readStreamDo: [ :changesFile |
		end _ changesFile size.
		changeList _ self new
			scanFile: changesFile
			from: (0 max: end - charCount)
			to: end.
	].
	ChangeListWindow open: changeList label: 'Recent changes'! !

!ChangeList class methodsFor: 'public access' stamp: 'jmv 3/6/2020 11:07:42'!
browseRecentLog
	"ChangeList browseRecentLog"
	"Prompt with a menu of how far back to go to browse the current image's changes log file"
	^ self
		browseRecentLogOn: Smalltalk currentChangesName asFileEntry
		startingFrom: Smalltalk lastQuitLogPosition! !

!ChangeList class methodsFor: 'public access' stamp: 'jmv 3/6/2020 11:07:26'!
browseRecentLogOn: origChangesFileEntry startingFrom: initialPos 
	"Prompt with a menu of how far back to go when browsing a changes file."

	| end banners positions pos chunk i |
	origChangesFileEntry readStreamDo: [ :changesFile |
		banners _ OrderedCollection new.
		positions _ OrderedCollection new.
		end _ changesFile size.
		pos _ initialPos.
		[pos = 0
			or: [banners size > 20]] 		"Go back at most 20 image exits"
			whileFalse: [
				changesFile position: pos.
				chunk _ changesFile nextChunk.
				i _ chunk indexOfSubCollection: 'priorSource: ' startingAt: 1.
				i > 0
					ifTrue: [
						positions addLast: pos.
						banners addLast: (chunk copyFrom: 5 to: i - 2).
						pos _ Number readFrom: (chunk copyFrom: i + 13 to: chunk size)]
					ifFalse: [
						pos _ 0]].
	].
	positions addLast: 0.
	banners addLast: 'Whole file'.
	pos _ (SelectionMenu labelList: banners selections: positions)
				startUpWithCaption: 'Browse as far back as...'.
	pos
		ifNil: [^ self].
	self browseRecent: end - pos on: origChangesFileEntry! !


!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'jmv 3/6/2020 11:08:54'!
restoreLostChangesManually

	ChangeList browseFrom: LastQuitLogPosition on: self currentChangesName asFileEntry labeled: 'Lost changes'
! !

