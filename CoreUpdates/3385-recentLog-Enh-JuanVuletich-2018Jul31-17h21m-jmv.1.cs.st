'From Cuis 5.0 of 7 November 2016 [latest update: #3384] on 31 July 2018 at 5:43:12 pm'!

!ChangeList class methodsFor: 'public access' stamp: 'jmv 7/31/2018 17:40:24'!
browseRecentLogOn: origChangesFileName 
	"figure out where the last snapshot or quit was, then browse the recent entries."

	| end done block positions prevBlock |
	origChangesFileName asFileEntry readStreamDo: [ :changesFile |
		positions _ SortedCollection new.
		end _ changesFile size.
		prevBlock _ end.
		block _ end - 1024 max: 0.
		done _ false.
		[ done or: [ positions size > 0 ]]
			whileFalse: [
				changesFile position: block.
				"ignore first fragment"
				changesFile nextChunk.
				[ changesFile position < prevBlock ]
					whileTrue: [
						| pos chunk |
						pos _ changesFile position.
						chunk _ changesFile nextChunk.
						((Smalltalk isQuitRecord: chunk) or: [ Smalltalk isSnapshotRecord: chunk ])
							ifTrue: [ positions add: pos ]].
				block = 0
					ifTrue: [done _ true]
					ifFalse: [
						prevBlock _ block.
						block _ block - 1024 max: 0]].
	].
	self browseRecentLogOn: origChangesFileName startingFrom: (positions isEmpty ifTrue: [0] ifFalse: [positions last])! !

!ChangeList class methodsFor: 'public access' stamp: 'jmv 7/31/2018 17:42:47'!
browseRecentLogOn: origChangesFileName startingFrom: initialPos 
	"Prompt with a menu of how far back to go when browsing a changes file."

	| end banners positions pos chunk i |
	origChangesFileName asFileEntry readStreamDo: [ :changesFile |
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
	self browseRecent: end - pos on: origChangesFileName! !

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 7/31/2018 17:21:48'!
serviceRecentChanges
	"Answer a service for opening a changelist browser on the tail end of a .changes file"

	^ SimpleServiceEntry 
		provider: self 
		label: 'recent changes in file'
		selector: #browseRecentLogOn:
		description: 'open a changelist tool on recent changes in file'
		buttonLabel: 'recent changes'! !

!methodRemoval: ChangeList class #browseRecentLogOnPath:!
ChangeList class removeSelector: #browseRecentLogOnPath:!
