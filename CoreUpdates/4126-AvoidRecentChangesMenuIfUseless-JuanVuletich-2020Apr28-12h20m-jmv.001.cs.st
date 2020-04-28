'From Cuis 5.0 [latest update: #4125] on 28 April 2020 at 12:21:30 pm'!

!ChangeList class methodsFor: 'public access' stamp: 'jmv 4/28/2020 12:19:04'!
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
	positions isEmpty
		ifTrue: [ pos _ 0 ]
		ifFalse: [
			positions addLast: 0.
			banners addLast: 'Whole file'.
			pos _ (SelectionMenu labelList: banners selections: positions)
						startUpWithCaption: 'Browse as far back as...'.
			pos
				ifNil: [^ self]].
	self browseRecent: end - pos on: origChangesFileEntry! !

