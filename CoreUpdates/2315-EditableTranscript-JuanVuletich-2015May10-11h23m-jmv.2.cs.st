'From Cuis 4.2 of 25 July 2013 [latest update: #2314] on 10 May 2015 at 12:14:15.023216 pm'!
!classDefinition: #TranscriptMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #TranscriptMorph
	instanceVariableNames: 'form lastIncludedIndex lastUnfinishedEntrySize workspace workspaceWindow '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!TranscriptMorph methodsFor: 'updating' stamp: 'jmv 5/10/2015 12:12'!
redrawNeeded
	| newContents newLastIndex newLastUnfinishedEntrySize |
	workspaceWindow ifNotNil: [
		workspaceWindow owner notNil
			ifTrue: [
				newLastIndex _ Transcript lastIndex.
				newLastIndex = lastIncludedIndex ifFalse: [
					newContents _ Transcript contentsStartingAt: lastIncludedIndex+1.
					newLastUnfinishedEntrySize _ Transcript unfinishedEntrySize.
					 lastUnfinishedEntrySize  > 0 ifTrue: [
						newContents _ newContents copyFrom: lastUnfinishedEntrySize+1 to: newContents size.].
					workspace actualContents: workspace actualContents, newContents.
					lastUnfinishedEntrySize _ newLastUnfinishedEntrySize ].
					lastIncludedIndex _ newLastIndex. ]
			ifFalse: [ workspace _ lastIncludedIndex _ nil ]].
	super redrawNeeded! !


!Transcript class methodsFor: 'private' stamp: 'jmv 5/10/2015 12:06'!
contentsStartingAt: index
	^String streamContents: [ :strm |
		self entriesStartingAt: index do: [ :e |
			strm nextPutAll: e; newLine ].
		strm nextPutAll: unfinishedEntry contents ]! !

!Transcript class methodsFor: 'iteration' stamp: 'jmv 5/10/2015 11:39'!
entriesStartingAt: startIndex do: aBlock
	| f i |
	f _ startIndex-1.
	firstIndex > lastIndex ifTrue: [ f _ f - self maxEntries ].
	f < lastIndex ifTrue: [
		i _ f \\ self maxEntries + 1.
		[
			aBlock value: (entries at: i).
			i = lastIndex
		] whileFalse: [ i _ i \\ self maxEntries + 1 ] ]! !

!Transcript class methodsFor: 'accessing' stamp: 'jmv 5/10/2015 10:09'!
lastIndex
	^lastIndex! !

!Transcript class methodsFor: 'accessing' stamp: 'jmv 5/10/2015 11:59'!
unfinishedEntrySize
	^unfinishedEntry position! !


!TextModel methodsFor: 'initialization' stamp: 'jmv 5/10/2015 11:45'!
openLabel: aString 
	"Create a standard system view of the model, me, and open it."
	^SystemWindow editText: self label: aString wrap: true! !


!TextModel class methodsFor: 'instance creation' stamp: 'jmv 5/10/2015 11:45'!
openLabel: aString

	^self new openLabel: aString! !


!Workspace methodsFor: 'gui' stamp: 'jmv 5/10/2015 11:44'!
openLabel: aString 
	"Create a standard system view of the model, me, and open it."
	| win |
	win _ WorkspaceWindow editText: self label: aString wrap: true.
	self changed: #actualContents.
	^win! !


!TranscriptMorph methodsFor: 'menu commands' stamp: 'jmv 5/10/2015 12:12'!
editContents
	workspace _ Workspace new.
	workspace shouldStyle ifTrue: [ workspace toggleStyling ].
	workspace contents: Transcript contents.
	lastUnfinishedEntrySize _ Transcript unfinishedEntrySize.
	workspaceWindow _ workspace openLabel: 'Workspace - Transcript contents'.
	lastIncludedIndex _ Transcript lastIndex.! !


!Transcript class methodsFor: 'private' stamp: 'jmv 5/10/2015 11:32'!
contents
	^self contentsStartingAt: firstIndex! !

!methodRemoval: Transcript class #entriesDo:!
Transcript class removeSelector: #entriesDo:!
!classDefinition: #TranscriptMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #TranscriptMorph
	instanceVariableNames: 'form workspace lastIncludedIndex workspaceWindow lastUnfinishedEntrySize'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
