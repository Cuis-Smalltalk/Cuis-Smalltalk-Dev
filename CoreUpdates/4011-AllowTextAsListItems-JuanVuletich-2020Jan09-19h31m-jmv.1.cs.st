'From Cuis 5.0 [latest update: #4010] on 9 January 2020 at 7:32:25 pm'!

!Browser methodsFor: 'message list' stamp: 'jmv 1/9/2020 12:07:48'!
messageListIndex: anInteger
	"Set the selected message selector to be the one indexed by anInteger."
	
	| index messageList |
	
	messageList _ self messageList.
	index _ messageList ifInBounds: anInteger ifNot: 0.

	selectedMessage _ index = 0 ifFalse: [ (messageList at: index) string ].
	self editSelection: (index > 0
		ifTrue: [#editMessage]
		ifFalse: [self messageCategoryListIndex > 0
			ifTrue: [#newMessage]
			ifFalse: [self classListIndex > 0
				ifTrue: [#editClass]
				ifFalse: [#newClass]]]).
	self changed: #messageListIndex. "update my selection"
	self acceptedContentsChanged! !


!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 1/9/2020 12:00:12'!
draw: item atRow: row on: canvas
	"display the given item at row row"
	| f c |
	(item is: #Text)
		ifTrue: [
			f _ font emphasized: (item emphasisAt: 1).
			c _ item colorAt: 1]
		ifFalse: [ 
			f _ font.
			c _  Theme current text].
	canvas
		drawString: item
		at: 0 @ (self drawYForRow: row)
		font: f
		color: c! !

