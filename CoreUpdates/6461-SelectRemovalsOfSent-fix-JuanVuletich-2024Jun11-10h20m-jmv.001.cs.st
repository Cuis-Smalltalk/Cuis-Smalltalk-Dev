'From Cuis7.1 [latest update: #6461] on 11 June 2024 at 10:21:19 am'!

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 6/11/2024 10:06:58'!
selectRemovalsOfSent
	"Selects all method removal for sent methods"

	1 to: changeList size do: [ :i | | change isRemovalOfSent |
		change := changeList at: i.
		isRemovalOfSent := change isMethodDeletion and: [
			Smalltalk isThereAReferenceTo: change methodSelector ].
		isRemovalOfSent := isRemovalOfSent or:
			(change isDoIt and: [
				change string includesSubString: 'removeSelector: #' ] and: [
					Smalltalk isThereAReferenceTo: (change string copyAfterLast: $#) asSymbol ]).
		listSelections at: i put: isRemovalOfSent ].
	self changed: #allSelections.
	self changed: #annotation! !

