'From Cuis7.1 [latest update: #6776] on 15 October 2024 at 5:14:31 pm'!

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 10/15/2024 17:04:38'!
removeOlderMethodVersions
	"Remove older versions of entries from the receiver."

	| newChangeList newList found |
	newChangeList := OrderedCollection new.
	newList := OrderedCollection new.
	found := Set new.
	changeList reverseWith: list do: [ :chRec :strNstamp | | str |
		str := strNstamp copyUpTo: $;.
		((str beginsWith: 'method: ') and: [found includes: str]) ifFalse: [
			found add: str.
			newChangeList add: chRec.
			newList add: strNstamp]].
	newChangeList size < changeList size ifTrue: [
		changeList := newChangeList reversed.
		list := newList reversed.
		listIndex := 0.
		self clearSelections ].
	self changed: #list.! !

