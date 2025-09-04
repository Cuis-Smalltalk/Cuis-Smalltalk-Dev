'From Cuis7.5 [latest update: #7474] on 1 September 2025 at 3:21:08 pm'!

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 9/1/2025 15:20:23'!
removeOlderMethodVersions
	"Remove older versions of entries from the receiver."

	| newChangeList newList found |
	newChangeList := OrderedCollection new.
	newList := OrderedCollection new.
	found := Set new.
	changeList reverseWith: list do: [ :chRec :strNstamp | | str |
		str := strNstamp copyUpTo: $-.
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

