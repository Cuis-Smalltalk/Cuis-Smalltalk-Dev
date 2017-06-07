'From Cuis 5.0 of 7 November 2016 [latest update: #3092] on 5 June 2017 at 12:39:46 am'!

!TheWorldMenu methodsFor: 'commands' stamp: 'pb 6/5/2017 00:35:30'!
splitNewMorphList: list depth: d
	| middle c prev next out |
	d <= 0 ifTrue: [ ^ Array with: list ].
	middle := list size // 2 + 1.
	c := (list at: middle) name first: 3.
	prev := middle - 1.
	[
	prev > 0 and: [ ((list at: prev) name first: 3) = c ]] whileTrue: [ prev := prev - 1 ].
	next := middle + 1.
	[
	next <= list size and: [ ((list at: next) name first: 3) = c ]] whileTrue: [ next := next + 1 ].
	"Choose the better cluster"
	middle := middle - prev < (next - middle)
		ifTrue: [ prev + 1 ]
		ifFalse: [ next ].
	middle = 1 ifTrue: [ middle := next ].
	middle >= list size ifTrue: [ middle := prev + 1 ].
	(middle = 1 or: [ middle >= list size ]) ifTrue: [ ^ Array with: list ].
	out := WriteStream on: Array new.
	out nextPutAll:
		(self
			splitNewMorphList:
				(list
					copyFrom: 1
					to: middle - 1)
			depth: d - 1).
	out nextPutAll:
		(self
			splitNewMorphList:
				(list
					copyFrom: middle
					to: list size)
			depth: d - 1).
	^ out contents.! !

!TheWorldMenu methodsFor: 'construction' stamp: 'pb 6/5/2017 00:38:53'!
alphabeticalMorphMenu
	| list splitLists menu firstChar lastChar subMenu |
	list := Morph withAllSubclasses select: [ :m |
		m includeInNewMorphMenu ].
	list := list asArray sort: [ :c1 :c2 |
		c1 name < c2 name ].
	splitLists := self
		splitNewMorphList: list
		depth: 4.
	menu := MenuMorph new defaultTarget: self.
	1
		to: splitLists size
		do: [ :i |
			firstChar := i = 1
				ifTrue: [ $A ]
				ifFalse: [ 
					(splitLists at: i) first name first: 3 ].
			lastChar := i = splitLists size
				ifTrue: [ $Z ]
				ifFalse: [ 
					(splitLists at: i) last name first: 3 ].
			subMenu := MenuMorph new.
			(splitLists at: i) do: [ :cl |
				subMenu
					add: cl name
					target: self
					selector: #newMorphOfClass:event:
					argument: cl ].
			menu
				add: firstChar asString , ' - ' , lastChar asString
				subMenu: subMenu ].
	^ menu.! !

