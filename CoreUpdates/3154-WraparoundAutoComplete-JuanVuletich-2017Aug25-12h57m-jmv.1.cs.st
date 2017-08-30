'From Cuis 5.0 of 7 November 2016 [latest update: #3153] on 25 August 2017 at 1:01:32 pm'!

!AutoCompleterMorph methodsFor: 'actions' stamp: 'jmv 8/25/2017 13:00:40'!
goDown
	self selected = completer entryCount ifTrue: [
		"Wrap around"
		^ self goHome ].
	self selected: self selected + 1.
	(self selected > self lastVisible
			and: [self selected <= completer entryCount])
		ifTrue: [firstVisible := firstVisible + 1].
	self redrawNeeded! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'jmv 8/25/2017 13:00:48'!
goUp
	(self selected = 0
			and: [self firstVisible = 1])
		ifTrue: [^ self].
	self selected = 1 ifTrue: [
		"Wrap around"
		^self goToEnd ].
	self selected: self selected - 1.
	self selected < self firstVisible
		ifTrue: [firstVisible := firstVisible - 1].
	self redrawNeeded! !

