'From Cuis 5.0 of 7 November 2016 [latest update: #3622] on 21 February 2019 at 10:08:10 pm'!

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 2/21/2019 22:00:30'!
ifEmptyEntriesShowAllPrefixedSelectors

	entries isEmpty ifTrue: [ self computeMessageEntriesForUnknowClass ] ! !


!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'HAW 2/21/2019 21:59:30'!
computeMessageEntriesForClass: aClass

	selectorsClasses := Array with: aClass.
	
	entries := (AutoCompleterSelectorsCollector for: prefix) 
		addSelectorsOf: aClass;
		entriesToShow.
		
	self ifEmptyEntriesShowAllPrefixedSelectors! !

