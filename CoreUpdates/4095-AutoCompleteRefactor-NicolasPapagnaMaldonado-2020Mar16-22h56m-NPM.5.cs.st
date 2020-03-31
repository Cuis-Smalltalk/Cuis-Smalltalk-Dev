'From Cuis 5.0 [latest update: #4069] on 23 March 2020 at 4:50:04 pm'!

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'NPM 3/18/2020 19:50:30'!
selectorsOf: aClass beginningWith: aPrefix

	^ (AutoCompleterSelectorsCollector for: aPrefix) 
		addSelectorsOf: aClass;
		entriesToShow! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'NPM 3/18/2020 19:50:06'!
unaryAndBinarySelectorsOf: aClass beginningWith: aPrefix 

	^ (AutoCompleterSelectorsCollector for: aPrefix) 
		addUnaryAndBinarySelectorsOf: aClass;
		entriesToShow! !

!SmalltalkCompleter methodsFor: 'identifier entries - private' stamp: 'NPM 3/17/2020 17:53:08'!
computeIdentifierEntriesBeginningWith: aPrefix
	"Use an aux Set to avoid duplicates, but keep the order given."

	| entriesSet lastTitle candidates |
	entriesSet _ Set new.
	lastTitle _ nil.
	
	candidates _ Array streamContents: [ :strm |
		parser namesBeginningWith: aPrefix do: [ :identifier :kindOfIdentifierTitle |
			(entriesSet includes: identifier) ifFalse: [
				kindOfIdentifierTitle = lastTitle ifFalse: [
					strm nextPut: kindOfIdentifierTitle.
					lastTitle _ kindOfIdentifierTitle ].
				entriesSet  add: identifier.
				strm nextPut: identifier ]]].
	entriesSet size = 1 ifTrue: [
		^ Array with: entriesSet anyOne ]
		ifFalse: [ ^ candidates ]! !

!SmalltalkCompleter methodsFor: 'message entries for unknown class - private' stamp: 'NPM 3/16/2020 23:09:03'!
computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: aCollection

	| selectorsToShow |
	selectorsToShow _ OrderedCollection new.
	
	self class protected: [
		Selectors forPrefix: prefix keysAndValuesDo: [ :selector :lastUsedTime |
				selectorsToShow := self add: selector and: lastUsedTime to: selectorsToShow.
				(Object canUnderstand: selector) ifFalse: [ aCollection add: selector ]]].
				
	selectorsToShow size < EntriesLimit ifTrue: [ selectorsToShow _  self sortByLastUsedTime: selectorsToShow ].
	
	" To see the timestamps in the menu (need to tweak #insertCompletion: to activate. Right now, just for debugging)
	entries _ selectorsToShow collect: [ :ary | ary first, '(', ((DateAndTime fromString: '01/01/1996 00:00') + ary second minutes) printString,')' ]
	"
	^ selectorsToShow collect: [ :selectorAndTime | selectorAndTime first ]
	
! !

!SmalltalkCompleter methodsFor: 'accessing' stamp: 'NPM 3/17/2020 03:12:11'!
changePositionTo: newPosition 
	
	position _ newPosition! !

!SmalltalkCompleter methodsFor: 'accessing' stamp: 'NPM 3/17/2020 17:30:02'!
possibleInvalidSelectors
	
	^ possibleInvalidSelectors! !


!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'NPM 3/18/2020 19:50:06'!
computeMessageEntriesWithBinaryMessageForClass: aClass 

	selectorsClasses := Array with: aClass.
	
	entries := self unaryAndBinarySelectorsOf: aClass beginningWith: prefix.
		! !

!SmalltalkCompleter methodsFor: 'message entries - private' stamp: 'NPM 3/18/2020 19:51:12'!
computeMessageEntriesWithoutBinaryMessageForClass: aClass

	selectorsClasses := Array with: aClass.
	
	entries := self selectorsOf: aClass beginningWith: prefix.! !

!SmalltalkCompleter methodsFor: 'identifier entries - private' stamp: 'NPM 3/18/2020 20:44:01'!
computeIdentifierEntries
	
	canShowSelectorDocumentation _ false.
	entries _ self computeIdentifierEntriesBeginningWith: prefix.! !

!SmalltalkCompleter methodsFor: 'message entries for unknown class - private' stamp: 'NPM 3/18/2020 20:47:56'!
computeMessageEntriesForUnknowClass

	selectorsClasses _ #().
	possibleInvalidSelectors _ IdentitySet new.
	canShowSelectorDocumentation _ true.
	entries _ self computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: possibleInvalidSelectors.! !

