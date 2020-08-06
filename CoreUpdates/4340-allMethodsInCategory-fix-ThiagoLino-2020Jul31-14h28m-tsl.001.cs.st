'From Cuis 5.0 [latest update: #4256] on 31 July 2020 at 2:29:03 pm'!

!SystemDictionary methodsFor: 'retrieving' stamp: 'tsl 7/31/2020 14:28:14'!
allMethodsInCategory: category
	| aCollection |
	aCollection := SortedCollection new.
	self allBehaviorsDo: [ :x |
		(x organization listAtCategoryNamed: category) do: [ :sel |
			aCollection add: (MethodReference class: x selector: sel)]].
	^aCollection! !

