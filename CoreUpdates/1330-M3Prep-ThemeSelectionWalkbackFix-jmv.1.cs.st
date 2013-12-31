'From Cuis 4.0 of 3 April 2012 [latest update: #4211] on 12 April 2012 at 7:55:17 am'!

!Theme class methodsFor: 'user interface' stamp: 'jmv 4/12/2012 07:54'!
changeTheme

	| themes set menu result |
		themes _ Theme allSubclasses copyWith: Theme.
		set _ themes collect: [ :i | { i asString . i } ].
		menu _ SelectionMenu fromArray: set asArray.
		result _ menu startUpWithCaption: 'Choose a theme'.

	result ifNotNil: [
		result beCurrent ]! !

