'From Cuis7.5 [latest update: #7528] on 8 September 2025 at 11:26:02 am'!

!VersionsBrowserWindow class methodsFor: 'services' stamp: 'len 6/6/2025 16:30:54'!
browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector lostMethodPointer: sourcePointer extraChangeRecord: aChangeRecord
	| browser |
	browser := VersionsBrowser new.
	(browser scanVersionsOf: method class: class meta: meta category: msgCategory selector: selector)
		ifNil: [ self inform: 'No versions available'. ^nil ].
	aChangeRecord ifNotNil: [browser addedChangeRecord: aChangeRecord ].
	browser toggleListIndex: 1.
	VersionsBrowserWindow open: browser label: 'Recent versions of ', selector.
	^browser! !

