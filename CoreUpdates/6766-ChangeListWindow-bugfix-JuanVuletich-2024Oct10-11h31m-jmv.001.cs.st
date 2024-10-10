'From Cuis7.1 [latest update: #6765] on 10 October 2024 at 11:31:17 am'!

!ChangeListWindow methodsFor: 'menu commands' stamp: 'jmv 10/10/2024 10:00:06'!
browseVersions

	| method class browser firstChange |
	method := model currentCompiledMethod.
	method isNil ifTrue: [
		model selectedClass ifNotNil: [ :selectedClass |
			^ VersionsBrowserWindow
				browseCommentOf: selectedClass ]].
	firstChange := model xtraVersion.
	firstChange ifNil: [ ^self ].
	class := method methodClass.
	browser := VersionsBrowserWindow
		browseVersionsOf: method
		class: class
		meta: class isMeta
		category: (class organization categoryOfElement: method selector)
		selector: method selector
		extraChangeRecord: firstChange.
	^browser! !

