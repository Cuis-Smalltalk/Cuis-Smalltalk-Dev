'From Cuis7.9 [latest update: #8306] on 2 November 2026 at 12:02:51 pm'!

!ChangeListWindow methodsFor: 'menu commands' stamp: 'jmv 2/Nov/2026, 2:13:23 pm (UTC)'!
browseVersions

	| method class browser |
	method := model currentCompiledMethod.
	method isNil ifTrue: [
		^self.
		"model selectedClass ifNotNil: [ :selectedClass |
			^ VersionsBrowserWindow
				browseCommentOf: selectedClass ]"].
	class := method methodClass.
	browser := MethodVersionsBrowserWindow
		browseVersionsOf: method
		class: class
		meta: class isMeta
		category: (class organization categoryOfElement: method selector)
		selector: method selector.
	^browser! !

