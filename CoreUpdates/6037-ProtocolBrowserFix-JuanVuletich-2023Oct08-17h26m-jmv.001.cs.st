'From Cuis 6.0 [latest update: #6036] on 8 October 2023 at 5:27:21 pm'!

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 10/8/2023 17:27:01'!
browseVersions
	"Create and schedule a Versions Browser, showing all versions of the 
	currently selected message. Answer the browser or nil."
	| method class |
	method := model currentCompiledMethod.
	method isNil ifTrue: [
		^ VersionsBrowserWindow
			browseCommentOf: model selectedClass ].
	class := method methodClass.
	^ VersionsBrowserWindow
		browseVersionsOf: method
		class: class
		meta: class isMeta
		category: (class organization categoryOfElement: method selector)
		selector: method selector! !

