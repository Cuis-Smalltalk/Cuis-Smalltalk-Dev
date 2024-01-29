'From Cuis6.3 [latest update: #6163] on 4 January 2024 at 1:05:03 pm'!


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 1/4/2024 13:04:10'!
browseVersions
	"Create and schedule a Versions Browser, showing all versions of the 
	currently selected message. Answer the browser or nil."
	| method class |
	model selectedMessageName isNil ifTrue: [
		^ VersionsBrowserWindow
			browseCommentOf: model selectedClass ].
	method := model currentCompiledMethod.
	class := method methodClass.
	^ VersionsBrowserWindow
		browseVersionsOf: method
		class: class
		meta: class isMeta
		category: (class organization categoryOfElement: method selector)
		selector: method selector! !

