'From Cuis7.1 [latest update: #6452] on 18 June 2024 at 11:09:05 pm'!

!CodeWindow methodsFor: 'menu commands' stamp: 'sqr 6/18/2024 23:07:20'!
browseVersions
	"Create and schedule a Versions Browser, showing all versions of the 
	currently selected message. Answer the browser or nil."
	| method class |
	model selectedMessageName isNil ifTrue: [
		^ VersionsBrowserWindow
			browseCommentOf: model selectedClass ].
	method := model currentCompiledMethod.
	"because maybe the method has been removed --- unhelpful"
	method isNil ifTrue: [^self].
	class := method methodClass.
	^ VersionsBrowserWindow
		browseVersionsOf: method
		class: class
		meta: class isMeta
		category: (class organization categoryOfElement: method selector)
		selector: method selector! !

