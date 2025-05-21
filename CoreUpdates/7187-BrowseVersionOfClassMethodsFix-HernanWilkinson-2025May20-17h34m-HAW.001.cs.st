'From Cuis7.3 [latest update: #7182] on 20 May 2025 at 5:42:29 pm'!

!CodeWindow methodsFor: 'menu commands' stamp: 'HAW 5/20/2025 17:40:50'!
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
		class: class theNonMetaClass 
		meta: class isMeta
		category: (class organization categoryOfElement: method selector)
		selector: method selector! !

