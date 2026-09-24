'From Cuis7.9 [latest update: #8233] on 24 September 2026 at 5:05:10 pm'!

!VersionsBrowserWindow class methodsFor: 'services' stamp: 'jmv 22/Sep/2026, 7:01:16 pm (UTC)'!
browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector
	^ self browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector extraChange: nil! !

!VersionsBrowserWindow class methodsFor: 'services' stamp: 'jmv 22/Sep/2026, 7:00:57 pm (UTC)'!
browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector extraChange: aChange
	| browser |
	browser := VersionsBrowser new.
	(browser scanVersionsOf: method class: class meta: meta category: msgCategory selector: selector)
		ifNil: [ self inform: 'No versions available'. ^nil ].
	aChange ifNotNil: [browser extraChange: aChange ].
	browser toggleListIndex: 1.
	VersionsBrowserWindow open: browser label: 'Recent versions of ', selector.
	^browser! !


!ChangeSorterWindow methodsFor: 'menu commands' stamp: 'jmv 24/Sep/2026, 8:03:59 pm (UTC)'!
browseVersions
	"Create and schedule a changelist browser on the versions of the 
	selected message."
	| class selector method category pair |

	(selector := model selectedMessageName) ifNil: [^ self].
	class := model selectedClassOrMetaClass.
	(class includesSelector: selector)
		ifTrue: [
			method := class compiledMethodAt: selector.
			category := class whichCategoryIncludesSelector: selector ]
		ifFalse: [
			pair := model methodInfoFromRemoval: {class name. selector}.
			pair ifNil: [^ nil].
			method := CompiledMethod toReturnSelf setSourcePointer: pair first.
			category := pair last ].
	VersionsBrowserWindow
		browseVersionsOf: method
		class: model selectedClass meta: class isMeta
		category: category selector: selector.! !

!methodRemoval: VersionsBrowserWindow class #browseVersionsOf:class:meta:category:selector:lostMethodPointer: stamp: 'jmv 24/Sep/2026, 8:02:35 pm (UTC)'!
VersionsBrowserWindow class removeSelector: #browseVersionsOf:class:meta:category:selector:lostMethodPointer:!
!methodRemoval: VersionsBrowserWindow class #browseVersionsOf:class:meta:category:selector:lostMethodPointer:extraChange: stamp: 'jmv 24/Sep/2026, 8:01:59 pm (UTC)'!
VersionsBrowserWindow class removeSelector: #browseVersionsOf:class:meta:category:selector:lostMethodPointer:extraChange:!
