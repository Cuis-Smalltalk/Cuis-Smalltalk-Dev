'From Cuis7.9 [latest update: #8235] on 24 September 2026 at 5:09:53 pm'!

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 22/Sep/2026, 6:56:16 pm (UTC)'!
methodRemoved: aCompiledMethod selector: aSymbol inProtocol: aCategoryName class: aClass

	| packageOrNil |
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	packageOrNil := CodePackage packageOfMethodCategory: aCategoryName ofClass: aClass ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			noteRemoveSelector: aSymbol
			class: aClass
			lastMethodInfo: {aCompiledMethod historyPointer. aCategoryName} ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ].! !


!ChangeSorterWindow methodsFor: 'menu commands' stamp: 'jmv 22/Sep/2026, 7:02:36 pm (UTC)'!
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
			method := CompiledMethod toReturnSelf historyPointer: pair first.
			category := pair last ].
	VersionsBrowserWindow
		browseVersionsOf: method
		class: model selectedClass meta: class isMeta
		category: category selector: selector.! !

!methodRemoval: RemoteString #setSourcePointer: stamp: 'jmv 24/Sep/2026, 8:08:54 pm (UTC)'!
RemoteString removeSelector: #setSourcePointer:!
!methodRemoval: CompiledMethod #setSourcePointer: stamp: 'jmv 24/Sep/2026, 8:08:55 pm (UTC)'!
CompiledMethod removeSelector: #setSourcePointer:!
